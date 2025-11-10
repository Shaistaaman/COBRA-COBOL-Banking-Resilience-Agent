/**
 * Performance Cache Manager
 * Provides in-memory caching for parsed ASTs, analysis results, and LLM responses
 */

import crypto from 'crypto'
import type { AST, ParseResult, LogicAnalysis } from '../mcp-server/types.js'
import type { ExplanationResponse } from '../llm/types.js'

export interface CacheStats {
  astCache: {
    size: number
    hits: number
    misses: number
    hitRate: number
  }
  analysisCache: {
    size: number
    hits: number
    misses: number
    hitRate: number
  }
  llmCache: {
    size: number
    hits: number
    misses: number
    hitRate: number
    estimatedCostSavings: number
  }
  totalMemoryUsage: number
}

interface CacheEntry<T> {
  key: string
  value: T
  timestamp: number
  expiresAt: number
  size: number
}

/**
 * Unified performance cache for all COBRA components
 */
export class PerformanceCache {
  private astCache: Map<string, CacheEntry<ParseResult>>
  private analysisCache: Map<string, CacheEntry<LogicAnalysis>>
  private llmCache: Map<string, CacheEntry<ExplanationResponse>>

  private astHits = 0
  private astMisses = 0
  private analysisHits = 0
  private analysisMisses = 0
  private llmHits = 0
  private llmMisses = 0

  private readonly maxASTEntries: number
  private readonly maxAnalysisEntries: number
  private readonly maxLLMEntries: number
  private readonly ttlSeconds: number

  // Cost tracking (approximate)
  private readonly llmCostPerRequest = 0.12 // Average cost per LLM request

  constructor (
    options: {
      maxASTEntries?: number
      maxAnalysisEntries?: number
      maxLLMEntries?: number
      ttlSeconds?: number
    } = {}
  ) {
    this.astCache = new Map()
    this.analysisCache = new Map()
    this.llmCache = new Map()

    this.maxASTEntries = options.maxASTEntries ?? 50
    this.maxAnalysisEntries = options.maxAnalysisEntries ?? 50
    this.maxLLMEntries = options.maxLLMEntries ?? 100
    this.ttlSeconds = options.ttlSeconds ?? 3600 // 1 hour default
  }

  /**
   * Generate cache key from content
   */
  private generateKey (content: string): string {
    return crypto.createHash('sha256').update(content).digest('hex')
  }

  /**
   * Estimate memory size of object
   */
  private estimateSize (obj: any): number {
    const str = JSON.stringify(obj)
    return Buffer.byteLength(str, 'utf8')
  }

  /**
   * Cache parsed AST result
   */
  cacheAST (source: string, result: ParseResult): void {
    const key = this.generateKey(source)
    const now = Date.now()

    // Evict oldest if cache is full
    if (this.astCache.size >= this.maxASTEntries) {
      const oldestKey = this.astCache.keys().next().value
      if (oldestKey) {
        this.astCache.delete(oldestKey)
      }
    }

    this.astCache.set(key, {
      key,
      value: result,
      timestamp: now,
      expiresAt: now + this.ttlSeconds * 1000,
      size: this.estimateSize(result)
    })
  }

  /**
   * Get cached AST result
   */
  getAST (source: string): ParseResult | null {
    const key = this.generateKey(source)
    const entry = this.astCache.get(key)

    if (!entry) {
      this.astMisses++
      return null
    }

    // Check expiration
    if (Date.now() > entry.expiresAt) {
      this.astCache.delete(key)
      this.astMisses++
      return null
    }

    this.astHits++
    return entry.value
  }

  /**
   * Cache analysis result
   */
  cacheAnalysis (astKey: string, analysis: LogicAnalysis): void {
    const key = this.generateKey(astKey)
    const now = Date.now()

    // Evict oldest if cache is full
    if (this.analysisCache.size >= this.maxAnalysisEntries) {
      const oldestKey = this.analysisCache.keys().next().value
      if (oldestKey) {
        this.analysisCache.delete(oldestKey)
      }
    }

    this.analysisCache.set(key, {
      key,
      value: analysis,
      timestamp: now,
      expiresAt: now + this.ttlSeconds * 1000,
      size: this.estimateSize(analysis)
    })
  }

  /**
   * Get cached analysis result
   */
  getAnalysis (astKey: string): LogicAnalysis | null {
    const key = this.generateKey(astKey)
    const entry = this.analysisCache.get(key)

    if (!entry) {
      this.analysisMisses++
      return null
    }

    // Check expiration
    if (Date.now() > entry.expiresAt) {
      this.analysisCache.delete(key)
      this.analysisMisses++
      return null
    }

    this.analysisHits++
    return entry.value
  }

  /**
   * Cache LLM response
   */
  cacheLLMResponse (prompt: string, response: ExplanationResponse): void {
    const key = this.generateKey(prompt)
    const now = Date.now()

    // Evict oldest if cache is full
    if (this.llmCache.size >= this.maxLLMEntries) {
      const oldestKey = this.llmCache.keys().next().value
      if (oldestKey) {
        this.llmCache.delete(oldestKey)
      }
    }

    this.llmCache.set(key, {
      key,
      value: response,
      timestamp: now,
      expiresAt: now + this.ttlSeconds * 1000,
      size: this.estimateSize(response)
    })
  }

  /**
   * Get cached LLM response
   */
  getLLMResponse (prompt: string): ExplanationResponse | null {
    const key = this.generateKey(prompt)
    const entry = this.llmCache.get(key)

    if (!entry) {
      this.llmMisses++
      return null
    }

    // Check expiration
    if (Date.now() > entry.expiresAt) {
      this.llmCache.delete(key)
      this.llmMisses++
      return null
    }

    this.llmHits++
    return entry.value
  }

  /**
   * Get comprehensive cache statistics
   */
  getStats (): CacheStats {
    const astTotal = this.astHits + this.astMisses
    const analysisTotal = this.analysisHits + this.analysisMisses
    const llmTotal = this.llmHits + this.llmMisses

    // Calculate memory usage
    let totalMemory = 0
    for (const entry of this.astCache.values()) {
      totalMemory += entry.size
    }
    for (const entry of this.analysisCache.values()) {
      totalMemory += entry.size
    }
    for (const entry of this.llmCache.values()) {
      totalMemory += entry.size
    }

    return {
      astCache: {
        size: this.astCache.size,
        hits: this.astHits,
        misses: this.astMisses,
        hitRate: astTotal > 0 ? this.astHits / astTotal : 0
      },
      analysisCache: {
        size: this.analysisCache.size,
        hits: this.analysisHits,
        misses: this.analysisMisses,
        hitRate: analysisTotal > 0 ? this.analysisHits / analysisTotal : 0
      },
      llmCache: {
        size: this.llmCache.size,
        hits: this.llmHits,
        misses: this.llmMisses,
        hitRate: llmTotal > 0 ? this.llmHits / llmTotal : 0,
        estimatedCostSavings: this.llmHits * this.llmCostPerRequest
      },
      totalMemoryUsage: totalMemory
    }
  }

  /**
   * Clear all caches
   */
  clear (): void {
    this.astCache.clear()
    this.analysisCache.clear()
    this.llmCache.clear()
    this.astHits = 0
    this.astMisses = 0
    this.analysisHits = 0
    this.analysisMisses = 0
    this.llmHits = 0
    this.llmMisses = 0
  }

  /**
   * Cleanup expired entries
   */
  cleanup (): { removed: number; freedMemory: number } {
    const now = Date.now()
    let removed = 0
    let freedMemory = 0

    // Cleanup AST cache
    for (const [key, entry] of this.astCache.entries()) {
      if (now > entry.expiresAt) {
        freedMemory += entry.size
        this.astCache.delete(key)
        removed++
      }
    }

    // Cleanup analysis cache
    for (const [key, entry] of this.analysisCache.entries()) {
      if (now > entry.expiresAt) {
        freedMemory += entry.size
        this.analysisCache.delete(key)
        removed++
      }
    }

    // Cleanup LLM cache
    for (const [key, entry] of this.llmCache.entries()) {
      if (now > entry.expiresAt) {
        freedMemory += entry.size
        this.llmCache.delete(key)
        removed++
      }
    }

    return { removed, freedMemory }
  }

  /**
   * Get formatted statistics for display
   */
  getFormattedStats (): string {
    const stats = this.getStats()
    const memoryMB = (stats.totalMemoryUsage / 1024 / 1024).toFixed(2)

    return `
📊 Performance Cache Statistics
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

AST Cache:
  Entries: ${stats.astCache.size}/${this.maxASTEntries}
  Hits: ${stats.astCache.hits} | Misses: ${stats.astCache.misses}
  Hit Rate: ${(stats.astCache.hitRate * 100).toFixed(1)}%

Analysis Cache:
  Entries: ${stats.analysisCache.size}/${this.maxAnalysisEntries}
  Hits: ${stats.analysisCache.hits} | Misses: ${stats.analysisCache.misses}
  Hit Rate: ${(stats.analysisCache.hitRate * 100).toFixed(1)}%

LLM Cache:
  Entries: ${stats.llmCache.size}/${this.maxLLMEntries}
  Hits: ${stats.llmCache.hits} | Misses: ${stats.llmCache.misses}
  Hit Rate: ${(stats.llmCache.hitRate * 100).toFixed(1)}%
  💰 Estimated Cost Savings: $${stats.llmCache.estimatedCostSavings.toFixed(2)}

Memory Usage: ${memoryMB} MB
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    `.trim()
  }
}

/**
 * Global singleton cache instance
 */
let globalCache: PerformanceCache | null = null

/**
 * Get or create global cache instance
 */
export function getGlobalCache (): PerformanceCache {
  if (!globalCache) {
    globalCache = new PerformanceCache()
  }
  return globalCache
}

/**
 * Reset global cache (useful for testing)
 */
export function resetGlobalCache (): void {
  globalCache = null
}
