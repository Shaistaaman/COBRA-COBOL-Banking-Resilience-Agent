/**
 * LLM Response Cache Manager
 * Caches analysis results to minimize API costs
 */

import crypto from 'crypto'
import type { CacheEntry, ExplanationResponse } from './types.js'

export interface CacheOptions {
  enabled?: boolean
  ttlSeconds?: number
  maxEntries?: number
}

/**
 * In-memory cache for LLM responses
 */
export class LLMCache {
  private cache: Map<string, CacheEntry>
  private enabled: boolean
  private ttlSeconds: number
  private maxEntries: number

  constructor (options: CacheOptions = {}) {
    this.cache = new Map()
    this.enabled = options.enabled ?? true
    this.ttlSeconds = options.ttlSeconds ?? 3600 // 1 hour default
    this.maxEntries = options.maxEntries ?? 100
  }

  /**
   * Generate cache key from AST and analysis
   */
  private generateKey (ast: any, analysis: any): string {
    const content = JSON.stringify({ ast, analysis })
    return crypto.createHash('sha256').update(content).digest('hex')
  }

  /**
   * Get cached response if available and not expired
   */
  get (ast: any, analysis: any): ExplanationResponse | null {
    if (!this.enabled) {
      return null
    }

    const key = this.generateKey(ast, analysis)
    const entry = this.cache.get(key)

    if (!entry) {
      return null
    }

    // Check if expired
    if (Date.now() > entry.expiresAt) {
      this.cache.delete(key)
      return null
    }

    return entry.response
  }

  /**
   * Store response in cache
   */
  set (ast: any, analysis: any, response: ExplanationResponse): void {
    if (!this.enabled) {
      return
    }

    // Evict oldest entries if cache is full
    if (this.cache.size >= this.maxEntries) {
      const oldestKey = this.cache.keys().next().value
      if (oldestKey) {
        this.cache.delete(oldestKey)
      }
    }

    const key = this.generateKey(ast, analysis)
    const now = Date.now()

    this.cache.set(key, {
      key,
      response,
      timestamp: now,
      expiresAt: now + this.ttlSeconds * 1000
    })
  }

  /**
   * Clear all cached entries
   */
  clear (): void {
    this.cache.clear()
  }

  /**
   * Get cache statistics
   */
  getStats (): { size: number; enabled: boolean; ttlSeconds: number } {
    return {
      size: this.cache.size,
      enabled: this.enabled,
      ttlSeconds: this.ttlSeconds
    }
  }

  /**
   * Remove expired entries
   */
  cleanup (): number {
    const now = Date.now()
    let removed = 0

    for (const [key, entry] of this.cache.entries()) {
      if (now > entry.expiresAt) {
        this.cache.delete(key)
        removed++
      }
    }

    return removed
  }
}

/**
 * Factory function to create cache instance
 */
export function createCache (options?: CacheOptions): LLMCache {
  return new LLMCache(options)
}
