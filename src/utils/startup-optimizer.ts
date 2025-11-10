/**
 * Startup Optimizer
 * Initializes all performance optimizations at application startup
 */

import { getGlobalCache } from './performance-cache.js'
import { getGlobalPreloader } from './example-preloader.js'
import { getGlobalCostTracker } from '../llm/cost-tracker.js'

export interface StartupOptions {
  preloadExamples?: boolean
  enableCache?: boolean
  verbose?: boolean
}

/**
 * Initialize all performance optimizations
 */
export async function initializePerformanceOptimizations (
  options: StartupOptions = {}
): Promise<void> {
  const {
    preloadExamples = true,
    enableCache = true,
    verbose = false
  } = options

  const startTime = Date.now()

  if (verbose) {
    console.log('🚀 Initializing COBRA performance optimizations...')
  }

  // Initialize cache
  if (enableCache) {
    const cache = getGlobalCache()
    if (verbose) {
      console.log('  ✓ Performance cache initialized')
    }
  }

  // Initialize cost tracker
  const costTracker = getGlobalCostTracker()
  if (verbose) {
    console.log('  ✓ Cost tracker initialized')
  }

  // Preload example files
  if (preloadExamples) {
    try {
      const preloader = getGlobalPreloader()
      await preloader.preloadExamples()

      if (verbose) {
        const examples = preloader.getAllExamples()
        console.log(`  ✓ Preloaded ${examples.length} example files`)
      }
    } catch (error) {
      if (verbose) {
        console.warn('  ⚠️  Failed to preload examples:', error)
      }
    }
  }

  const elapsed = Date.now() - startTime

  if (verbose) {
    console.log(`✓ Performance optimizations ready in ${elapsed}ms\n`)
  }
}

/**
 * Get performance summary
 */
export function getPerformanceSummary (): string {
  const cache = getGlobalCache()
  const costTracker = getGlobalCostTracker()
  const preloader = getGlobalPreloader()

  const cacheStats = cache.getStats()
  const costSummary = costTracker.getSummary()
  const examples = preloader.getAllExamples()

  return `
🎯 COBRA Performance Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Cache Performance:
  AST Cache: ${cacheStats.astCache.size} entries, ${(
    cacheStats.astCache.hitRate * 100
  ).toFixed(1)}% hit rate
  Analysis Cache: ${cacheStats.analysisCache.size} entries, ${(
    cacheStats.analysisCache.hitRate * 100
  ).toFixed(1)}% hit rate
  LLM Cache: ${cacheStats.llmCache.size} entries, ${(
    cacheStats.llmCache.hitRate * 100
  ).toFixed(1)}% hit rate
  Memory Usage: ${(cacheStats.totalMemoryUsage / 1024 / 1024).toFixed(2)} MB

Cost Tracking:
  Total Requests: ${costSummary.totalRequests}
  Total Cost: $${costSummary.totalCost.toFixed(4)}
  Estimated Savings: $${cacheStats.llmCache.estimatedCostSavings.toFixed(2)}

Preloaded Examples: ${examples.length}

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  `.trim()
}

/**
 * Cleanup performance optimizations
 */
export async function cleanupPerformanceOptimizations (): Promise<void> {
  const cache = getGlobalCache()
  const preloader = getGlobalPreloader()

  // Cleanup expired cache entries
  const cleanup = cache.cleanup()
  console.log(`🧹 Cleaned up ${cleanup.removed} expired cache entries`)

  // Clear preloaded examples
  preloader.clear()

  console.log('✓ Performance optimizations cleaned up')
}

/**
 * Schedule periodic cache cleanup
 */
export function schedulePeriodicCleanup (
  intervalMinutes: number = 30
): NodeJS.Timeout {
  const intervalMs = intervalMinutes * 60 * 1000

  return setInterval(() => {
    const cache = getGlobalCache()
    const cleanup = cache.cleanup()

    if (cleanup.removed > 0) {
      console.log(
        `🧹 Periodic cleanup: removed ${
          cleanup.removed
        } expired entries, freed ${(cleanup.freedMemory / 1024 / 1024).toFixed(
          2
        )} MB`
      )
    }
  }, intervalMs)
}
