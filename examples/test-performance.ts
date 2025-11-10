/**
 * Performance Optimization Test
 * Demonstrates caching, preloading, and cost tracking
 */

import { createOrchestrator } from '../src/orchestrator.js'
import {
  getGlobalCache,
  getGlobalPreloader,
  getPerformanceSummary
} from '../src/utils/index.js'
import { getGlobalCostTracker } from '../src/llm/cost-tracker.js'
import { initializePerformanceOptimizations } from '../src/utils/startup-optimizer.js'

// Sample COBOL code
const sampleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCOUNT-BALANCE     PIC 9(13)V99.
       01  INTEREST-RATE       PIC 9V9999.
       01  INTEREST-AMOUNT     PIC 9(13)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           COMPUTE INTEREST-AMOUNT = ACCOUNT-BALANCE * INTEREST-RATE.
           STOP RUN.
`

async function testPerformanceOptimizations () {
  console.log('🧪 Testing COBRA Performance Optimizations\n')

  // Initialize optimizations
  console.log('1️⃣  Initializing performance optimizations...')
  await initializePerformanceOptimizations({
    preloadExamples: true,
    enableCache: true,
    verbose: true
  })

  const orchestrator = createOrchestrator({
    verbose: true,
    enableCaching: true
  })

  // Test 1: First parse (uncached)
  console.log('\n2️⃣  Test 1: First parse (uncached)')
  const start1 = Date.now()
  const result1 = await orchestrator.analyzeQuick(sampleCobol)
  const time1 = Date.now() - start1
  console.log(`   ✓ Completed in ${time1}ms`)

  // Test 2: Second parse (should be cached)
  console.log('\n3️⃣  Test 2: Second parse (should be cached)')
  const start2 = Date.now()
  const result2 = await orchestrator.analyzeQuick(sampleCobol)
  const time2 = Date.now() - start2
  console.log(`   ✓ Completed in ${time2}ms`)
  console.log(`   🚀 Speedup: ${(time1 / time2).toFixed(1)}x faster`)

  // Test 3: Preloaded example
  console.log('\n4️⃣  Test 3: Preloaded example access')
  const preloader = getGlobalPreloader()
  const examples = preloader.getAllExamples()
  if (examples.length > 0) {
    const start3 = Date.now()
    const example = preloader.getExample(examples[0].name)
    const time3 = Date.now() - start3
    console.log(`   ✓ Retrieved "${examples[0].name}" in ${time3}ms`)
    console.log(`   ⚡ Instant access to preloaded example`)
  } else {
    console.log('   ⚠️  No examples preloaded')
  }

  // Test 4: Cache statistics
  console.log('\n5️⃣  Test 4: Cache statistics')
  const cache = getGlobalCache()
  const stats = cache.getStats()
  console.log(
    `   AST Cache: ${stats.astCache.size} entries, ${(
      stats.astCache.hitRate * 100
    ).toFixed(1)}% hit rate`
  )
  console.log(
    `   Analysis Cache: ${stats.analysisCache.size} entries, ${(
      stats.analysisCache.hitRate * 100
    ).toFixed(1)}% hit rate`
  )
  console.log(
    `   Memory Usage: ${(stats.totalMemoryUsage / 1024 / 1024).toFixed(2)} MB`
  )

  // Test 5: Cost tracking
  console.log('\n6️⃣  Test 5: Cost tracking')
  const costTracker = getGlobalCostTracker()
  const costSummary = costTracker.getSummary()
  console.log(`   Total Requests: ${costSummary.totalRequests}`)
  console.log(`   Total Cost: $${costSummary.totalCost.toFixed(4)}`)
  if (stats.llmCache.estimatedCostSavings > 0) {
    console.log(
      `   💰 Estimated Savings: $${stats.llmCache.estimatedCostSavings.toFixed(
        2
      )}`
    )
  }

  // Performance summary
  console.log('\n7️⃣  Performance Summary')
  console.log(getPerformanceSummary())

  // Verify performance targets
  console.log('\n8️⃣  Performance Target Verification')
  const lineCount = sampleCobol.split('\n').length
  const targetTime = (lineCount / 10000) * 30000 // 30s for 10k lines
  console.log(`   Parse time: ${time1}ms (target: ${targetTime.toFixed(0)}ms)`)
  console.log(`   Status: ${time1 < targetTime ? '✅ PASS' : '❌ FAIL'}`)
  console.log(`   Cache speedup: ${(time1 / time2).toFixed(1)}x`)
  console.log(
    `   Status: ${time2 < 100 ? '✅ PASS (sub-100ms cached)' : '⚠️  SLOW'}`
  )

  console.log('\n✅ Performance optimization tests complete!\n')
}

// Run tests
testPerformanceOptimizations().catch(error => {
  console.error('❌ Test failed:', error)
  process.exit(1)
})
