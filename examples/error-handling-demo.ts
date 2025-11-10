/**
 * Error Handling Demo
 * Demonstrates COBRA's comprehensive error handling and user feedback
 */

import {
  createError,
  ErrorCode,
  formatErrorForUser,
  formatErrorForLog
} from '../src/utils/error-handler.js'
import {
  ProgressTracker,
  createConsoleProgressReporter
} from '../src/utils/progress-tracker.js'

console.log('='.repeat(80))
console.log('COBRA Error Handling Demo')
console.log('='.repeat(80))
console.log()

// Demo 1: Parser Error
console.log('Demo 1: Parser Error with Suggestions')
console.log('-'.repeat(80))
const parseError = createError(
  ErrorCode.PARSE_INVALID_SYNTAX,
  'Missing period at end of statement on line 42'
)
console.log(formatErrorForUser(parseError))
console.log()

// Demo 2: LLM Rate Limit Error
console.log('Demo 2: LLM Rate Limit Error')
console.log('-'.repeat(80))
const rateLimitError = createError(
  ErrorCode.LLM_RATE_LIMIT,
  'OpenAI API rate limit exceeded'
)
console.log(formatErrorForUser(rateLimitError))
console.log()

// Demo 3: Missing Division Warning
console.log('Demo 3: Missing Division Warning')
console.log('-'.repeat(80))
const missingDivisionError = createError(
  ErrorCode.PARSE_MISSING_DIVISION,
  'Missing IDENTIFICATION DIVISION'
)
console.log(formatErrorForUser(missingDivisionError))
console.log()

// Demo 4: Progress Tracking
console.log('Demo 4: Progress Tracking')
console.log('-'.repeat(80))

async function simulateAnalysis () {
  const tracker = new ProgressTracker(
    {
      parsing: 20,
      analyzing: 30,
      explaining: 20,
      generating: 30
    },
    createConsoleProgressReporter()
  )

  // Simulate parsing
  tracker.startStage('parsing', 'Parsing COBOL source code...')
  await sleep(500)
  tracker.updateProgress('parsing', 50, 'Validating syntax...')
  await sleep(500)
  tracker.completeStage('parsing', 'Parsing complete ✓')

  // Simulate analyzing
  tracker.startStage('analyzing', 'Analyzing business logic...')
  await sleep(500)
  tracker.updateProgress('analyzing', 50, 'Found 3 banking patterns')
  await sleep(500)
  tracker.completeStage('analyzing', 'Analysis complete ✓')

  // Simulate explaining
  tracker.startStage('explaining', 'Generating explanation...')
  await sleep(500)
  tracker.updateProgress('explaining', 50, 'Processing with LLM...')
  await sleep(500)
  tracker.completeStage('explaining', 'Explanation generated ✓')

  // Simulate generating
  tracker.startStage('generating', 'Generating AWS code...')
  await sleep(500)
  tracker.updateProgress('generating', 50, 'Creating Lambda functions...')
  await sleep(500)
  tracker.completeStage(
    'generating',
    `Complete in ${tracker.getFormattedElapsedTime()} ✓`
  )
}

function sleep (ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}

// Run the demo
simulateAnalysis()
  .then(() => {
    console.log()
    console.log('='.repeat(80))
    console.log('Demo Complete!')
    console.log('='.repeat(80))
  })
  .catch(error => {
    console.error('Demo failed:', error)
  })
