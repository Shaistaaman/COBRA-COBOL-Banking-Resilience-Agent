#!/usr/bin/env node
/**
 * Integration Test Script
 * Tests end-to-end integration of all COBRA components
 */

import { readFileSync } from 'fs'
import { join } from 'path'
import { createOrchestrator } from './orchestrator.js'

// ANSI color codes for terminal output
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
}

function log (message: string, color: keyof typeof colors = 'reset') {
  console.log(`${colors[color]}${message}${colors.reset}`)
}

function logSection (title: string) {
  console.log('\n' + '='.repeat(60))
  log(title, 'cyan')
  console.log('='.repeat(60))
}

function logSuccess (message: string) {
  log(`✓ ${message}`, 'green')
}

function logError (message: string) {
  log(`✗ ${message}`, 'red')
}

function logWarning (message: string) {
  log(`⚠ ${message}`, 'yellow')
}

function logInfo (message: string) {
  log(`ℹ ${message}`, 'blue')
}

/**
 * Test 1: Parse COBOL source
 */
async function testParsing (orchestrator: any, cobolSource: string) {
  logSection('Test 1: COBOL Parsing')

  try {
    const parseResult = await orchestrator.parse(cobolSource)

    if (parseResult.errors && parseResult.errors.length > 0) {
      logWarning(`Parse completed with ${parseResult.errors.length} error(s):`)
      parseResult.errors.forEach((err: any) => {
        console.log(`  Line ${err.line}: ${err.message}`)
      })
    }

    if (parseResult.ast) {
      logSuccess('COBOL parsing successful')
      logInfo(`Program ID: ${parseResult.ast.programId}`)
      logInfo(`Line count: ${parseResult.metadata.lineCount}`)
      logInfo(`Complexity: ${parseResult.metadata.complexity}`)
      return { success: true, parseResult }
    } else {
      logError('Failed to generate AST')
      return { success: false, parseResult }
    }
  } catch (error) {
    logError(`Parsing failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Test 2: Analyze business logic
 */
async function testAnalysis (orchestrator: any, ast: any) {
  logSection('Test 2: Logic Analysis')

  try {
    const analysis = await orchestrator.analyze(ast)

    logSuccess('Logic analysis successful')
    logInfo(`Business rules: ${analysis.businessRules.length}`)
    logInfo(`Data structures: ${analysis.dataStructures.length}`)
    logInfo(`Dependencies: ${analysis.dependencies.length}`)
    logInfo(`Patterns identified: ${analysis.patterns.length}`)

    if (analysis.patterns.length > 0) {
      console.log('\n  Detected patterns:')
      analysis.patterns.forEach((pattern: any) => {
        console.log(
          `    - ${pattern.type} (confidence: ${(
            pattern.confidence * 100
          ).toFixed(0)}%)`
        )
      })
    }

    return { success: true, analysis }
  } catch (error) {
    logError(`Analysis failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Test 3: Generate specification documents
 */
async function testSpecGeneration (orchestrator: any, analysis: any) {
  logSection('Test 3: Specification Generation')

  try {
    const spec = await orchestrator.generateSpecDocuments(analysis)

    logSuccess('Specification generation successful')
    logInfo(`Requirements: ${spec.requirements.split('\n').length} lines`)
    logInfo(`Design: ${spec.design.split('\n').length} lines`)
    logInfo(`Tasks: ${spec.tasks.split('\n').length} lines`)

    return { success: true, spec }
  } catch (error) {
    logError(`Spec generation failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Test 4: Generate AWS infrastructure code
 */
async function testCodeGeneration (orchestrator: any, analysis: any) {
  logSection('Test 4: AWS Code Generation')

  try {
    const awsCode = await orchestrator.generateAWSInfrastructure(analysis)

    logSuccess('AWS code generation successful')
    logInfo(`Lambda functions: ${awsCode.lambdaFunctions.length}`)
    logInfo(
      `API Gateway config: ${
        typeof awsCode.apiGateway === 'string' ? 'generated' : 'not generated'
      }`
    )
    logInfo(
      `CDK stack: ${
        typeof awsCode.cdkStack === 'string' ? 'generated' : 'not generated'
      }`
    )

    return { success: true, awsCode }
  } catch (error) {
    logError(`Code generation failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Test 5: Generate modernization recommendations
 */
async function testModernizationPlan (orchestrator: any, analysis: any) {
  logSection('Test 5: Modernization Planning')

  try {
    const plan = await orchestrator.suggestModernizationStrategy(analysis)

    logSuccess('Modernization planning successful')
    logInfo(`Recommendations: ${plan.recommendations.length}`)
    logInfo(`Prioritized modules: ${plan.prioritizedModules.length}`)
    logInfo(`De-risking strategies: ${plan.deRiskingStrategies.length}`)

    if (plan.recommendations.length > 0) {
      console.log('\n  Top recommendation:')
      const top = plan.recommendations[0]
      console.log(`    Module: ${top.module}`)
      console.log(`    AWS Service: ${top.awsService}`)
      console.log(`    Effort: ${top.estimatedEffort}`)
    }

    return { success: true, plan }
  } catch (error) {
    logError(`Modernization planning failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Test 6: Complete end-to-end workflow
 */
async function testCompleteWorkflow (orchestrator: any, cobolSource: string) {
  logSection('Test 6: Complete End-to-End Workflow')

  try {
    const startTime = Date.now()

    const result = await orchestrator.analyzeComplete(
      cobolSource,
      (progress: any) => {
        console.log(
          `  [${progress.progress}%] ${progress.stage}: ${progress.message}`
        )
      }
    )

    const duration = ((Date.now() - startTime) / 1000).toFixed(2)

    logSuccess(`Complete workflow finished in ${duration}s`)
    logInfo(`Parse result: ${result.parseResult ? 'OK' : 'FAILED'}`)
    logInfo(`Analysis: ${result.analysis ? 'OK' : 'FAILED'}`)
    logInfo(`Explanation: ${result.explanation ? 'OK' : 'SKIPPED (no LLM)'}`)
    logInfo(`Spec: ${result.spec ? 'OK' : 'FAILED'}`)
    logInfo(`AWS Code: ${result.awsCode ? 'OK' : 'FAILED'}`)
    logInfo(`Modernization Plan: ${result.modernizationPlan ? 'OK' : 'FAILED'}`)

    return { success: true, result, duration }
  } catch (error) {
    logError(`Complete workflow failed: ${error}`)
    return { success: false, error }
  }
}

/**
 * Main test runner
 */
async function runTests () {
  log('\n🐍 COBRA Integration Test Suite', 'cyan')
  log('Testing end-to-end component integration\n', 'cyan')

  // Load example COBOL file
  let cobolSource: string
  try {
    cobolSource = readFileSync(
      join(process.cwd(), 'examples', 'interest-calculation.cbl'),
      'utf-8'
    )
    logSuccess('Loaded example COBOL file: interest-calculation.cbl')
  } catch (error) {
    logError('Failed to load example COBOL file')
    logInfo('Using minimal COBOL sample instead')
    cobolSource = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT PIC 9(7)V99 VALUE 0.
       01 WS-RATE PIC 9V9999 VALUE 0.
       01 WS-INTEREST PIC 9(7)V99 VALUE 0.
       
       PROCEDURE DIVISION.
           COMPUTE WS-INTEREST = WS-AMOUNT * WS-RATE.
           DISPLAY "Interest: " WS-INTEREST.
           STOP RUN.`
  }

  // Create orchestrator
  const orchestrator = createOrchestrator({
    llmApiKey: process.env.OPENAI_API_KEY || process.env.ANTHROPIC_API_KEY,
    llmProvider: process.env.OPENAI_API_KEY ? 'openai' : 'anthropic',
    enableCaching: true,
    verbose: false
  })

  if (!process.env.OPENAI_API_KEY && !process.env.ANTHROPIC_API_KEY) {
    logWarning('No LLM API key found - explanation generation will be skipped')
    logInfo(
      'Set OPENAI_API_KEY or ANTHROPIC_API_KEY environment variable to enable'
    )
  }

  // Run tests
  const results = []

  // Test 1: Parsing
  const parseTest = await testParsing(orchestrator, cobolSource)
  results.push(parseTest)

  if (!parseTest.success || !parseTest.parseResult?.ast) {
    logError('Cannot continue tests without successful parsing')
    printSummary(results)
    process.exit(1)
  }

  // Test 2: Analysis
  const analysisTest = await testAnalysis(
    orchestrator,
    parseTest.parseResult.ast
  )
  results.push(analysisTest)

  if (!analysisTest.success || !analysisTest.analysis) {
    logError('Cannot continue tests without successful analysis')
    printSummary(results)
    process.exit(1)
  }

  // Test 3: Spec Generation
  const specTest = await testSpecGeneration(orchestrator, analysisTest.analysis)
  results.push(specTest)

  // Test 4: Code Generation
  const codeTest = await testCodeGeneration(orchestrator, analysisTest.analysis)
  results.push(codeTest)

  // Test 5: Modernization Planning
  const planTest = await testModernizationPlan(
    orchestrator,
    analysisTest.analysis
  )
  results.push(planTest)

  // Test 6: Complete Workflow
  const workflowTest = await testCompleteWorkflow(orchestrator, cobolSource)
  results.push(workflowTest)

  // Print summary
  printSummary(results)

  // Exit with appropriate code
  const allPassed = results.every(r => r.success)
  process.exit(allPassed ? 0 : 1)
}

/**
 * Print test summary
 */
function printSummary (results: any[]) {
  logSection('Test Summary')

  const passed = results.filter(r => r.success).length
  const failed = results.length - passed

  console.log(`Total tests: ${results.length}`)
  logSuccess(`Passed: ${passed}`)
  if (failed > 0) {
    logError(`Failed: ${failed}`)
  }

  const successRate = ((passed / results.length) * 100).toFixed(0)
  console.log(`\nSuccess rate: ${successRate}%`)

  if (passed === results.length) {
    log('\n✓ All integration tests passed!', 'green')
    log('All COBRA components are properly wired together.', 'green')
  } else {
    log('\n✗ Some integration tests failed', 'red')
    log('Check the errors above for details.', 'red')
  }
}

// Run tests
runTests().catch(error => {
  logError(`Fatal error: ${error}`)
  process.exit(1)
})
