/**
 * Example: Using COBRA MCP Tools
 *
 * This example demonstrates how to use the MCP tools to:
 * 1. Parse COBOL source code
 * 2. Analyze business logic
 * 3. Generate spec documents
 * 4. Get modernization recommendations
 */

import {
  parseCobol,
  analyzeLogic,
  generateSpec,
  suggestModernization
} from '../src/mcp-server/tools/index.js'

// Sample COBOL program for interest calculation
const sampleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.
       AUTHOR. BANKING SYSTEMS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL         PIC 9(7)V99 VALUE 10000.00.
       01 WS-RATE              PIC 9V9999 VALUE 0.0525.
       01 WS-DAYS              PIC 9(3) VALUE 365.
       01 WS-INTEREST          PIC 9(7)V99.
       01 WS-TOTAL-AMOUNT      PIC 9(8)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "CALCULATING INTEREST...".
           
           COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE * 
                                 (WS-DAYS / 365).
           
           COMPUTE WS-TOTAL-AMOUNT = WS-PRINCIPAL + WS-INTEREST.
           
           DISPLAY "PRINCIPAL: " WS-PRINCIPAL.
           DISPLAY "RATE: " WS-RATE.
           DISPLAY "INTEREST: " WS-INTEREST.
           DISPLAY "TOTAL AMOUNT: " WS-TOTAL-AMOUNT.
           
           STOP RUN.
`

async function demonstrateMCPTools () {
  console.log('='.repeat(60))
  console.log('COBRA MCP Tools Demonstration')
  console.log('='.repeat(60))
  console.log()

  // Step 1: Parse COBOL source
  console.log('Step 1: Parsing COBOL source code...')
  console.log('-'.repeat(60))

  const parseResult = await parseCobol(sampleCobol)

  console.log(`Program Name: ${parseResult.metadata.programName}`)
  console.log(`Line Count: ${parseResult.metadata.lineCount}`)
  console.log(`Complexity: ${parseResult.metadata.complexity}`)
  console.log(`Errors: ${parseResult.errors.length}`)
  console.log(`Warnings: ${parseResult.warnings.length}`)

  if (parseResult.errors.length > 0) {
    console.log('\nParse Errors:')
    parseResult.errors.forEach(err => {
      console.log(`  - Line ${err.line}: ${err.message}`)
    })
    return
  }

  console.log('✓ Parsing successful!')
  console.log()

  // Step 2: Analyze business logic
  console.log('Step 2: Analyzing business logic...')
  console.log('-'.repeat(60))

  if (!parseResult.ast) {
    console.log('✗ No AST available for analysis')
    return
  }

  const analysis = await analyzeLogic(parseResult.ast)

  console.log(`Business Rules: ${analysis.businessRules.length}`)
  console.log(`Data Structures: ${analysis.dataStructures.length}`)
  console.log(`Dependencies: ${analysis.dependencies.length}`)
  console.log(`Entry Points: ${analysis.entryPoints.length}`)
  console.log(`Banking Patterns: ${analysis.patterns.length}`)

  if (analysis.patterns.length > 0) {
    console.log('\nDetected Banking Patterns:')
    analysis.patterns.forEach(pattern => {
      console.log(
        `  - ${pattern.type.replace(/_/g, ' ').toUpperCase()} (confidence: ${(
          pattern.confidence * 100
        ).toFixed(0)}%)`
      )
      console.log(`    ${pattern.description}`)
    })
  }

  console.log('✓ Analysis complete!')
  console.log()

  // Step 3: Generate spec documents
  console.log('Step 3: Generating Kiro spec documents...')
  console.log('-'.repeat(60))

  const spec = await generateSpec(analysis)

  console.log(`Requirements: ${spec.requirements.split('\n').length} lines`)
  console.log(`Design: ${spec.design.split('\n').length} lines`)
  console.log(`Tasks: ${spec.tasks.split('\n').length} lines`)

  console.log('\nRequirements Preview:')
  console.log(spec.requirements.split('\n').slice(0, 10).join('\n'))
  console.log('...')

  console.log('✓ Spec generation complete!')
  console.log()

  // Step 4: Get modernization recommendations
  console.log('Step 4: Generating modernization recommendations...')
  console.log('-'.repeat(60))

  const plan = await suggestModernization(analysis)

  console.log(`Recommendations: ${plan.recommendations.length}`)
  console.log(`Prioritized Modules: ${plan.prioritizedModules.length}`)
  console.log(`De-risking Strategies: ${plan.deRiskingStrategies.length}`)

  if (plan.prioritizedModules.length > 0) {
    console.log('\nPrioritized Modules:')
    plan.prioritizedModules.forEach(module => {
      console.log(`  - ${module.name}`)
      console.log(`    Priority: ${module.priority}`)
      console.log(`    Coupling: ${module.coupling}`)
      console.log(`    Complexity: ${module.complexity}`)
      console.log(`    Business Value: ${module.businessValue}`)
    })
  }

  if (plan.recommendations.length > 0) {
    console.log('\nAWS Service Recommendations:')
    plan.recommendations.forEach(rec => {
      console.log(`  - ${rec.module} → ${rec.awsService}`)
      console.log(`    Rationale: ${rec.rationale}`)
      console.log(`    Estimated Effort: ${rec.estimatedEffort}`)
    })
  }

  if (plan.deRiskingStrategies.length > 0) {
    console.log('\nDe-risking Strategies:')
    plan.deRiskingStrategies.forEach((strategy, idx) => {
      console.log(`  ${idx + 1}. ${strategy.approach}`)
      console.log(`     ${strategy.description}`)
      console.log(`     Risks: ${strategy.risks.length}`)
      console.log(`     Mitigations: ${strategy.mitigations.length}`)
    })
  }

  console.log('✓ Modernization recommendations complete!')
  console.log()

  console.log('='.repeat(60))
  console.log('Demonstration Complete!')
  console.log('='.repeat(60))
}

// Run the demonstration
demonstrateMCPTools().catch(error => {
  console.error('Error:', error)
  process.exit(1)
})
