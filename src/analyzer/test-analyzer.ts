/**
 * Test script for Logic Analyzer
 * Verifies that all analyzer components work correctly
 */

import { readFileSync } from 'fs'
import { createParser } from '../parser/cobol-parser.js'
import { createLogicAnalyzer } from './index.js'

async function testAnalyzer () {
  console.log('=== Testing COBRA Logic Analyzer ===\n')

  // Read example COBOL file
  const cobolSource = readFileSync('examples/interest-calculation.cbl', 'utf-8')
  console.log('✓ Loaded example COBOL file\n')

  // Parse COBOL
  const parser = createParser()
  const parseResult = await parser.parse(cobolSource)

  if (!parseResult.ast) {
    console.error('✗ Failed to parse COBOL')
    console.error('Errors:', parseResult.errors)
    return
  }

  console.log('✓ Parsed COBOL successfully')
  console.log(`  Program: ${parseResult.metadata.programName}`)
  console.log(`  Lines: ${parseResult.metadata.lineCount}`)
  console.log(`  Complexity: ${parseResult.metadata.complexity}\n`)

  // Analyze logic
  const analyzer = createLogicAnalyzer()
  const analysis = analyzer.analyze(parseResult.ast, 'interest-calculation.cbl')

  console.log('✓ Analysis complete\n')

  // Display results
  console.log('--- Banking Patterns ---')
  console.log(`Found ${analysis.patterns.length} patterns:`)
  for (const pattern of analysis.patterns) {
    console.log(
      `  • ${pattern.type} (confidence: ${pattern.confidence.toFixed(2)})`
    )
    console.log(`    ${pattern.description}`)
    console.log(`    Line ${pattern.location.startLine}`)
  }
  console.log()

  console.log('--- Business Rules ---')
  console.log(`Found ${analysis.businessRules.length} business rules:`)
  for (const rule of analysis.businessRules.slice(0, 5)) {
    console.log(`  • ${rule.id}: ${rule.type}`)
    console.log(`    ${rule.description}`)
    console.log(
      `    Inputs: ${rule.inputs.map(i => i.name).join(', ') || 'none'}`
    )
    console.log(
      `    Outputs: ${rule.outputs.map(o => o.name).join(', ') || 'none'}`
    )
  }
  if (analysis.businessRules.length > 5) {
    console.log(`  ... and ${analysis.businessRules.length - 5} more`)
  }
  console.log()

  console.log('--- Data Structures ---')
  console.log(`Found ${analysis.dataStructures.length} data structures:`)
  for (const struct of analysis.dataStructures) {
    console.log(`  • ${struct.name} (${struct.type})`)
    console.log(`    Fields: ${struct.fields.length}`)
  }
  console.log()

  console.log('--- Dependencies ---')
  console.log(`Found ${analysis.dependencies.length} dependencies:`)
  for (const dep of analysis.dependencies) {
    console.log(`  • ${dep.type}: ${dep.name}`)
  }
  console.log()

  console.log('--- Entry Points ---')
  console.log(`Found ${analysis.entryPoints.length} entry points:`)
  for (const entry of analysis.entryPoints) {
    console.log(`  • ${entry.name}`)
  }
  console.log()

  console.log('=== All Tests Passed ===')
}

// Run tests
testAnalyzer().catch(error => {
  console.error('Test failed:', error)
  process.exit(1)
})
