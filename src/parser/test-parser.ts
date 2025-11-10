/**
 * Simple test script for COBOL parser
 */

import { readFileSync } from 'fs'
import { createParser } from './cobol-parser.js'
import { extractCopybooks } from './copybook-extractor.js'
import {
  picToTypeScript,
  generateTypeScriptInterface
} from './data-structure-parser.js'

async function testParser () {
  console.log('Testing COBOL Parser...\n')

  // Read sample COBOL file
  const cobolSource = readFileSync('examples/interest-calculation.cbl', 'utf-8')

  // Create parser
  const parser = createParser({ dialect: 'IBM' })

  // Parse COBOL
  console.log('Parsing COBOL source...')
  const result = await parser.parse(cobolSource)

  console.log('\n=== Parse Result ===')
  console.log('Program Name:', result.metadata.programName)
  console.log('Line Count:', result.metadata.lineCount)
  console.log('Complexity:', result.metadata.complexity)
  console.log('Errors:', result.errors.length)
  console.log('Warnings:', result.warnings.length)

  if (result.errors.length > 0) {
    console.log('\nErrors:')
    result.errors.forEach(err => {
      console.log(`  Line ${err.line}: ${err.message}`)
    })
  }

  if (result.ast) {
    console.log('\n=== AST Structure ===')
    console.log('Program ID:', result.ast.programId)

    if (result.ast.divisions.identification) {
      console.log('\nIdentification Division:')
      console.log('  Author:', result.ast.divisions.identification.author)
      console.log(
        '  Date Written:',
        result.ast.divisions.identification.dateWritten
      )
    }

    if (result.ast.divisions.data?.workingStorage) {
      console.log(
        '\nWorking Storage Elements:',
        result.ast.divisions.data.workingStorage.length
      )
      result.ast.divisions.data.workingStorage.slice(0, 5).forEach(elem => {
        console.log(`  ${elem.level} ${elem.name} ${elem.picture || '(group)'}`)
      })
    }

    if (result.ast.divisions.procedure?.statements) {
      console.log(
        '\nProcedure Statements:',
        result.ast.divisions.procedure.statements.length
      )
      result.ast.divisions.procedure.statements.slice(0, 5).forEach(stmt => {
        console.log(`  ${stmt.type}`)
      })
    }
  }

  // Test copybook extraction
  console.log('\n=== Copybook Extraction ===')
  const copybooks = extractCopybooks(cobolSource)
  console.log('Copybooks found:', copybooks.length)

  // Test PIC to TypeScript conversion
  console.log('\n=== PIC to TypeScript Conversion ===')
  const testPics = ['X(10)', '9(5)', '9(13)V99', 'S9(7)V99', 'A(20)']

  testPics.forEach(pic => {
    const tsType = picToTypeScript(pic)
    console.log(`  ${pic} -> ${tsType.tsType} (${tsType.description})`)
  })

  // Test TypeScript interface generation
  if (result.ast?.divisions.data?.workingStorage) {
    console.log('\n=== TypeScript Interface Generation ===')
    const tsInterface = generateTypeScriptInterface(
      result.ast.divisions.data.workingStorage,
      'AccountRecord'
    )
    console.log('\nGenerated Interface:')
    console.log(tsInterface.code)
  }

  console.log('\n✓ Parser test completed successfully!')
}

// Run test
testParser().catch(error => {
  console.error('Test failed:', error)
  process.exit(1)
})
