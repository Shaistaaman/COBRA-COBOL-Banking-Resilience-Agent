/**
 * Example: Using the LLM Explanation Generator
 *
 * This example demonstrates how to:
 * 1. Parse COBOL code
 * 2. Analyze business logic
 * 3. Generate natural-language explanations
 * 4. Format output as markdown
 */

import { createCobolParser } from '../src/parser/index.js'
import { createLogicAnalyzer } from '../src/analyzer/index.js'
import {
  createExplanationGenerator,
  formatAnalysisAsMarkdown,
  formatAsJSON
} from '../src/llm/index.js'
import fs from 'fs/promises'

async function main () {
  // Read example COBOL file
  const cobolSource = await fs.readFile(
    'examples/interest-calculation.cbl',
    'utf-8'
  )

  console.log('Step 1: Parsing COBOL code...')
  const parser = createCobolParser()
  const parseResult = parser.parse(cobolSource)

  if (parseResult.errors.length > 0) {
    console.error('Parse errors:', parseResult.errors)
    return
  }

  console.log('✓ Parsed successfully')
  console.log(`  Program: ${parseResult.metadata.programName}`)
  console.log(`  Lines: ${parseResult.metadata.lineCount}`)
  console.log(`  Complexity: ${parseResult.metadata.complexity}`)

  console.log('\nStep 2: Analyzing business logic...')
  const analyzer = createLogicAnalyzer()
  const analysis = analyzer.analyze(
    parseResult.ast!,
    'interest-calculation.cbl'
  )

  console.log('✓ Analysis complete')
  console.log(`  Business Rules: ${analysis.businessRules.length}`)
  console.log(`  Banking Patterns: ${analysis.patterns.length}`)
  console.log(`  Dependencies: ${analysis.dependencies.length}`)

  console.log('\nStep 3: Generating natural-language explanation...')

  // Check for API key
  const apiKey = process.env.OPENAI_API_KEY || process.env.ANTHROPIC_API_KEY
  if (!apiKey) {
    console.log('⚠ No API key found. Set OPENAI_API_KEY or ANTHROPIC_API_KEY')
    console.log('  Skipping LLM explanation generation')
    console.log('  (Fallback explanation will be used in production)')
    return
  }

  // Create explanation generator
  const generator = createExplanationGenerator({
    llmConfig: {
      provider: process.env.OPENAI_API_KEY ? 'openai' : 'anthropic',
      apiKey,
      model: process.env.OPENAI_API_KEY
        ? 'gpt-4o-mini'
        : 'claude-3-5-sonnet-20241022',
      temperature: 0.3,
      maxTokens: 2000
    },
    cacheEnabled: true,
    cacheTTL: 3600
  })

  // Generate explanation
  const explanation = await generator.generateExplanation(
    parseResult.ast!,
    analysis,
    'interest-calculation.cbl'
  )

  console.log('✓ Explanation generated')
  console.log(`\nSummary:\n${explanation.summary}`)

  console.log('\nStep 4: Formatting output...')

  // Format as markdown
  const markdown = formatAnalysisAsMarkdown(
    parseResult.ast!,
    analysis,
    explanation,
    'interest-calculation.cbl',
    {
      includeSourceReferences: true,
      includeMetadata: true,
      maxRulesDisplay: 20
    }
  )

  // Save markdown
  await fs.writeFile('examples/analysis-output.md', markdown)
  console.log('✓ Markdown saved to examples/analysis-output.md')

  // Format as JSON
  const json = formatAsJSON(
    parseResult.ast!,
    analysis,
    explanation,
    'interest-calculation.cbl'
  )

  // Save JSON
  await fs.writeFile('examples/analysis-output.json', json)
  console.log('✓ JSON saved to examples/analysis-output.json')

  // Display cache stats
  const cacheStats = generator.getCacheStats()
  console.log('\nCache Statistics:')
  console.log(`  Enabled: ${cacheStats.enabled}`)
  console.log(`  Entries: ${cacheStats.size}`)
  console.log(`  TTL: ${cacheStats.ttlSeconds}s`)

  console.log('\n✅ Complete! Check the output files.')
}

main().catch(console.error)
