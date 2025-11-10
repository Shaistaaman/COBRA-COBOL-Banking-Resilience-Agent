/**
 * Structured Output Formatter
 * Converts analysis results into formatted markdown documentation
 */

import type { AST, LogicAnalysis } from '../mcp-server/types.js'
import type { ExplanationResponse } from './types.js'

export interface FormatterOptions {
  includeSourceReferences?: boolean
  includeMetadata?: boolean
  maxRulesDisplay?: number
}

/**
 * Format complete analysis as markdown documentation
 */
export function formatAnalysisAsMarkdown (
  ast: AST,
  analysis: LogicAnalysis,
  explanation: ExplanationResponse,
  fileName: string = 'UNKNOWN',
  options: FormatterOptions = {}
): string {
  const {
    includeSourceReferences = true,
    includeMetadata = true,
    maxRulesDisplay = 20
  } = options

  const sections: string[] = []

  // Header
  sections.push(`# COBOL Program Analysis: ${ast.programId}`)
  sections.push('')

  // Metadata
  if (includeMetadata) {
    sections.push('## Metadata')
    sections.push('')
    sections.push(`- **Program ID**: ${ast.programId}`)
    sections.push(`- **File Name**: ${fileName}`)
    sections.push(`- **Complexity**: ${explanation.complexity}`)
    sections.push(
      `- **Business Rules**: ${analysis.businessRules.length} identified`
    )
    sections.push(
      `- **Banking Patterns**: ${analysis.patterns.length} detected`
    )
    sections.push(`- **Dependencies**: ${analysis.dependencies.length}`)
    sections.push('')
  }

  // Summary
  sections.push('## Summary')
  sections.push('')
  sections.push(explanation.summary)
  sections.push('')

  // Business Logic
  sections.push('## Business Logic')
  sections.push('')
  sections.push(explanation.businessLogic)
  sections.push('')

  // Inputs
  if (explanation.inputs.length > 0) {
    sections.push('## Inputs')
    sections.push('')
    sections.push(formatInputsOutputs(explanation.inputs))
    sections.push('')
  }

  // Outputs
  if (explanation.outputs.length > 0) {
    sections.push('## Outputs')
    sections.push('')
    sections.push(formatInputsOutputs(explanation.outputs))
    sections.push('')
  }

  // Data Structures
  if (analysis.dataStructures.length > 0) {
    sections.push('## Data Structures')
    sections.push('')
    sections.push(formatDataStructuresTable(analysis.dataStructures))
    sections.push('')
  }

  // Business Rules
  if (explanation.rules.length > 0) {
    sections.push('## Business Rules')
    sections.push('')
    sections.push(
      formatBusinessRulesList(
        explanation.rules,
        analysis.businessRules,
        includeSourceReferences,
        maxRulesDisplay
      )
    )
    sections.push('')
  }

  // Banking Patterns
  if (analysis.patterns.length > 0) {
    sections.push('## Banking Patterns Detected')
    sections.push('')
    sections.push(formatBankingPatternsTable(analysis.patterns))
    sections.push('')
  }

  // Dependencies
  if (analysis.dependencies.length > 0) {
    sections.push('## Dependencies')
    sections.push('')
    sections.push(formatDependenciesList(analysis.dependencies))
    sections.push('')
  }

  // Recommendations
  if (explanation.recommendations.length > 0) {
    sections.push('## Modernization Recommendations')
    sections.push('')
    sections.push(formatRecommendationsList(explanation.recommendations))
    sections.push('')
  }

  return sections.join('\n')
}

/**
 * Format inputs/outputs as bullet list
 */
function formatInputsOutputs (items: string[]): string {
  return items.map(item => `- ${item}`).join('\n')
}

/**
 * Format data structures as markdown table
 */
function formatDataStructuresTable (structures: any[]): string {
  const lines: string[] = []

  lines.push('| Structure Name | Type | Fields |')
  lines.push('|----------------|------|--------|')

  for (const struct of structures) {
    const fieldCount = struct.fields?.length || 0
    const fieldSummary = fieldCount > 0 ? `${fieldCount} fields` : 'No fields'

    lines.push(`| ${struct.name} | ${struct.type} | ${fieldSummary} |`)
  }

  return lines.join('\n')
}

/**
 * Format data structure details with field information
 */
export function formatDataStructureDetails (structure: any): string {
  const lines: string[] = []

  lines.push(`### ${structure.name}`)
  lines.push('')
  lines.push(`**Type**: ${structure.type}`)
  lines.push('')

  if (structure.fields && structure.fields.length > 0) {
    lines.push('**Fields**:')
    lines.push('')
    lines.push('| Level | Name | Picture | Value | Usage |')
    lines.push('|-------|------|---------|-------|-------|')

    for (const field of structure.fields) {
      const level = field.level || '-'
      const name = field.name || '-'
      const picture = field.picture || '-'
      const value = field.value || '-'
      const usage = field.usage || '-'

      lines.push(`| ${level} | ${name} | ${picture} | ${value} | ${usage} |`)
    }
  }

  return lines.join('\n')
}

/**
 * Format business rules as numbered list with source references
 */
function formatBusinessRulesList (
  ruleDescriptions: string[],
  detailedRules: any[],
  includeSourceReferences: boolean,
  maxDisplay: number
): string {
  const lines: string[] = []

  const displayCount = Math.min(ruleDescriptions.length, maxDisplay)

  for (let i = 0; i < displayCount; i++) {
    const description = ruleDescriptions[i]
    lines.push(`${i + 1}. ${description}`)

    // Add source reference if available
    if (includeSourceReferences && detailedRules[i]) {
      const rule = detailedRules[i]
      if (rule.cobolSource) {
        lines.push(
          `   - *Source: Lines ${rule.cobolSource.startLine}-${rule.cobolSource.endLine}*`
        )
      }
    }
  }

  if (ruleDescriptions.length > maxDisplay) {
    lines.push('')
    lines.push(`*... and ${ruleDescriptions.length - maxDisplay} more rules*`)
  }

  return lines.join('\n')
}

/**
 * Format banking patterns as table
 */
function formatBankingPatternsTable (patterns: any[]): string {
  const lines: string[] = []

  lines.push('| Pattern Type | Confidence | Description | Location |')
  lines.push('|--------------|------------|-------------|----------|')

  for (const pattern of patterns) {
    const type = pattern.type.replace(/_/g, ' ').toUpperCase()
    const confidence = `${(pattern.confidence * 100).toFixed(0)}%`
    const description = pattern.description || 'N/A'
    const location = pattern.location
      ? `Lines ${pattern.location.startLine}-${pattern.location.endLine}`
      : 'N/A'

    lines.push(`| ${type} | ${confidence} | ${description} | ${location} |`)
  }

  return lines.join('\n')
}

/**
 * Format dependencies as categorized list
 */
function formatDependenciesList (dependencies: any[]): string {
  const lines: string[] = []

  // Group by type
  const grouped = dependencies.reduce((acc, dep) => {
    if (!acc[dep.type]) {
      acc[dep.type] = []
    }
    acc[dep.type].push(dep)
    return acc
  }, {} as Record<string, any[]>)

  for (const [type, deps] of Object.entries(grouped)) {
    lines.push(`**${type.toUpperCase()}**:`)
    lines.push('')

    for (const dep of deps as any[]) {
      const location = dep.location ? ` (${dep.location})` : ''
      lines.push(`- ${dep.name}${location}`)
    }

    lines.push('')
  }

  return lines.join('\n')
}

/**
 * Format recommendations as numbered list
 */
function formatRecommendationsList (recommendations: string[]): string {
  return recommendations.map((rec, idx) => `${idx + 1}. ${rec}`).join('\n')
}

/**
 * Format quick summary (minimal output)
 */
export function formatQuickSummary (
  ast: AST,
  analysis: LogicAnalysis,
  summary: string
): string {
  const lines: string[] = []

  lines.push(`# ${ast.programId}`)
  lines.push('')
  lines.push(summary)
  lines.push('')
  lines.push(
    `- **Business Rules**: ${analysis.businessRules.length} | **Patterns**: ${analysis.patterns.length} | **Dependencies**: ${analysis.dependencies.length}`
  )

  return lines.join('\n')
}

/**
 * Format as JSON (for API responses)
 */
export function formatAsJSON (
  ast: AST,
  analysis: LogicAnalysis,
  explanation: ExplanationResponse,
  fileName: string = 'UNKNOWN'
): string {
  const output = {
    metadata: {
      programId: ast.programId,
      fileName,
      complexity: explanation.complexity,
      timestamp: new Date().toISOString()
    },
    summary: explanation.summary,
    businessLogic: explanation.businessLogic,
    inputs: explanation.inputs,
    outputs: explanation.outputs,
    dataStructures: analysis.dataStructures.map(ds => ({
      name: ds.name,
      type: ds.type,
      fieldCount: ds.fields?.length || 0
    })),
    businessRules: analysis.businessRules.map(rule => ({
      type: rule.type,
      description: rule.description,
      sourceLines: `${rule.cobolSource.startLine}-${rule.cobolSource.endLine}`
    })),
    patterns: analysis.patterns.map(pattern => ({
      type: pattern.type,
      confidence: pattern.confidence,
      description: pattern.description
    })),
    dependencies: analysis.dependencies,
    recommendations: explanation.recommendations
  }

  return JSON.stringify(output, null, 2)
}

/**
 * Format as plain text (for console output)
 */
export function formatAsPlainText (
  ast: AST,
  analysis: LogicAnalysis,
  explanation: ExplanationResponse
): string {
  const lines: string[] = []

  lines.push(`COBOL PROGRAM ANALYSIS: ${ast.programId}`)
  lines.push('='.repeat(60))
  lines.push('')
  lines.push('SUMMARY:')
  lines.push(explanation.summary)
  lines.push('')
  lines.push('BUSINESS LOGIC:')
  lines.push(explanation.businessLogic)
  lines.push('')

  if (explanation.inputs.length > 0) {
    lines.push('INPUTS:')
    explanation.inputs.forEach(input => lines.push(`  - ${input}`))
    lines.push('')
  }

  if (explanation.outputs.length > 0) {
    lines.push('OUTPUTS:')
    explanation.outputs.forEach(output => lines.push(`  - ${output}`))
    lines.push('')
  }

  if (explanation.rules.length > 0) {
    lines.push('BUSINESS RULES:')
    explanation.rules.forEach((rule, idx) =>
      lines.push(`  ${idx + 1}. ${rule}`)
    )
    lines.push('')
  }

  lines.push(`COMPLEXITY: ${explanation.complexity}`)
  lines.push(`PATTERNS DETECTED: ${analysis.patterns.length}`)
  lines.push(`DEPENDENCIES: ${analysis.dependencies.length}`)

  return lines.join('\n')
}
