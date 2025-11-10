/**
 * LLM Prompt Templates for COBOL Explanation
 */

import type { AST, LogicAnalysis } from '../mcp-server/types.js'

/**
 * Generate system prompt for COBOL explanation
 */
export function getSystemPrompt (): string {
  return `You are an expert COBOL analyst specializing in banking systems. Your role is to analyze COBOL programs and explain their business logic in clear, natural language.

When analyzing COBOL code, focus on:
1. The business purpose and functionality
2. Input data sources and their formats
3. Output destinations and formats
4. Business rules and validation logic
5. Calculations and transformations
6. Banking-specific patterns (interest calculations, transactions, batch processing)

Provide explanations that are:
- Clear and accessible to non-COBOL developers
- Focused on business logic rather than technical syntax
- Structured with clear sections
- Specific about data flows and transformations
- Highlighting any banking domain patterns`
}

/**
 * Generate user prompt for COBOL explanation
 */
export function getExplanationPrompt (
  ast: AST,
  analysis: LogicAnalysis,
  fileName: string = 'UNKNOWN'
): string {
  const prompt = `Analyze this COBOL program and provide a comprehensive explanation.

**Program Information:**
- Program ID: ${ast.programId}
- File Name: ${fileName}

**Data Structures:**
${formatDataStructures(analysis.dataStructures)}

**Business Rules Identified:**
${formatBusinessRules(analysis.businessRules)}

**Banking Patterns Detected:**
${formatBankingPatterns(analysis.patterns)}

**Dependencies:**
${formatDependencies(analysis.dependencies)}

**Entry Points:**
${formatEntryPoints(analysis.entryPoints)}

Please provide:
1. **Summary**: A brief overview of what this program does (2-3 sentences)
2. **Business Logic**: Detailed explanation of the core business functionality
3. **Inputs**: List all input sources with their purposes
4. **Outputs**: List all output destinations with their purposes
5. **Rules**: Numbered list of business rules and validations
6. **Complexity**: Assessment of the program's complexity (Low/Medium/High) with justification
7. **Recommendations**: Suggestions for modernization or improvement

Format your response as JSON with these exact keys: summary, businessLogic, inputs, outputs, rules, complexity, recommendations`

  return prompt
}

/**
 * Format data structures for prompt
 */
function formatDataStructures (structures: any[]): string {
  if (!structures || structures.length === 0) {
    return '- No data structures identified'
  }

  return structures
    .map(struct => {
      const fields = struct.fields
        ?.slice(0, 5)
        .map((f: any) => `  - ${f.name} (${f.picture || 'N/A'})`)
        .join('\n')
      return `- ${struct.name} (${struct.type})\n${fields || '  - No fields'}`
    })
    .join('\n\n')
}

/**
 * Format business rules for prompt
 */
function formatBusinessRules (rules: any[]): string {
  if (!rules || rules.length === 0) {
    return '- No business rules identified'
  }

  return rules
    .slice(0, 10)
    .map(
      (rule, idx) =>
        `${idx + 1}. [${rule.type}] ${rule.description}\n   Lines: ${
          rule.cobolSource.startLine
        }-${rule.cobolSource.endLine}`
    )
    .join('\n')
}

/**
 * Format banking patterns for prompt
 */
function formatBankingPatterns (patterns: any[]): string {
  if (!patterns || patterns.length === 0) {
    return '- No banking patterns detected'
  }

  return patterns
    .map(
      pattern =>
        `- ${pattern.type.replace(/_/g, ' ').toUpperCase()} (confidence: ${(
          pattern.confidence * 100
        ).toFixed(0)}%)\n  ${pattern.description}`
    )
    .join('\n')
}

/**
 * Format dependencies for prompt
 */
function formatDependencies (dependencies: any[]): string {
  if (!dependencies || dependencies.length === 0) {
    return '- No dependencies identified'
  }

  return dependencies
    .map(
      dep =>
        `- ${dep.type}: ${dep.name}${dep.location ? ` (${dep.location})` : ''}`
    )
    .join('\n')
}

/**
 * Format entry points for prompt
 */
function formatEntryPoints (entryPoints: any[]): string {
  if (!entryPoints || entryPoints.length === 0) {
    return '- No entry points identified'
  }

  return entryPoints
    .map(ep => {
      const params = ep.parameters?.length
        ? `\n  Parameters: ${ep.parameters.map((p: any) => p.name).join(', ')}`
        : ''
      return `- ${ep.name}${params}`
    })
    .join('\n')
}

/**
 * Generate prompt for quick summary (cheaper, faster)
 */
export function getQuickSummaryPrompt (
  ast: AST,
  fileName: string = 'UNKNOWN'
): string {
  return `Provide a brief 2-3 sentence summary of this COBOL program:

Program ID: ${ast.programId}
File Name: ${fileName}

Focus on the main business purpose and key functionality.`
}
