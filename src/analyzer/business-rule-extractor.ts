/**
 * Business Rule Extraction Engine
 * Extracts business logic from COBOL PROCEDURE DIVISION
 */

import type {
  AST,
  BusinessRule,
  Statement,
  DataElement,
  Condition,
  SourceLocation
} from '../mcp-server/types.js'

export interface BusinessRuleExtractorOptions {
  includeSimpleRules?: boolean
  maxRuleComplexity?: number
}

/**
 * Business Rule Extractor
 */
export class BusinessRuleExtractor {
  private options: BusinessRuleExtractorOptions
  private ruleCounter: number = 0

  constructor (options: BusinessRuleExtractorOptions = {}) {
    this.options = {
      includeSimpleRules: options.includeSimpleRules ?? true,
      maxRuleComplexity: options.maxRuleComplexity ?? 10
    }
  }

  /**
   * Extract all business rules from AST
   */
  extractBusinessRules (ast: AST, fileName: string = 'UNKNOWN'): BusinessRule[] {
    const rules: BusinessRule[] = []

    if (!ast.divisions.procedure) {
      return rules
    }

    const statements = ast.divisions.procedure.statements
    const dataElements = ast.divisions.data?.workingStorage || []

    // Extract IF-THEN-ELSE logic
    rules.push(
      ...this.extractConditionalRules(statements, dataElements, fileName)
    )

    // Extract COMPUTE operations
    rules.push(...this.extractComputeRules(statements, dataElements, fileName))

    // Extract arithmetic operations
    rules.push(
      ...this.extractArithmeticRules(statements, dataElements, fileName)
    )

    // Extract PERFORM workflow steps
    rules.push(...this.extractPerformRules(statements, dataElements, fileName))

    return rules
  }

  /**
   * Extract IF-THEN-ELSE logic as business rules
   */
  private extractConditionalRules (
    statements: Statement[],
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule[] {
    const rules: BusinessRule[] = []

    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i]

      if (stmt.type === 'IF' && stmt.raw) {
        const rule = this.parseIfStatement(stmt, dataElements, fileName)
        if (rule) {
          rules.push(rule)
        }
      }
    }

    return rules
  }

  /**
   * Parse IF statement into business rule
   */
  private parseIfStatement (
    stmt: Statement,
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule | null {
    const raw = stmt.raw

    // Extract condition and action
    const ifMatch = raw.match(
      /IF\s+(.+?)(?:\s+THEN)?(?:\s+(MOVE|ADD|SUBTRACT|MULTIPLY|DIVIDE|COMPUTE|PERFORM|DISPLAY|CALL).+)?$/i
    )

    if (!ifMatch) return null

    const conditionText = ifMatch[1].trim()
    const actionText = ifMatch[2]
      ? raw.substring(raw.indexOf(ifMatch[2]))
      : 'Continue processing'

    // Identify inputs and outputs
    const inputs = this.identifyVariables(conditionText, dataElements)
    const outputs = this.identifyVariables(actionText, dataElements)

    // Create condition object
    const conditions: Condition[] = [
      {
        expression: conditionText,
        action: actionText
      }
    ]

    // Generate description
    const description = this.generateConditionalDescription(
      conditionText,
      actionText
    )

    return {
      id: this.generateRuleId(),
      type: 'decision',
      description,
      cobolSource: {
        file: fileName,
        startLine: stmt.location.startLine,
        endLine: stmt.location.endLine,
        snippet: raw
      },
      inputs,
      outputs,
      conditions
    }
  }

  /**
   * Extract COMPUTE and arithmetic operations
   */
  private extractComputeRules (
    statements: Statement[],
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule[] {
    const rules: BusinessRule[] = []

    for (const stmt of statements) {
      if (stmt.type === 'COMPUTE' && stmt.raw) {
        const rule = this.parseComputeStatement(stmt, dataElements, fileName)
        if (rule) {
          rules.push(rule)
        }
      }
    }

    return rules
  }

  /**
   * Parse COMPUTE statement into business rule
   */
  private parseComputeStatement (
    stmt: Statement,
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule | null {
    const raw = stmt.raw

    // Extract target and formula
    const computeMatch = raw.match(
      /COMPUTE\s+([A-Z0-9-]+)\s*=\s*(.+?)(?:\.|$)/i
    )

    if (!computeMatch) return null

    const target = computeMatch[1]
    const formula = computeMatch[2].trim()

    // Identify inputs and outputs
    const inputs = this.identifyVariables(formula, dataElements)
    const outputs = dataElements.filter(e => e.name === target)

    // Generate description
    const description = `Calculate ${target} using formula: ${formula}`

    return {
      id: this.generateRuleId(),
      type: 'calculation',
      description,
      cobolSource: {
        file: fileName,
        startLine: stmt.location.startLine,
        endLine: stmt.location.endLine,
        snippet: raw
      },
      inputs,
      outputs,
      formula
    }
  }

  /**
   * Identify COMPUTE and arithmetic operations
   */
  private extractArithmeticRules (
    statements: Statement[],
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule[] {
    const rules: BusinessRule[] = []

    const arithmeticOps = ['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE']

    for (const stmt of statements) {
      if (arithmeticOps.includes(stmt.type) && stmt.raw) {
        const rule = this.parseArithmeticStatement(stmt, dataElements, fileName)
        if (rule) {
          rules.push(rule)
        }
      }
    }

    return rules
  }

  /**
   * Parse arithmetic statement into business rule
   */
  private parseArithmeticStatement (
    stmt: Statement,
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule | null {
    const raw = stmt.raw
    const operation = stmt.type

    // Extract operands
    let inputs: DataElement[] = []
    let outputs: DataElement[] = []
    let formula = ''

    // Parse different arithmetic patterns
    if (operation === 'ADD') {
      // ADD A TO B or ADD A B GIVING C
      const addMatch = raw.match(
        /ADD\s+([A-Z0-9-]+)(?:\s+([A-Z0-9-]+))?\s+(?:TO|GIVING)\s+([A-Z0-9-]+)/i
      )
      if (addMatch) {
        const operand1 = addMatch[1]
        const operand2 = addMatch[2] || addMatch[3]
        const target = addMatch[3]

        inputs = this.findDataElements([operand1, operand2], dataElements)
        outputs = this.findDataElements([target], dataElements)
        formula = `${target} = ${operand1} + ${operand2}`
      }
    } else if (operation === 'SUBTRACT') {
      const subMatch = raw.match(
        /SUBTRACT\s+([A-Z0-9-]+)\s+FROM\s+([A-Z0-9-]+)/i
      )
      if (subMatch) {
        const operand1 = subMatch[1]
        const target = subMatch[2]

        inputs = this.findDataElements([operand1, target], dataElements)
        outputs = this.findDataElements([target], dataElements)
        formula = `${target} = ${target} - ${operand1}`
      }
    } else if (operation === 'MULTIPLY') {
      const mulMatch = raw.match(/MULTIPLY\s+([A-Z0-9-]+)\s+BY\s+([A-Z0-9-]+)/i)
      if (mulMatch) {
        const operand1 = mulMatch[1]
        const target = mulMatch[2]

        inputs = this.findDataElements([operand1, target], dataElements)
        outputs = this.findDataElements([target], dataElements)
        formula = `${target} = ${target} * ${operand1}`
      }
    } else if (operation === 'DIVIDE') {
      const divMatch = raw.match(
        /DIVIDE\s+([A-Z0-9-]+)\s+(?:BY|INTO)\s+([A-Z0-9-]+)/i
      )
      if (divMatch) {
        const operand1 = divMatch[1]
        const target = divMatch[2]

        inputs = this.findDataElements([operand1, target], dataElements)
        outputs = this.findDataElements([target], dataElements)
        formula = `${target} = ${target} / ${operand1}`
      }
    }

    if (inputs.length === 0 && outputs.length === 0) return null

    const description = `${operation} operation: ${formula || raw}`

    return {
      id: this.generateRuleId(),
      type: 'calculation',
      description,
      cobolSource: {
        file: fileName,
        startLine: stmt.location.startLine,
        endLine: stmt.location.endLine,
        snippet: raw
      },
      inputs,
      outputs,
      formula: formula || undefined
    }
  }

  /**
   * Map PERFORM statements to workflow steps
   */
  private extractPerformRules (
    statements: Statement[],
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule[] {
    const rules: BusinessRule[] = []

    for (const stmt of statements) {
      if (stmt.type === 'PERFORM' && stmt.raw) {
        const rule = this.parsePerformStatement(stmt, dataElements, fileName)
        if (rule) {
          rules.push(rule)
        }
      }
    }

    return rules
  }

  /**
   * Parse PERFORM statement into workflow step
   */
  private parsePerformStatement (
    stmt: Statement,
    dataElements: DataElement[],
    fileName: string
  ): BusinessRule | null {
    const raw = stmt.raw

    // Extract paragraph/section name
    const performMatch = raw.match(
      /PERFORM\s+([A-Z0-9-]+)(?:\s+(TIMES|UNTIL|VARYING|THRU|THROUGH))?/i
    )

    if (!performMatch) return null

    const paragraphName = performMatch[1]
    const loopType = performMatch[2]

    // Identify loop variables
    const inputs = this.identifyVariables(raw, dataElements)

    // Generate description
    let description = `Execute workflow step: ${paragraphName}`
    if (loopType) {
      description += ` (${loopType} loop)`
    }

    return {
      id: this.generateRuleId(),
      type: 'transformation',
      description,
      cobolSource: {
        file: fileName,
        startLine: stmt.location.startLine,
        endLine: stmt.location.endLine,
        snippet: raw
      },
      inputs,
      outputs: []
    }
  }

  /**
   * Helper: Identify variables in text
   */
  private identifyVariables (
    text: string,
    dataElements: DataElement[]
  ): DataElement[] {
    const variables: DataElement[] = []
    const upperText = text.toUpperCase()

    for (const element of dataElements) {
      // Match whole word boundaries
      const pattern = new RegExp(`\\b${element.name}\\b`, 'i')
      if (pattern.test(upperText)) {
        variables.push(element)
      }
    }

    return variables
  }

  /**
   * Helper: Find data elements by names
   */
  private findDataElements (
    names: string[],
    dataElements: DataElement[]
  ): DataElement[] {
    const found: DataElement[] = []

    for (const name of names) {
      const element = dataElements.find(e => e.name === name)
      if (element) {
        found.push(element)
      }
    }

    return found
  }

  /**
   * Generate human-readable description for conditional logic
   */
  private generateConditionalDescription (
    condition: string,
    action: string
  ): string {
    // Clean up COBOL syntax
    let desc = condition
      .replace(/GREATER THAN/gi, '>')
      .replace(/LESS THAN/gi, '<')
      .replace(/EQUAL TO/gi, '=')
      .replace(/NOT EQUAL/gi, '!=')

    return `If ${desc}, then ${action}`
  }

  /**
   * Generate unique rule ID
   */
  private generateRuleId (): string {
    return `RULE-${String(++this.ruleCounter).padStart(4, '0')}`
  }
}

/**
 * Factory function to create business rule extractor
 */
export function createBusinessRuleExtractor (
  options?: BusinessRuleExtractorOptions
): BusinessRuleExtractor {
  return new BusinessRuleExtractor(options)
}
