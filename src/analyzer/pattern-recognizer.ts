/**
 * Banking Pattern Recognition Module
 * Identifies common banking operations in COBOL code
 */

import type {
  AST,
  BankingPattern,
  Statement,
  DataElement,
  SourceLocation
} from '../mcp-server/types.js'

export interface PatternRecognizerOptions {
  confidenceThreshold?: number
  enableAllPatterns?: boolean
}

/**
 * Pattern Recognizer for banking operations
 */
export class PatternRecognizer {
  private options: PatternRecognizerOptions

  constructor (options: PatternRecognizerOptions = {}) {
    this.options = {
      confidenceThreshold: options.confidenceThreshold ?? 0.6,
      enableAllPatterns: options.enableAllPatterns ?? true
    }
  }

  /**
   * Identify all banking patterns in the AST
   */
  identifyPatterns (ast: AST): BankingPattern[] {
    const patterns: BankingPattern[] = []

    if (!ast.divisions.procedure) {
      return patterns
    }

    const statements = ast.divisions.procedure.statements
    const dataElements = ast.divisions.data?.workingStorage || []

    // Detect interest calculation patterns
    patterns.push(...this.detectInterestCalculation(statements, dataElements))

    // Detect loan amortization patterns
    patterns.push(...this.detectLoanAmortization(statements, dataElements))

    // Detect transaction validation patterns
    patterns.push(...this.detectTransactionValidation(statements, dataElements))

    // Detect batch processing patterns
    patterns.push(...this.detectBatchProcessing(statements, dataElements))

    // Filter by confidence threshold
    return patterns.filter(
      p => p.confidence >= this.options.confidenceThreshold!
    )
  }

  /**
   * Detect interest calculation formulas
   */
  private detectInterestCalculation (
    statements: Statement[],
    dataElements: DataElement[]
  ): BankingPattern[] {
    const patterns: BankingPattern[] = []

    // Look for COMPUTE statements with interest-related variables
    const interestKeywords = [
      'INTEREST',
      'RATE',
      'PRINCIPAL',
      'AMOUNT',
      'BALANCE',
      'ACCRUAL'
    ]

    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i]

      if (stmt.type === 'COMPUTE' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()

        // Check for interest calculation patterns
        const hasInterestKeywords = interestKeywords.some(kw =>
          raw.includes(kw)
        )
        const hasMultiplication = raw.includes('*')
        const hasDivision = raw.includes('/')

        if (hasInterestKeywords && (hasMultiplication || hasDivision)) {
          const confidence = this.calculateInterestConfidence(raw, dataElements)

          if (confidence > 0) {
            patterns.push({
              type: 'interest_calculation',
              confidence,
              location: stmt.location,
              description: 'Interest calculation formula detected',
              parameters: this.extractInterestParameters(raw, dataElements)
            })
          }
        }
      }

      // Check for MULTIPLY/DIVIDE with interest variables
      if ((stmt.type === 'MULTIPLY' || stmt.type === 'DIVIDE') && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasInterestKeywords = interestKeywords.some(kw =>
          raw.includes(kw)
        )

        if (hasInterestKeywords) {
          patterns.push({
            type: 'interest_calculation',
            confidence: 0.7,
            location: stmt.location,
            description: 'Interest calculation using arithmetic operations',
            parameters: this.extractInterestParameters(raw, dataElements)
          })
        }
      }
    }

    return patterns
  }

  /**
   * Detect loan amortization logic patterns
   */
  private detectLoanAmortization (
    statements: Statement[],
    dataElements: DataElement[]
  ): BankingPattern[] {
    const patterns: BankingPattern[] = []

    const loanKeywords = [
      'LOAN',
      'PAYMENT',
      'AMORTIZATION',
      'TERM',
      'INSTALLMENT',
      'SCHEDULE'
    ]

    // Look for PERFORM loops with loan calculations
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i]

      if (stmt.type === 'PERFORM' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasLoanKeywords = loanKeywords.some(kw => raw.includes(kw))
        const hasLoop =
          raw.includes('TIMES') ||
          raw.includes('UNTIL') ||
          raw.includes('VARYING')

        if (hasLoanKeywords && hasLoop) {
          patterns.push({
            type: 'loan_amortization',
            confidence: 0.75,
            location: stmt.location,
            description: 'Loan amortization schedule calculation',
            parameters: this.extractLoanParameters(raw, dataElements)
          })
        }
      }

      // Check for COMPUTE with loan-related calculations
      if (stmt.type === 'COMPUTE' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasLoanKeywords = loanKeywords.some(kw => raw.includes(kw))

        if (hasLoanKeywords) {
          patterns.push({
            type: 'loan_amortization',
            confidence: 0.65,
            location: stmt.location,
            description: 'Loan payment calculation',
            parameters: this.extractLoanParameters(raw, dataElements)
          })
        }
      }
    }

    return patterns
  }

  /**
   * Identify transaction validation rules
   */
  private detectTransactionValidation (
    statements: Statement[],
    dataElements: DataElement[]
  ): BankingPattern[] {
    const patterns: BankingPattern[] = []

    const validationKeywords = [
      'VALID',
      'CHECK',
      'VERIFY',
      'BALANCE',
      'LIMIT',
      'OVERDRAFT',
      'INSUFFICIENT'
    ]

    // Look for IF statements with validation logic
    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i]

      if (stmt.type === 'IF' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasValidationKeywords = validationKeywords.some(kw =>
          raw.includes(kw)
        )
        const hasComparison = /(<|>|=|NOT|GREATER|LESS|EQUAL)/.test(raw)

        if (hasValidationKeywords && hasComparison) {
          patterns.push({
            type: 'validation',
            confidence: 0.8,
            location: stmt.location,
            description: 'Transaction validation rule',
            parameters: this.extractValidationParameters(raw, dataElements)
          })
        }
      }
    }

    return patterns
  }

  /**
   * Recognize batch processing workflows
   */
  private detectBatchProcessing (
    statements: Statement[],
    dataElements: DataElement[]
  ): BankingPattern[] {
    const patterns: BankingPattern[] = []

    const batchKeywords = [
      'BATCH',
      'FILE',
      'READ',
      'WRITE',
      'RECORD',
      'EOF',
      'END-OF-FILE'
    ]

    // Look for file processing loops
    let inFileLoop = false
    let loopStart: SourceLocation | null = null

    for (let i = 0; i < statements.length; i++) {
      const stmt = statements[i]

      // Detect file OPEN
      if (stmt.type === 'OPEN' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasBatchKeywords = batchKeywords.some(kw => raw.includes(kw))

        if (hasBatchKeywords) {
          loopStart = stmt.location
        }
      }

      // Detect READ loop
      if (stmt.type === 'PERFORM' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasReadLoop = raw.includes('READ') || raw.includes('UNTIL')

        if (hasReadLoop && loopStart) {
          inFileLoop = true
        }
      }

      // Detect batch processing pattern
      if (stmt.type === 'READ' && stmt.raw) {
        const raw = stmt.raw.toUpperCase()
        const hasBatchKeywords = batchKeywords.some(kw => raw.includes(kw))

        if (hasBatchKeywords) {
          patterns.push({
            type: 'batch_processing',
            confidence: 0.85,
            location: loopStart || stmt.location,
            description: 'Batch file processing workflow',
            parameters: this.extractBatchParameters(raw, dataElements)
          })
          inFileLoop = false
          loopStart = null
        }
      }
    }

    return patterns
  }

  /**
   * Calculate confidence score for interest calculation
   */
  private calculateInterestConfidence (
    raw: string,
    dataElements: DataElement[]
  ): number {
    let confidence = 0.5

    // Increase confidence for specific patterns
    if (raw.includes('PRINCIPAL') && raw.includes('RATE')) confidence += 0.2
    if (raw.includes('INTEREST')) confidence += 0.15
    if (raw.includes('*') || raw.includes('/')) confidence += 0.1
    if (raw.includes('365') || raw.includes('360')) confidence += 0.05 // Day count

    return Math.min(confidence, 1.0)
  }

  /**
   * Extract interest calculation parameters
   */
  private extractInterestParameters (
    raw: string,
    dataElements: DataElement[]
  ): Record<string, DataElement> {
    const parameters: Record<string, DataElement> = {}

    // Find relevant data elements
    for (const element of dataElements) {
      const name = element.name.toUpperCase()

      if (raw.includes(name)) {
        if (name.includes('PRINCIPAL') || name.includes('BALANCE')) {
          parameters.principal = element
        } else if (name.includes('RATE') || name.includes('INTEREST')) {
          parameters.rate = element
        } else if (name.includes('AMOUNT')) {
          parameters.amount = element
        }
      }
    }

    return parameters
  }

  /**
   * Extract loan amortization parameters
   */
  private extractLoanParameters (
    raw: string,
    dataElements: DataElement[]
  ): Record<string, DataElement> {
    const parameters: Record<string, DataElement> = {}

    for (const element of dataElements) {
      const name = element.name.toUpperCase()

      if (raw.includes(name)) {
        if (name.includes('LOAN') || name.includes('PRINCIPAL')) {
          parameters.principal = element
        } else if (name.includes('PAYMENT') || name.includes('INSTALLMENT')) {
          parameters.amount = element
        } else if (name.includes('TERM') || name.includes('PERIOD')) {
          parameters.term = element
        } else if (name.includes('RATE')) {
          parameters.rate = element
        }
      }
    }

    return parameters
  }

  /**
   * Extract validation parameters
   */
  private extractValidationParameters (
    raw: string,
    dataElements: DataElement[]
  ): Record<string, DataElement> {
    const parameters: Record<string, DataElement> = {}

    for (const element of dataElements) {
      const name = element.name.toUpperCase()

      if (raw.includes(name)) {
        if (name.includes('BALANCE') || name.includes('AMOUNT')) {
          parameters.amount = element
        } else if (name.includes('LIMIT')) {
          parameters.accountNumber = element
        }
      }
    }

    return parameters
  }

  /**
   * Extract batch processing parameters
   */
  private extractBatchParameters (
    raw: string,
    dataElements: DataElement[]
  ): Record<string, DataElement> {
    const parameters: Record<string, DataElement> = {}

    for (const element of dataElements) {
      const name = element.name.toUpperCase()

      if (raw.includes(name) && name.includes('RECORD')) {
        parameters.amount = element
      }
    }

    return parameters
  }
}

/**
 * Factory function to create pattern recognizer
 */
export function createPatternRecognizer (
  options?: PatternRecognizerOptions
): PatternRecognizer {
  return new PatternRecognizer(options)
}
