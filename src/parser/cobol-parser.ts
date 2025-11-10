/**
 * COBOL Parser Wrapper
 * Provides a unified interface for parsing COBOL source code
 */

import type {
  AST,
  ParseResult,
  ParseError,
  SourceMap,
  SourceLocation,
  IdentificationDivision,
  EnvironmentDivision,
  DataDivision,
  ProcedureDivision,
  DataElement,
  FileDefinition,
  Statement
} from '../mcp-server/types.js'
import type { CobolDialect, ParserOptions, ValidationResult } from './types.js'
import {
  createError,
  ErrorCode,
  createFallbackHandler
} from '../utils/error-handler.js'

/**
 * Main COBOL Parser class
 */
export class CobolParser {
  private options: ParserOptions

  constructor (options: ParserOptions = {}) {
    this.options = {
      dialect: options.dialect || 'COBOL-85',
      preserveComments: options.preserveComments ?? true,
      strictMode: options.strictMode ?? false
    }
  }

  /**
   * Parse COBOL source code and generate AST
   */
  async parse (source: string, dialect?: CobolDialect): Promise<ParseResult> {
    const startTime = Date.now()
    const errors: ParseError[] = []
    const warnings: string[] = []
    const effectiveDialect = dialect || this.options.dialect || 'COBOL-85'

    // Check cache first
    const { getGlobalCache } = await import('../utils/performance-cache.js')
    const cache = getGlobalCache()
    const cachedResult = cache.getAST(source)
    if (cachedResult) {
      return cachedResult
    }

    try {
      // Validate input
      if (!source || source.trim().length === 0) {
        const error = createError(
          ErrorCode.PARSE_EMPTY_SOURCE,
          'Empty COBOL source provided'
        )
        errors.push({
          message: error.userMessage,
          line: 0,
          column: 0,
          severity: 'error'
        })
        warnings.push(...error.suggestions)
        return this.createErrorResult(errors, warnings, source)
      }

      // Check for unsupported dialects
      const supportedDialects = [
        'IBM',
        'MicroFocus',
        'GnuCOBOL',
        'COBOL-85',
        'COBOL-2002'
      ]
      if (!supportedDialects.includes(effectiveDialect)) {
        const error = createError(
          ErrorCode.PARSE_UNSUPPORTED_DIALECT,
          `Dialect ${effectiveDialect} not fully supported`
        )
        warnings.push(error.userMessage)
        warnings.push(...error.suggestions)
      }

      // Create source map
      const sourceMap = this.createSourceMap(source)

      // Parse the COBOL source
      const ast = await this.parseCobolSource(
        source,
        effectiveDialect,
        errors,
        warnings
      )

      // Calculate complexity
      const complexity = this.calculateComplexity(ast)

      const parseTime = Date.now() - startTime

      // Check performance requirement (30 seconds for 10,000 lines)
      const lineCount = source.split('\n').length
      if (parseTime > 30000 && lineCount <= 10000) {
        const error = createError(
          ErrorCode.PARSE_TIMEOUT,
          `Parse time ${parseTime}ms exceeded target for ${lineCount} lines`
        )
        warnings.push(error.userMessage)
        warnings.push(...error.suggestions)
      }

      const result = {
        ast,
        errors,
        warnings,
        metadata: {
          programName: ast?.programId || 'UNKNOWN',
          lineCount,
          complexity
        }
      }

      // Cache the result
      cache.cacheAST(source, result)

      return result
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : String(error)
      const cobraError = createError(
        ErrorCode.PARSE_INVALID_SYNTAX,
        `Fatal parsing error: ${errorMessage}`
      )
      errors.push({
        message: cobraError.userMessage,
        line: 0,
        column: 0,
        severity: 'error'
      })
      warnings.push(...cobraError.suggestions)
      return this.createErrorResult(errors, warnings, source)
    }
  }

  /**
   * Validate COBOL source without full parsing
   */
  async validate (source: string): Promise<ValidationResult> {
    const errors: ParseError[] = []
    const warnings: string[] = []

    try {
      // Basic syntax validation
      this.validateBasicSyntax(source, errors, warnings)

      // Check for required divisions
      this.validateDivisions(source, errors, warnings)

      return {
        valid: errors.length === 0,
        errors,
        warnings
      }
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : String(error)
      errors.push({
        message: `Validation error: ${errorMessage}`,
        line: 0,
        column: 0,
        severity: 'error'
      })
      return {
        valid: false,
        errors,
        warnings
      }
    }
  }

  /**
   * Parse COBOL source into AST structure
   */
  private async parseCobolSource (
    source: string,
    dialect: CobolDialect,
    errors: ParseError[],
    warnings: string[]
  ): Promise<AST | null> {
    try {
      const lines = source.split('\n')
      const sourceMap = this.createSourceMap(source)

      // Extract divisions
      const identification = this.parseIdentificationDivision(lines, errors)
      const environment = this.parseEnvironmentDivision(lines, errors)
      const data = this.parseDataDivision(lines, errors)
      const procedure = this.parseProcedureDivision(lines, errors)

      // Get program ID
      const programId =
        identification?.programId || this.extractProgramId(source) || 'UNKNOWN'

      return {
        programId,
        divisions: {
          identification,
          environment,
          data,
          procedure
        },
        sourceMap
      }
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : String(error)
      errors.push({
        message: `AST generation failed: ${errorMessage}`,
        line: 0,
        column: 0,
        severity: 'error'
      })
      return null
    }
  }

  /**
   * Parse IDENTIFICATION DIVISION
   */
  private parseIdentificationDivision (
    lines: string[],
    errors: ParseError[]
  ): IdentificationDivision | undefined {
    try {
      const divisionStart = this.findDivisionStart(lines, 'IDENTIFICATION')
      if (divisionStart === -1) {
        return undefined
      }

      const divisionEnd = this.findNextDivision(lines, divisionStart)
      const divisionLines = lines.slice(divisionStart, divisionEnd)

      const programId = this.extractValue(divisionLines, 'PROGRAM-ID')
      const author = this.extractValue(divisionLines, 'AUTHOR')
      const dateWritten = this.extractValue(divisionLines, 'DATE-WRITTEN')

      return {
        programId: programId || 'UNKNOWN',
        author,
        dateWritten
      }
    } catch (error) {
      errors.push({
        message: `Failed to parse IDENTIFICATION DIVISION: ${error}`,
        line: 0,
        column: 0,
        severity: 'warning'
      })
      return undefined
    }
  }

  /**
   * Parse ENVIRONMENT DIVISION
   */
  private parseEnvironmentDivision (
    lines: string[],
    errors: ParseError[]
  ): EnvironmentDivision | undefined {
    try {
      const divisionStart = this.findDivisionStart(lines, 'ENVIRONMENT')
      if (divisionStart === -1) {
        return undefined
      }

      const divisionEnd = this.findNextDivision(lines, divisionStart)
      const divisionLines = lines.slice(divisionStart, divisionEnd)

      return {
        configuration: this.parseConfiguration(divisionLines),
        inputOutput: this.parseInputOutput(divisionLines)
      }
    } catch (error) {
      errors.push({
        message: `Failed to parse ENVIRONMENT DIVISION: ${error}`,
        line: 0,
        column: 0,
        severity: 'warning'
      })
      return undefined
    }
  }

  /**
   * Parse DATA DIVISION
   */
  private parseDataDivision (
    lines: string[],
    errors: ParseError[]
  ): DataDivision | undefined {
    try {
      const divisionStart = this.findDivisionStart(lines, 'DATA')
      if (divisionStart === -1) {
        return undefined
      }

      const divisionEnd = this.findNextDivision(lines, divisionStart)
      const divisionLines = lines.slice(divisionStart, divisionEnd)

      return {
        workingStorage: this.parseWorkingStorage(divisionLines, errors),
        fileSection: this.parseFileSection(divisionLines, errors)
      }
    } catch (error) {
      errors.push({
        message: `Failed to parse DATA DIVISION: ${error}`,
        line: 0,
        column: 0,
        severity: 'warning'
      })
      return undefined
    }
  }

  /**
   * Parse PROCEDURE DIVISION
   */
  private parseProcedureDivision (
    lines: string[],
    errors: ParseError[]
  ): ProcedureDivision | undefined {
    try {
      const divisionStart = this.findDivisionStart(lines, 'PROCEDURE')
      if (divisionStart === -1) {
        return undefined
      }

      const divisionLines = lines.slice(divisionStart)
      const statements = this.parseStatements(
        divisionLines,
        divisionStart,
        errors
      )

      return {
        statements
      }
    } catch (error) {
      errors.push({
        message: `Failed to parse PROCEDURE DIVISION: ${error}`,
        line: 0,
        column: 0,
        severity: 'warning'
      })
      return undefined
    }
  }

  /**
   * Parse WORKING-STORAGE SECTION
   */
  private parseWorkingStorage (
    lines: string[],
    errors: ParseError[]
  ): DataElement[] {
    const elements: DataElement[] = []
    const wsStart = lines.findIndex(line =>
      /WORKING-STORAGE\s+SECTION/i.test(line)
    )

    if (wsStart === -1) {
      return elements
    }

    const wsEnd = lines.findIndex(
      (line, idx) =>
        idx > wsStart &&
        /^\s*(FILE|LINKAGE|LOCAL-STORAGE)\s+SECTION/i.test(line)
    )
    const wsLines = lines.slice(wsStart + 1, wsEnd === -1 ? undefined : wsEnd)

    for (let i = 0; i < wsLines.length; i++) {
      const line = wsLines[i].trim()
      if (!line || line.startsWith('*')) continue

      const element = this.parseDataElement(line, errors)
      if (element) {
        elements.push(element)
      }
    }

    return elements
  }

  /**
   * Parse FILE SECTION
   */
  private parseFileSection (
    lines: string[],
    errors: ParseError[]
  ): FileDefinition[] {
    const files: FileDefinition[] = []
    const fsStart = lines.findIndex(line => /FILE\s+SECTION/i.test(line))

    if (fsStart === -1) {
      return files
    }

    const fsEnd = lines.findIndex(
      (line, idx) =>
        idx > fsStart &&
        /^\s*(WORKING-STORAGE|LINKAGE|LOCAL-STORAGE)\s+SECTION/i.test(line)
    )
    const fsLines = lines.slice(fsStart + 1, fsEnd === -1 ? undefined : fsEnd)

    for (const line of fsLines) {
      const fdMatch = line.match(/FD\s+(\S+)/i)
      if (fdMatch) {
        files.push({
          name: fdMatch[1],
          organization: 'SEQUENTIAL',
          accessMode: 'SEQUENTIAL'
        })
      }
    }

    return files
  }

  /**
   * Parse individual data element
   */
  private parseDataElement (
    line: string,
    errors: ParseError[]
  ): DataElement | null {
    try {
      // Match level number and name
      const match = line.match(/^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+(.+))?/i)
      if (!match) return null

      const level = parseInt(match[1], 10)
      const name = match[2]
      const rest = match[3] || ''

      // Extract PIC clause
      const picMatch = rest.match(/PIC(?:TURE)?\s+(?:IS\s+)?([A-Z0-9()V]+)/i)
      const picture = picMatch ? picMatch[1] : undefined

      // Extract VALUE clause
      const valueMatch = rest.match(/VALUE\s+(?:IS\s+)?([^.\s]+)/i)
      const value = valueMatch ? valueMatch[1].replace(/['"]/g, '') : undefined

      // Extract USAGE clause
      const usageMatch = rest.match(/USAGE\s+(?:IS\s+)?([A-Z-]+)/i)
      const usage = usageMatch ? usageMatch[1] : undefined

      return {
        name,
        level,
        picture,
        value,
        usage
      }
    } catch (error) {
      return null
    }
  }

  /**
   * Parse statements from PROCEDURE DIVISION
   */
  private parseStatements (
    lines: string[],
    startLine: number,
    errors: ParseError[]
  ): Statement[] {
    const statements: Statement[] = []

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim()
      if (!line || line.startsWith('*')) continue

      const statement = this.parseStatement(line, startLine + i)
      if (statement) {
        statements.push(statement)
      }
    }

    return statements
  }

  /**
   * Parse individual statement
   */
  private parseStatement (line: string, lineNumber: number): Statement | null {
    // Identify statement type
    const keywords = [
      'MOVE',
      'ADD',
      'SUBTRACT',
      'MULTIPLY',
      'DIVIDE',
      'COMPUTE',
      'IF',
      'ELSE',
      'PERFORM',
      'CALL',
      'ACCEPT',
      'DISPLAY',
      'READ',
      'WRITE',
      'OPEN',
      'CLOSE',
      'STOP',
      'EXIT'
    ]

    for (const keyword of keywords) {
      if (line.toUpperCase().startsWith(keyword)) {
        return {
          type: keyword,
          location: {
            startLine: lineNumber,
            endLine: lineNumber,
            startColumn: 0,
            endColumn: line.length
          },
          raw: line
        }
      }
    }

    return null
  }

  /**
   * Helper methods
   */
  private findDivisionStart (lines: string[], divisionName: string): number {
    return lines.findIndex(line =>
      new RegExp(`${divisionName}\\s+DIVISION`, 'i').test(line)
    )
  }

  private findNextDivision (lines: string[], startFrom: number): number {
    const divisionPattern =
      /(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION/i
    for (let i = startFrom + 1; i < lines.length; i++) {
      if (divisionPattern.test(lines[i])) {
        return i
      }
    }
    return lines.length
  }

  private extractValue (lines: string[], keyword: string): string | undefined {
    for (const line of lines) {
      const match = line.match(
        new RegExp(`${keyword}\\.?\\s+(.+?)(?:\\.|$)`, 'i')
      )
      if (match) {
        return match[1].trim()
      }
    }
    return undefined
  }

  private extractProgramId (source: string): string | null {
    const match = source.match(/PROGRAM-ID\.\s+([A-Z0-9-]+)/i)
    return match ? match[1] : null
  }

  private parseConfiguration (lines: string[]): any {
    return {}
  }

  private parseInputOutput (lines: string[]): any {
    return {}
  }

  private createSourceMap (source: string): SourceMap {
    const lines = source.split('\n')
    const lineMap = new Map<number, string>()
    lines.forEach((line, index) => {
      lineMap.set(index + 1, line)
    })
    return { lines: lineMap }
  }

  private calculateComplexity (ast: AST | null): number {
    if (!ast) return 0

    let complexity = 1 // Base complexity

    // Add complexity for data elements
    const dataElements = ast.divisions.data?.workingStorage?.length || 0
    complexity += Math.floor(dataElements / 10)

    // Add complexity for statements
    const statements = ast.divisions.procedure?.statements.length || 0
    complexity += Math.floor(statements / 5)

    return complexity
  }

  private validateBasicSyntax (
    source: string,
    errors: ParseError[],
    warnings: string[]
  ): void {
    const lines = source.split('\n')

    // Check for basic COBOL structure
    if (!source.match(/IDENTIFICATION\s+DIVISION/i)) {
      const error = createError(
        ErrorCode.PARSE_MISSING_DIVISION,
        'Missing IDENTIFICATION DIVISION'
      )
      errors.push({
        message: error.userMessage,
        line: 0,
        column: 0,
        severity: 'error'
      })
      warnings.push(...error.suggestions)
    }

    // Check for PROGRAM-ID
    if (!source.match(/PROGRAM-ID/i)) {
      const error = createError(
        ErrorCode.PARSE_MISSING_DIVISION,
        'Missing PROGRAM-ID'
      )
      errors.push({
        message: error.userMessage,
        line: 0,
        column: 0,
        severity: 'error'
      })
      warnings.push(...error.suggestions)
    }
  }

  private validateDivisions (
    source: string,
    errors: ParseError[],
    warnings: string[]
  ): void {
    const requiredDivisions = ['IDENTIFICATION']
    const optionalDivisions = ['ENVIRONMENT', 'DATA', 'PROCEDURE']

    for (const division of requiredDivisions) {
      if (!source.match(new RegExp(`${division}\\s+DIVISION`, 'i'))) {
        const error = createError(
          ErrorCode.PARSE_MISSING_DIVISION,
          `Missing required ${division} DIVISION`
        )
        errors.push({
          message: error.userMessage,
          line: 0,
          column: 0,
          severity: 'error'
        })
      }
    }

    for (const division of optionalDivisions) {
      if (!source.match(new RegExp(`${division}\\s+DIVISION`, 'i'))) {
        warnings.push(
          `⚠️  Missing optional ${division} DIVISION - analysis may be incomplete`
        )
      }
    }
  }

  private createErrorResult (
    errors: ParseError[],
    warnings: string[],
    source: string
  ): ParseResult {
    return {
      ast: null,
      errors,
      warnings,
      metadata: {
        programName: 'UNKNOWN',
        lineCount: source.split('\n').length,
        complexity: 0
      }
    }
  }
}

/**
 * Factory function to create parser instance
 */
export function createParser (options?: ParserOptions): CobolParser {
  return new CobolParser(options)
}
