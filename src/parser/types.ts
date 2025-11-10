/**
 * COBOL Parser Type Definitions
 */

export type CobolDialect =
  | 'IBM'
  | 'MicroFocus'
  | 'GnuCOBOL'
  | 'COBOL-85'
  | 'COBOL-2002'

export interface ParserOptions {
  dialect?: CobolDialect
  preserveComments?: boolean
  strictMode?: boolean
}

export interface ValidationResult {
  valid: boolean
  errors: ParseError[]
  warnings: string[]
}

export interface ParseError {
  message: string
  line: number
  column: number
  severity: 'error' | 'warning'
}

export interface Copybook {
  name: string
  content: string
  location?: string
}
