/**
 * Centralized Error Handling Utilities
 * Provides consistent error handling and user feedback across COBRA
 */

export enum ErrorCode {
  // Parser errors
  PARSE_EMPTY_SOURCE = 'PARSE_EMPTY_SOURCE',
  PARSE_INVALID_SYNTAX = 'PARSE_INVALID_SYNTAX',
  PARSE_MISSING_DIVISION = 'PARSE_MISSING_DIVISION',
  PARSE_TIMEOUT = 'PARSE_TIMEOUT',
  PARSE_UNSUPPORTED_DIALECT = 'PARSE_UNSUPPORTED_DIALECT',

  // Analyzer errors
  ANALYZE_INVALID_AST = 'ANALYZE_INVALID_AST',
  ANALYZE_NO_PATTERNS = 'ANALYZE_NO_PATTERNS',
  ANALYZE_INCOMPLETE_DATA = 'ANALYZE_INCOMPLETE_DATA',

  // LLM errors
  LLM_API_ERROR = 'LLM_API_ERROR',
  LLM_RATE_LIMIT = 'LLM_RATE_LIMIT',
  LLM_INVALID_RESPONSE = 'LLM_INVALID_RESPONSE',
  LLM_TIMEOUT = 'LLM_TIMEOUT',

  // Generator errors
  GEN_INVALID_INPUT = 'GEN_INVALID_INPUT',
  GEN_TEMPLATE_ERROR = 'GEN_TEMPLATE_ERROR',
  GEN_VALIDATION_FAILED = 'GEN_VALIDATION_FAILED',

  // System errors
  SYSTEM_OUT_OF_MEMORY = 'SYSTEM_OUT_OF_MEMORY',
  SYSTEM_TIMEOUT = 'SYSTEM_TIMEOUT',
  SYSTEM_UNKNOWN = 'SYSTEM_UNKNOWN'
}

export interface CobraError {
  code: ErrorCode
  message: string
  userMessage: string
  details?: any
  recoverable: boolean
  suggestions: string[]
  timestamp: Date
}

/**
 * Create a user-friendly error with recovery suggestions
 */
export function createError (
  code: ErrorCode,
  message: string,
  details?: any
): CobraError {
  const errorInfo = getErrorInfo(code)

  return {
    code,
    message,
    userMessage: errorInfo.userMessage,
    details,
    recoverable: errorInfo.recoverable,
    suggestions: errorInfo.suggestions,
    timestamp: new Date()
  }
}

/**
 * Get error information including user-friendly messages and suggestions
 */
function getErrorInfo (code: ErrorCode): {
  userMessage: string
  recoverable: boolean
  suggestions: string[]
} {
  switch (code) {
    case ErrorCode.PARSE_EMPTY_SOURCE:
      return {
        userMessage: 'No COBOL source code provided',
        recoverable: true,
        suggestions: [
          'Provide valid COBOL source code',
          'Check that the file is not empty',
          'Ensure the content was copied correctly'
        ]
      }

    case ErrorCode.PARSE_INVALID_SYNTAX:
      return {
        userMessage: 'COBOL syntax error detected',
        recoverable: true,
        suggestions: [
          'Check for missing periods at end of statements',
          'Verify division headers are correctly formatted',
          'Ensure proper indentation and column alignment',
          'Review COBOL syntax rules for your dialect'
        ]
      }

    case ErrorCode.PARSE_MISSING_DIVISION:
      return {
        userMessage: 'Required COBOL division is missing',
        recoverable: true,
        suggestions: [
          'Add IDENTIFICATION DIVISION at the start',
          'Include PROGRAM-ID statement',
          'Verify all required divisions are present',
          'Check for typos in division names'
        ]
      }

    case ErrorCode.PARSE_TIMEOUT:
      return {
        userMessage: 'Parsing took too long to complete',
        recoverable: true,
        suggestions: [
          'Try parsing a smaller section of code',
          'Remove comments and unused sections',
          'Split large programs into smaller modules',
          'Contact support if the file is under 10,000 lines'
        ]
      }

    case ErrorCode.PARSE_UNSUPPORTED_DIALECT:
      return {
        userMessage: 'COBOL dialect not fully supported',
        recoverable: true,
        suggestions: [
          'Try using COBOL-85 or COBOL-2002 standard syntax',
          'Remove vendor-specific extensions',
          'Check documentation for supported dialects',
          'Contact support for dialect-specific features'
        ]
      }

    case ErrorCode.ANALYZE_INVALID_AST:
      return {
        userMessage: 'Unable to analyze the parsed COBOL structure',
        recoverable: true,
        suggestions: [
          'Ensure the COBOL code parsed successfully',
          'Check for complete division structures',
          'Verify the AST contains procedure division',
          'Try re-parsing the source code'
        ]
      }

    case ErrorCode.ANALYZE_NO_PATTERNS:
      return {
        userMessage: 'No recognizable banking patterns found',
        recoverable: true,
        suggestions: [
          'This may be a utility or non-banking program',
          'Check if the code contains business logic',
          'Verify the procedure division has statements',
          'Manual review may be needed for this code'
        ]
      }

    case ErrorCode.ANALYZE_INCOMPLETE_DATA:
      return {
        userMessage: 'Analysis completed with missing information',
        recoverable: true,
        suggestions: [
          'Some sections may not have been analyzed',
          'Review the warnings for details',
          'Consider providing copybook definitions',
          'Manual review recommended for completeness'
        ]
      }

    case ErrorCode.LLM_API_ERROR:
      return {
        userMessage: 'AI service temporarily unavailable',
        recoverable: true,
        suggestions: [
          'Check your API key configuration',
          'Verify internet connectivity',
          'Try again in a few moments',
          'Check LLM provider status page'
        ]
      }

    case ErrorCode.LLM_RATE_LIMIT:
      return {
        userMessage: 'AI service rate limit reached',
        recoverable: true,
        suggestions: [
          'Wait a few minutes before retrying',
          'Consider upgrading your API plan',
          'Use caching to reduce API calls',
          'Process fewer files simultaneously'
        ]
      }

    case ErrorCode.LLM_INVALID_RESPONSE:
      return {
        userMessage: 'AI service returned unexpected response',
        recoverable: true,
        suggestions: [
          'Try the request again',
          'Check if the input is too large',
          'Simplify the COBOL code if possible',
          'Contact support if issue persists'
        ]
      }

    case ErrorCode.LLM_TIMEOUT:
      return {
        userMessage: 'AI service request timed out',
        recoverable: true,
        suggestions: [
          'Try again with a smaller code sample',
          'Check your internet connection',
          'Increase timeout settings if available',
          'Contact support if issue persists'
        ]
      }

    case ErrorCode.GEN_INVALID_INPUT:
      return {
        userMessage: 'Invalid input for code generation',
        recoverable: true,
        suggestions: [
          'Ensure analysis completed successfully',
          'Check that required fields are present',
          'Verify the input format is correct',
          'Review the analysis results'
        ]
      }

    case ErrorCode.GEN_TEMPLATE_ERROR:
      return {
        userMessage: 'Code generation template error',
        recoverable: false,
        suggestions: [
          'This is an internal error',
          'Try a different generation approach',
          'Contact support with error details',
          'Check for system updates'
        ]
      }

    case ErrorCode.GEN_VALIDATION_FAILED:
      return {
        userMessage: 'Generated code failed validation',
        recoverable: true,
        suggestions: [
          'Review the generated code manually',
          'Check for unsupported COBOL features',
          'Simplify the source logic if possible',
          'Report validation errors to support'
        ]
      }

    case ErrorCode.SYSTEM_OUT_OF_MEMORY:
      return {
        userMessage: 'System ran out of memory',
        recoverable: true,
        suggestions: [
          'Try processing a smaller file',
          'Close other applications',
          'Restart the COBRA service',
          'Contact support for large file processing'
        ]
      }

    case ErrorCode.SYSTEM_TIMEOUT:
      return {
        userMessage: 'Operation timed out',
        recoverable: true,
        suggestions: [
          'Try again with a smaller input',
          'Check system resources',
          'Increase timeout if configurable',
          'Contact support if issue persists'
        ]
      }

    case ErrorCode.SYSTEM_UNKNOWN:
    default:
      return {
        userMessage: 'An unexpected error occurred',
        recoverable: true,
        suggestions: [
          'Try the operation again',
          'Check system logs for details',
          'Restart the service if needed',
          'Contact support with error details'
        ]
      }
  }
}

/**
 * Format error for display to users
 */
export function formatErrorForUser (error: CobraError): string {
  let message = `❌ ${error.userMessage}\n\n`

  if (error.details) {
    message += `Details: ${JSON.stringify(error.details, null, 2)}\n\n`
  }

  if (error.suggestions.length > 0) {
    message += 'Suggestions:\n'
    error.suggestions.forEach((suggestion, index) => {
      message += `  ${index + 1}. ${suggestion}\n`
    })
  }

  return message
}

/**
 * Format error for logging
 */
export function formatErrorForLog (error: CobraError): string {
  return JSON.stringify(
    {
      code: error.code,
      message: error.message,
      userMessage: error.userMessage,
      details: error.details,
      recoverable: error.recoverable,
      timestamp: error.timestamp.toISOString()
    },
    null,
    2
  )
}

/**
 * Wrap async operations with error handling
 */
export async function withErrorHandling<T> (
  operation: () => Promise<T>,
  errorCode: ErrorCode,
  context?: string
): Promise<{ success: true; data: T } | { success: false; error: CobraError }> {
  try {
    const data = await operation()
    return { success: true, data }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err)
    const error = createError(errorCode, message, {
      context,
      originalError: err
    })
    return { success: false, error }
  }
}

/**
 * Create a graceful degradation handler
 */
export function createFallbackHandler<T> (
  fallbackValue: T,
  warningMessage: string
): (error: any) => T {
  return (error: any) => {
    console.warn(`⚠️  ${warningMessage}:`, error)
    return fallbackValue
  }
}
