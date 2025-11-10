/**
 * Lambda Function Generator
 * Generates AWS Lambda functions from COBOL business logic
 */

import type {
  LogicAnalysis,
  BusinessRule,
  DataElement,
  BankingPattern
} from '../mcp-server/types.js'
import type { GeneratorOptions, CodeArtifact } from './types.js'

export interface LambdaGeneratorOptions extends GeneratorOptions {
  runtime?: 'nodejs20.x' | 'python3.12'
  includeAuditLogging?: boolean
  includeInputValidation?: boolean
}

/**
 * Generate Lambda function code from COBOL analysis
 */
export function generateLambdaFunction (
  analysis: LogicAnalysis,
  options: LambdaGeneratorOptions = {}
): CodeArtifact[] {
  const {
    language = 'typescript',
    runtime = 'nodejs20.x',
    includeAuditLogging = true,
    includeInputValidation = true,
    includeComments = true
  } = options

  const artifacts: CodeArtifact[] = []

  // Generate main handler
  if (language === 'typescript') {
    artifacts.push(
      generateTypeScriptHandler(analysis, {
        includeAuditLogging,
        includeInputValidation,
        includeComments
      })
    )
    artifacts.push(generateTypeScriptTypes(analysis))
    artifacts.push(generateTypeScriptValidation(analysis))
    artifacts.push(generateTypeScriptPackageJson())
    artifacts.push(generateTypeScriptTsConfig())
  } else {
    artifacts.push(
      generatePythonHandler(analysis, {
        includeAuditLogging,
        includeInputValidation,
        includeComments
      })
    )
    artifacts.push(generatePythonRequirements())
  }

  return artifacts
}

/**
 * Generate TypeScript Lambda handler
 */
function generateTypeScriptHandler (
  analysis: LogicAnalysis,
  options: {
    includeAuditLogging: boolean
    includeInputValidation: boolean
    includeComments: boolean
  }
): CodeArtifact {
  const { businessRules, patterns, entryPoints } = analysis
  const entryPoint = entryPoints[0] || { name: 'MAIN', parameters: [] }

  let code = `/**
 * Lambda Handler - Generated from COBOL
 * Original Program: ${entryPoint.name}
 */

import { APIGatewayProxyEvent, APIGatewayProxyResult } from 'aws-lambda'
import { validateInput, ValidationError } from './validation.js'
import { InputData, OutputData } from './types.js'
`

  if (options.includeAuditLogging) {
    code += `import { auditLog } from './audit.js'\n`
  }

  code += `
export const handler = async (
  event: APIGatewayProxyEvent
): Promise<APIGatewayProxyResult> => {
  const startTime = Date.now()
  const requestId = event.requestContext.requestId
  
  try {
`

  if (options.includeInputValidation) {
    code += `    // Input validation (from COBOL data validation)
    const input = JSON.parse(event.body || '{}') as InputData
    const validationResult = validateInput(input)
    
    if (!validationResult.valid) {
      return {
        statusCode: 400,
        body: JSON.stringify({
          error: 'Validation failed',
          details: validationResult.errors
        })
      }
    }
`
  } else {
    code += `    const input = JSON.parse(event.body || '{}') as InputData
`
  }

  code += `
    // Business logic (translated from COBOL)
    const result = await executeBusinessLogic(input)
`

  if (options.includeAuditLogging) {
    code += `
    // Audit logging (compliance requirement)
    await auditLog({
      requestId,
      timestamp: new Date().toISOString(),
      userId: event.requestContext.identity?.cognitoIdentityId || 'anonymous',
      operation: '${entryPoint.name}',
      input,
      output: result,
      duration: Date.now() - startTime,
      statusCode: 200
    })
`
  }

  code += `
    return {
      statusCode: 200,
      headers: {
        'Content-Type': 'application/json',
        'X-Request-Id': requestId
      },
      body: JSON.stringify(result)
    }
  } catch (error) {
    console.error('Error processing request:', error)
`

  if (options.includeAuditLogging) {
    code += `
    await auditLog({
      requestId,
      timestamp: new Date().toISOString(),
      userId: event.requestContext.identity?.cognitoIdentityId || 'anonymous',
      operation: '${entryPoint.name}',
      input: event.body,
      error: error instanceof Error ? error.message : 'Unknown error',
      duration: Date.now() - startTime,
      statusCode: 500
    })
`
  }

  code += `
    return {
      statusCode: error instanceof ValidationError ? 400 : 500,
      headers: {
        'Content-Type': 'application/json',
        'X-Request-Id': requestId
      },
      body: JSON.stringify({
        error: error instanceof Error ? error.message : 'Internal server error',
        requestId
      })
    }
  }
}

/**
 * Execute business logic translated from COBOL
 */
async function executeBusinessLogic(input: InputData): Promise<OutputData> {
`

  // Generate business logic from rules
  for (const rule of businessRules) {
    if (options.includeComments) {
      code += `  // ${rule.description}\n`
      code += `  // Source: Lines ${rule.cobolSource.startLine}-${rule.cobolSource.endLine}\n`
    }

    if (rule.type === 'calculation' && rule.formula) {
      code += `  ${generateCalculationCode(rule)}\n`
    } else if (rule.type === 'validation') {
      code += `  ${generateValidationCode(rule)}\n`
    } else if (rule.type === 'decision' && rule.conditions) {
      code += `  ${generateDecisionCode(rule)}\n`
    }
  }

  // Generate pattern-specific logic
  for (const pattern of patterns) {
    if (options.includeComments) {
      code += `\n  // Banking Pattern: ${pattern.type} (confidence: ${pattern.confidence})\n`
      code += `  // ${pattern.description}\n`
    }

    if (pattern.type === 'interest_calculation') {
      code += generateInterestCalculation(pattern)
    } else if (pattern.type === 'transaction_posting') {
      code += generateTransactionPosting(pattern)
    }
  }

  code += `
  return {
    success: true,
    message: 'Operation completed successfully',
    data: {} // TODO: Map output data from COBOL
  }
}
`

  if (options.includeAuditLogging) {
    code += `
/**
 * Audit logging function
 */
async function auditLog(entry: any): Promise<void> {
  // TODO: Implement audit logging to CloudWatch or DynamoDB
  console.log('AUDIT:', JSON.stringify(entry))
}
`
  }

  return {
    filename: 'handler.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate calculation code from business rule
 */
function generateCalculationCode (rule: BusinessRule): string {
  if (!rule.formula) return ''

  const outputVar = rule.outputs[0]?.name || 'result'
  const formula = rule.formula.replace(/\s+/g, ' ')

  return `const ${outputVar} = ${formula};`
}

/**
 * Generate validation code from business rule
 */
function generateValidationCode (rule: BusinessRule): string {
  const conditions = rule.conditions || []
  if (conditions.length === 0) return ''

  let code = ''
  for (const condition of conditions) {
    code += `  if (${condition.expression}) {\n`
    code += `    ${condition.action}\n`
    code += `  }\n`
  }

  return code
}

/**
 * Generate decision code from business rule
 */
function generateDecisionCode (rule: BusinessRule): string {
  const conditions = rule.conditions || []
  if (conditions.length === 0) return ''

  let code = ''
  for (let i = 0; i < conditions.length; i++) {
    const condition = conditions[i]
    const keyword = i === 0 ? 'if' : 'else if'

    code += `  ${keyword} (${condition.expression}) {\n`
    code += `    ${condition.action}\n`
    code += `  }\n`
  }

  return code
}

/**
 * Generate interest calculation logic
 */
function generateInterestCalculation (pattern: BankingPattern): string {
  const principal = pattern.parameters.principal?.name || 'principal'
  const rate = pattern.parameters.rate?.name || 'rate'
  const term = pattern.parameters.term?.name || 'term'

  return `
  // Interest calculation: I = P * R * T
  const interest = input.${principal} * input.${rate} * input.${term}
  const totalAmount = input.${principal} + interest
`
}

/**
 * Generate transaction posting logic
 */
function generateTransactionPosting (pattern: BankingPattern): string {
  const amount = pattern.parameters.amount?.name || 'amount'
  const accountNumber =
    pattern.parameters.accountNumber?.name || 'accountNumber'

  return `
  // Transaction posting
  const transaction = {
    accountNumber: input.${accountNumber},
    amount: input.${amount},
    timestamp: new Date().toISOString(),
    status: 'POSTED'
  }
`
}

/**
 * Generate TypeScript type definitions
 */
function generateTypeScriptTypes (analysis: LogicAnalysis): CodeArtifact {
  const { dataStructures, entryPoints } = analysis
  const entryPoint = entryPoints[0] || { name: 'MAIN', parameters: [] }

  let code = `/**
 * Type Definitions - Generated from COBOL
 */

export interface InputData {
`

  // Generate input types from entry point parameters
  for (const param of entryPoint.parameters) {
    const tsType = cobolTypeToTypeScript(param.picture || '')
    code += `  ${param.name}: ${tsType}\n`
  }

  // Add fields from data structures
  for (const struct of dataStructures) {
    for (const field of struct.fields) {
      const tsType = cobolTypeToTypeScript(field.picture || '')
      code += `  ${field.name}?: ${tsType}\n`
    }
  }

  code += `}

export interface OutputData {
  success: boolean
  message: string
  data: any
}

export interface AuditLogEntry {
  requestId: string
  timestamp: string
  userId: string
  operation: string
  input: any
  output?: any
  error?: string
  duration: number
  statusCode: number
}
`

  return {
    filename: 'types.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Convert COBOL PIC clause to TypeScript type
 */
function cobolTypeToTypeScript (picture: string): string {
  if (!picture) return 'any'

  // Numeric types
  if (/^9+$/.test(picture) || /^9+V9+$/.test(picture)) {
    return 'number'
  }

  // Alphanumeric types
  if (/^X+$/.test(picture)) {
    return 'string'
  }

  // Signed numeric
  if (/^S9+/.test(picture)) {
    return 'number'
  }

  return 'string' // Default to string
}

/**
 * Generate validation module
 */
function generateTypeScriptValidation (analysis: LogicAnalysis): CodeArtifact {
  const { dataStructures, entryPoints } = analysis
  const entryPoint = entryPoints[0] || { name: 'MAIN', parameters: [] }

  let code = `/**
 * Input Validation - Generated from COBOL PIC clauses
 */

import { InputData } from './types.js'

export class ValidationError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'ValidationError'
  }
}

export interface ValidationResult {
  valid: boolean
  errors: string[]
}

export function validateInput(input: InputData): ValidationResult {
  const errors: string[] = []

`

  // Generate validation for each parameter
  for (const param of entryPoint.parameters) {
    code += generateFieldValidation(param)
  }

  code += `
  return {
    valid: errors.length === 0,
    errors
  }
}

`

  // Generate individual field validators
  for (const param of entryPoint.parameters) {
    code += generateFieldValidator(param)
  }

  return {
    filename: 'validation.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate validation code for a field
 */
function generateFieldValidation (field: DataElement): string {
  const { name, picture } = field

  if (!picture) {
    return `  // No validation for ${name} (no PIC clause)\n`
  }

  let code = `  // Validate ${name} (PIC ${picture})\n`

  // Required field check
  code += `  if (input.${name} === undefined || input.${name} === null) {\n`
  code += `    errors.push('${name} is required')\n`
  code += `  } else {\n`

  // Type-specific validation
  if (/^9+$/.test(picture) || /^9+V9+$/.test(picture)) {
    // Numeric validation
    const length = picture.replace('V', '').length
    code += `    if (typeof input.${name} !== 'number') {\n`
    code += `      errors.push('${name} must be a number')\n`
    code += `    }\n`
  } else if (/^X+$/.test(picture)) {
    // String validation
    const maxLength = picture.length
    code += `    if (typeof input.${name} !== 'string') {\n`
    code += `      errors.push('${name} must be a string')\n`
    code += `    } else if (input.${name}.length > ${maxLength}) {\n`
    code += `      errors.push('${name} must not exceed ${maxLength} characters')\n`
    code += `    }\n`
  }

  code += `  }\n\n`

  return code
}

/**
 * Generate individual field validator function
 */
function generateFieldValidator (field: DataElement): string {
  const { name, picture } = field

  if (!picture) return ''

  return `function validate${capitalize(name)}(value: any): boolean {
  // TODO: Implement detailed validation for ${name} (PIC ${picture})
  return true
}

`
}

/**
 * Capitalize first letter
 */
function capitalize (str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1)
}

/**
 * Generate package.json for TypeScript Lambda
 */
function generateTypeScriptPackageJson (): CodeArtifact {
  const content = {
    name: 'cobol-lambda-function',
    version: '1.0.0',
    description: 'Lambda function generated from COBOL',
    type: 'module',
    main: 'dist/handler.js',
    scripts: {
      build: 'tsc',
      test: 'jest',
      package: 'npm run build && zip -r function.zip dist node_modules'
    },
    dependencies: {
      '@aws-sdk/client-cloudwatch-logs': '^3.0.0',
      '@aws-sdk/client-dynamodb': '^3.0.0'
    },
    devDependencies: {
      '@types/aws-lambda': '^8.10.0',
      '@types/node': '^20.0.0',
      typescript: '^5.0.0',
      jest: '^29.0.0',
      '@types/jest': '^29.0.0'
    }
  }

  return {
    filename: 'package.json',
    content: JSON.stringify(content, null, 2),
    language: 'json'
  }
}

/**
 * Generate tsconfig.json for TypeScript Lambda
 */
function generateTypeScriptTsConfig (): CodeArtifact {
  const content = {
    compilerOptions: {
      target: 'ES2022',
      module: 'ES2022',
      moduleResolution: 'node',
      lib: ['ES2022'],
      outDir: './dist',
      rootDir: './src',
      strict: true,
      esModuleInterop: true,
      skipLibCheck: true,
      forceConsistentCasingInFileNames: true,
      resolveJsonModule: true
    },
    include: ['src/**/*'],
    exclude: ['node_modules', 'dist']
  }

  return {
    filename: 'tsconfig.json',
    content: JSON.stringify(content, null, 2),
    language: 'json'
  }
}

/**
 * Generate Python Lambda handler
 */
function generatePythonHandler (
  analysis: LogicAnalysis,
  options: {
    includeAuditLogging: boolean
    includeInputValidation: boolean
    includeComments: boolean
  }
): CodeArtifact {
  const { businessRules, patterns, entryPoints } = analysis
  const entryPoint = entryPoints[0] || { name: 'MAIN', parameters: [] }

  let code = `"""
Lambda Handler - Generated from COBOL
Original Program: ${entryPoint.name}
"""

import json
import time
from typing import Dict, Any
from datetime import datetime
`

  if (options.includeInputValidation) {
    code += `from validation import validate_input, ValidationError\n`
  }

  if (options.includeAuditLogging) {
    code += `from audit import audit_log\n`
  }

  code += `

def lambda_handler(event: Dict[str, Any], context: Any) -> Dict[str, Any]:
    """Main Lambda handler function"""
    start_time = time.time()
    request_id = context.request_id
    
    try:
`

  if (options.includeInputValidation) {
    code += `        # Input validation (from COBOL data validation)
        body = json.loads(event.get('body', '{}'))
        validation_result = validate_input(body)
        
        if not validation_result['valid']:
            return {
                'statusCode': 400,
                'body': json.dumps({
                    'error': 'Validation failed',
                    'details': validation_result['errors']
                })
            }
        
        input_data = body
`
  } else {
    code += `        input_data = json.loads(event.get('body', '{}'))
`
  }

  code += `
        # Business logic (translated from COBOL)
        result = execute_business_logic(input_data)
`

  if (options.includeAuditLogging) {
    code += `
        # Audit logging (compliance requirement)
        audit_log({
            'request_id': request_id,
            'timestamp': datetime.utcnow().isoformat(),
            'user_id': event.get('requestContext', {}).get('identity', {}).get('cognitoIdentityId', 'anonymous'),
            'operation': '${entryPoint.name}',
            'input': input_data,
            'output': result,
            'duration': time.time() - start_time,
            'status_code': 200
        })
`
  }

  code += `
        return {
            'statusCode': 200,
            'headers': {
                'Content-Type': 'application/json',
                'X-Request-Id': request_id
            },
            'body': json.dumps(result)
        }
    except Exception as error:
        print(f'Error processing request: {error}')
`

  if (options.includeAuditLogging) {
    code += `
        audit_log({
            'request_id': request_id,
            'timestamp': datetime.utcnow().isoformat(),
            'user_id': event.get('requestContext', {}).get('identity', {}).get('cognitoIdentityId', 'anonymous'),
            'operation': '${entryPoint.name}',
            'input': event.get('body'),
            'error': str(error),
            'duration': time.time() - start_time,
            'status_code': 500
        })
`
  }

  code += `
        return {
            'statusCode': 400 if isinstance(error, ValidationError) else 500,
            'headers': {
                'Content-Type': 'application/json',
                'X-Request-Id': request_id
            },
            'body': json.dumps({
                'error': str(error),
                'request_id': request_id
            })
        }


def execute_business_logic(input_data: Dict[str, Any]) -> Dict[str, Any]:
    """Execute business logic translated from COBOL"""
`

  // Generate business logic from rules
  for (const rule of businessRules) {
    if (options.includeComments) {
      code += `    # ${rule.description}\n`
      code += `    # Source: Lines ${rule.cobolSource.startLine}-${rule.cobolSource.endLine}\n`
    }
  }

  // Generate pattern-specific logic
  for (const pattern of patterns) {
    if (options.includeComments) {
      code += `\n    # Banking Pattern: ${pattern.type} (confidence: ${pattern.confidence})\n`
      code += `    # ${pattern.description}\n`
    }

    if (pattern.type === 'interest_calculation') {
      const principal = pattern.parameters.principal?.name || 'principal'
      const rate = pattern.parameters.rate?.name || 'rate'
      const term = pattern.parameters.term?.name || 'term'

      code += `    # Interest calculation: I = P * R * T
    interest = input_data['${principal}'] * input_data['${rate}'] * input_data['${term}']
    total_amount = input_data['${principal}'] + interest
`
    }
  }

  code += `
    return {
        'success': True,
        'message': 'Operation completed successfully',
        'data': {}  # TODO: Map output data from COBOL
    }
`

  return {
    filename: 'handler.py',
    content: code,
    language: 'python'
  }
}

/**
 * Generate Python requirements.txt
 */
function generatePythonRequirements (): CodeArtifact {
  const content = `boto3>=1.28.0
aws-lambda-powertools>=2.0.0
`

  return {
    filename: 'requirements.txt',
    content,
    language: 'text'
  }
}

/**
 * Map COBOL return codes to HTTP status codes
 */
export function mapReturnCodeToHttpStatus (returnCode: number): number {
  // COBOL return code mapping
  const mapping: Record<number, number> = {
    0: 200, // Success
    4: 400, // Validation error
    8: 500, // Processing error
    12: 500, // Severe error
    16: 503 // Service unavailable
  }

  return mapping[returnCode] || 500
}
