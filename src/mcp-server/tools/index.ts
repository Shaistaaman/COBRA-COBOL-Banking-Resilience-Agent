/**
 * MCP Tool Handlers for COBRA
 */

import type {
  ParseResult,
  LogicAnalysis,
  SpecDocument,
  AWSArtifacts,
  ModernizationPlan
} from '../types.js'
import { createParser } from '../../parser/cobol-parser.js'

/**
 * Parse COBOL source code and generate AST
 * Implements timeout handling for large files (30 second limit)
 * Returns structured ParseResult with AST, errors, and metadata
 */
export async function parseCobol (source: string): Promise<ParseResult> {
  const parser = createParser({
    dialect: 'COBOL-85',
    preserveComments: true,
    strictMode: false
  })

  // Implement timeout handling (30 seconds as per requirements)
  const timeoutMs = 30000
  const timeoutPromise = new Promise<ParseResult>((_, reject) => {
    setTimeout(() => {
      reject(
        new Error(
          `Parse timeout: COBOL parsing exceeded ${timeoutMs / 1000} seconds`
        )
      )
    }, timeoutMs)
  })

  try {
    // Race between parsing and timeout
    const parsePromise = parser.parse(source)
    const result = await Promise.race([parsePromise, timeoutPromise])

    // Add additional complexity metrics
    if (result.ast) {
      result.metadata.complexity = calculateProgramComplexity(result.ast)
    }

    return result
  } catch (error) {
    // Handle timeout or parsing errors
    const errorMessage = error instanceof Error ? error.message : String(error)

    return {
      ast: null,
      errors: [
        {
          message: errorMessage,
          line: 0,
          column: 0,
          severity: 'error'
        }
      ],
      warnings: [],
      metadata: {
        programName: 'UNKNOWN',
        lineCount: source.split('\n').length,
        complexity: 0
      }
    }
  }
}

/**
 * Calculate program complexity metrics
 * Considers: cyclomatic complexity, data elements, statements, nesting depth
 */
function calculateProgramComplexity (ast: any): number {
  let complexity = 1 // Base complexity

  // Data complexity
  const dataElements = ast.divisions.data?.workingStorage?.length || 0
  complexity += Math.floor(dataElements / 10)

  // Statement complexity
  const statements = ast.divisions.procedure?.statements || []
  complexity += Math.floor(statements.length / 5)

  // Cyclomatic complexity (count decision points)
  let decisionPoints = 0
  for (const stmt of statements) {
    if (stmt.type === 'IF' || stmt.type === 'PERFORM') {
      decisionPoints++
    }
  }
  complexity += decisionPoints

  // File I/O complexity
  const files = ast.divisions.data?.fileSection?.length || 0
  complexity += files * 2

  return complexity
}

/**
 * Analyze COBOL logic and extract business rules
 * Returns LogicAnalysis with business rules, patterns, and dependencies
 * Includes confidence scores for pattern recognition
 */
export async function analyzeLogic (ast: any): Promise<LogicAnalysis> {
  // Validate input
  if (!ast || !ast.programId) {
    throw new Error('Invalid AST: missing programId')
  }

  // Import analyzer dynamically to avoid circular dependencies
  const { createLogicAnalyzer } = await import('../../analyzer/index.js')

  // Create analyzer with pattern recognition enabled
  const analyzer = createLogicAnalyzer({
    patternRecognition: {
      enableAllPatterns: true,
      confidenceThreshold: 0.6
    }
  })

  try {
    // Perform comprehensive analysis
    const analysis = analyzer.analyze(ast, ast.programId)

    // Ensure all patterns have confidence scores
    const patternsWithConfidence = analysis.patterns.map(pattern => ({
      ...pattern,
      confidence: pattern.confidence || calculatePatternConfidence(pattern, ast)
    }))

    return {
      ...analysis,
      patterns: patternsWithConfidence
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    console.error('Logic analysis failed:', errorMessage)

    // Return minimal analysis on error
    return {
      businessRules: [],
      dataStructures: [],
      dependencies: [],
      entryPoints: [
        {
          name: ast.programId,
          parameters: [],
          returnType: undefined
        }
      ],
      patterns: []
    }
  }
}

/**
 * Calculate confidence score for pattern recognition
 * Based on keyword matches, structure, and context
 */
function calculatePatternConfidence (pattern: any, ast: any): number {
  let confidence = 0.5 // Base confidence

  // Check for specific keywords in procedure division
  const statements = ast.divisions.procedure?.statements || []
  const statementText = statements
    .map((s: any) => s.raw || '')
    .join(' ')
    .toUpperCase()

  // Pattern-specific confidence adjustments
  switch (pattern.type) {
    case 'interest_calculation':
      if (
        statementText.includes('INTEREST') ||
        statementText.includes('RATE') ||
        statementText.includes('COMPUTE')
      ) {
        confidence += 0.3
      }
      break

    case 'transaction_posting':
      if (
        statementText.includes('TRANSACTION') ||
        statementText.includes('POST') ||
        statementText.includes('WRITE')
      ) {
        confidence += 0.3
      }
      break

    case 'batch_processing':
      if (
        statementText.includes('PERFORM') ||
        statementText.includes('READ') ||
        statementText.includes('UNTIL')
      ) {
        confidence += 0.3
      }
      break

    case 'validation':
      if (
        statementText.includes('IF') ||
        statementText.includes('VALIDATE') ||
        statementText.includes('CHECK')
      ) {
        confidence += 0.3
      }
      break

    case 'loan_amortization':
      if (
        statementText.includes('LOAN') ||
        statementText.includes('PAYMENT') ||
        statementText.includes('PRINCIPAL')
      ) {
        confidence += 0.3
      }
      break

    case 'account_reconciliation':
      if (
        statementText.includes('BALANCE') ||
        statementText.includes('RECONCILE') ||
        statementText.includes('MATCH')
      ) {
        confidence += 0.3
      }
      break
  }

  // Cap confidence at 0.95 (never 100% certain)
  return Math.min(confidence, 0.95)
}

/**
 * Generate Kiro spec documents from COBOL analysis
 * Creates requirements.md with EARS-compliant acceptance criteria
 * Generates design.md with AWS architecture recommendations
 * Produces tasks.md with implementation steps
 */
export async function generateSpec (
  analysis: LogicAnalysis
): Promise<SpecDocument> {
  // Validate input
  if (!analysis || !analysis.businessRules) {
    throw new Error('Invalid analysis: missing business rules')
  }

  try {
    // Generate each spec document
    const requirements = await generateRequirements(analysis)
    const design = await generateDesign(analysis)
    const tasks = await generateTasks(analysis)

    return {
      requirements,
      design,
      tasks
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    console.error('Spec generation failed:', errorMessage)

    // Return minimal spec documents on error
    return {
      requirements: generateFallbackRequirements(analysis),
      design: generateFallbackDesign(analysis),
      tasks: generateFallbackTasks(analysis)
    }
  }
}

/**
 * Generate requirements.md with EARS-compliant acceptance criteria
 */
async function generateRequirements (analysis: LogicAnalysis): Promise<string> {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'
  const businessRules = analysis.businessRules.slice(0, 10)
  const patterns = analysis.patterns

  let requirements = `# Requirements Document

## Introduction

This document specifies the requirements for modernizing the COBOL program ${programName} to AWS cloud-native architecture. The system preserves existing business logic while exposing functionality through modern REST APIs.

## Glossary

- **Modernization System**: The AWS-based system that replicates COBOL business logic
- **Lambda Function**: AWS serverless compute service that executes business logic
- **API Gateway**: AWS service that exposes Lambda functions as REST APIs
- **COBOL Program**: The legacy ${programName} program being modernized

## Requirements

`

  // Generate requirements from business rules
  businessRules.forEach((rule, idx) => {
    const reqNum = idx + 1
    requirements += `### Requirement ${reqNum}

**User Story:** As a system user, I want to ${rule.description.toLowerCase()}, so that business operations continue seamlessly after modernization.

#### Acceptance Criteria

`

    // Generate EARS-compliant acceptance criteria
    requirements += `1. WHEN the system receives input data matching ${rule.inputs
      .map(i => i.name)
      .join(
        ', '
      )}, THE Modernization System SHALL execute the business logic defined in lines ${
      rule.cobolSource.startLine
    }-${rule.cobolSource.endLine}\n`

    if (rule.conditions && rule.conditions.length > 0) {
      requirements += `2. IF ${rule.conditions[0].expression}, THEN THE Modernization System SHALL ${rule.conditions[0].action}\n`
    } else {
      requirements += `2. THE Modernization System SHALL validate all input data according to COBOL data definitions\n`
    }

    if (rule.outputs && rule.outputs.length > 0) {
      requirements += `3. THE Modernization System SHALL produce output data ${rule.outputs
        .map(o => o.name)
        .join(', ')} matching the COBOL program's output format\n`
    } else {
      requirements += `3. THE Modernization System SHALL return results in JSON format with appropriate HTTP status codes\n`
    }

    requirements += `4. THE Modernization System SHALL complete processing within 3 seconds for 95% of requests\n`
    requirements += `5. THE Modernization System SHALL log all transactions for audit compliance\n\n`
  })

  // Add pattern-specific requirements
  if (patterns.length > 0) {
    const patternReqNum = businessRules.length + 1
    requirements += `### Requirement ${patternReqNum}

**User Story:** As a banking operations manager, I want the system to preserve ${patterns[0].type.replace(
      /_/g,
      ' '
    )} patterns, so that critical banking operations remain accurate.

#### Acceptance Criteria

1. THE Modernization System SHALL implement ${patterns[0].type.replace(
      /_/g,
      ' '
    )} logic identical to the COBOL program
2. THE Modernization System SHALL produce results within 0.01% accuracy of COBOL calculations
3. THE Modernization System SHALL handle edge cases including zero values, negative amounts, and boundary conditions
4. THE Modernization System SHALL validate all calculations against test cases derived from COBOL program
5. THE Modernization System SHALL provide detailed error messages for invalid inputs

`
  }

  return requirements
}

/**
 * Generate design.md with AWS architecture recommendations
 */
async function generateDesign (analysis: LogicAnalysis): Promise<string> {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'
  const patterns = analysis.patterns
  const dependencies = analysis.dependencies

  let design = `# Design Document

## Overview

This document describes the AWS cloud-native architecture for modernizing the COBOL program ${programName}. The design uses serverless Lambda functions to replicate business logic, API Gateway for REST endpoints, and AWS CDK for infrastructure as code.

## Architecture

### High-Level Architecture

\`\`\`mermaid
graph TB
    Client[API Client] --> APIGW[API Gateway]
    APIGW --> Lambda[Lambda Function]
    Lambda --> CW[CloudWatch Logs]
    Lambda --> DDB[DynamoDB]
    APIGW --> WAF[AWS WAF]
\`\`\`

### Components

1. **API Gateway**: REST API endpoint exposing COBOL business logic
2. **Lambda Function**: Serverless compute executing modernized business rules
3. **CloudWatch**: Logging and monitoring for audit compliance
4. **DynamoDB**: Optional data storage for state management
5. **AWS WAF**: Web application firewall for security

## Components and Interfaces

### Lambda Function

**Runtime**: Node.js 20.x (TypeScript)

**Handler**: \`index.handler\`

**Input Schema**:
\`\`\`json
{
${analysis.businessRules[0]?.inputs
  .map(i => `  "${i.name}": "string"`)
  .join(',\n')}
}
\`\`\`

**Output Schema**:
\`\`\`json
{
${analysis.businessRules[0]?.outputs
  .map(o => `  "${o.name}": "string"`)
  .join(',\n')}
}
\`\`\`

### API Gateway Configuration

**Endpoint**: \`POST /api/${programName.toLowerCase()}\`

**Authentication**: AWS IAM or API Key

**Throttling**: 1000 requests per second

**CORS**: Enabled for specified origins

## Data Models

### Input Data Model

\`\`\`typescript
interface ${programName}Input {
${analysis.businessRules[0]?.inputs
  .map(i => `  ${i.name}: string; // ${i.picture || 'N/A'}`)
  .join('\n')}
}
\`\`\`

### Output Data Model

\`\`\`typescript
interface ${programName}Output {
${analysis.businessRules[0]?.outputs
  .map(o => `  ${o.name}: string; // ${o.picture || 'N/A'}`)
  .join('\n')}
  status: 'success' | 'error';
  message?: string;
}
\`\`\`

## Business Logic Implementation

`

  // Add pattern-specific implementation notes
  patterns.forEach(pattern => {
    design += `### ${pattern.type.replace(/_/g, ' ').toUpperCase()}

**Description**: ${pattern.description}

**Confidence**: ${(pattern.confidence * 100).toFixed(0)}%

**Implementation Approach**: Translate COBOL ${
      pattern.type
    } logic to TypeScript using modern JavaScript math libraries. Preserve calculation precision using decimal.js for financial accuracy.

`
  })

  design += `## Error Handling

1. **Input Validation**: Validate all inputs against COBOL PIC clauses
2. **Business Rule Violations**: Return HTTP 400 with detailed error messages
3. **System Errors**: Return HTTP 500 with generic error message, log details
4. **Timeout Handling**: Set Lambda timeout to 30 seconds, return 504 if exceeded

## Testing Strategy

1. **Unit Tests**: Test each business rule independently with Jest
2. **Integration Tests**: Test Lambda function with API Gateway locally using SAM
3. **Comparison Tests**: Compare outputs with COBOL program for identical inputs
4. **Load Tests**: Verify performance under expected load using Artillery

## Security Considerations

1. **Authentication**: AWS IAM roles for service-to-service, API keys for external clients
2. **Encryption**: TLS 1.2+ for data in transit, KMS encryption for sensitive data at rest
3. **Input Validation**: Strict validation against expected formats to prevent injection
4. **Audit Logging**: Log all requests with timestamp, user, input, output, and duration
5. **WAF Rules**: Protect against SQL injection, XSS, and common vulnerabilities

`

  return design
}

/**
 * Generate tasks.md with implementation steps
 */
async function generateTasks (analysis: LogicAnalysis): Promise<string> {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  let tasks = `# Implementation Plan

## Overview

This plan outlines the steps to modernize the COBOL program ${programName} to AWS cloud-native architecture.

## Tasks

- [ ] 1. Set up project structure
  - Create TypeScript project with AWS CDK
  - Configure build and deployment scripts
  - Set up testing framework (Jest)
  - _Requirements: All_

- [ ] 2. Implement Lambda function
`

  // Generate tasks for each business rule
  analysis.businessRules.slice(0, 5).forEach((rule, idx) => {
    tasks += `- [ ] 2.${idx + 1} Implement ${rule.type} logic: ${
      rule.description
    }
  - Translate COBOL logic from lines ${rule.cobolSource.startLine}-${
      rule.cobolSource.endLine
    }
  - Add input validation for ${rule.inputs.map(i => i.name).join(', ')}
  - Generate output ${rule.outputs.map(o => o.name).join(', ')}
  - _Requirements: ${idx + 1}_

`
  })

  tasks += `- [ ]* 2.${
    analysis.businessRules.length + 1
  } Write unit tests for business logic
  - Test each business rule with valid inputs
  - Test error handling with invalid inputs
  - Compare outputs with COBOL program results
  - _Requirements: All_

- [ ] 3. Create API Gateway configuration
  - Define REST API with OpenAPI specification
  - Configure request/response models
  - Set up authentication (IAM or API Key)
  - Enable CORS for allowed origins
  - _Requirements: All_

- [ ] 4. Implement AWS CDK infrastructure
  - Create CDK Stack with Lambda function
  - Configure API Gateway integration
  - Set up CloudWatch log groups
  - Add IAM roles with least-privilege permissions
  - Configure AWS WAF rules
  - _Requirements: All_

- [ ] 5. Add monitoring and logging
  - Implement structured logging in Lambda
  - Create CloudWatch dashboard
  - Set up alarms for errors and latency
  - Configure audit logging for compliance
  - _Requirements: All_

- [ ]* 6. Deploy and validate
  - Deploy to AWS test environment
  - Run integration tests against deployed API
  - Perform load testing
  - Compare results with COBOL program
  - _Requirements: All_

`

  return tasks
}

/**
 * Fallback requirements generation (no LLM)
 */
function generateFallbackRequirements (analysis: LogicAnalysis): string {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  return `# Requirements Document

## Introduction

Requirements for modernizing COBOL program ${programName}.

## Glossary

- **Modernization System**: AWS-based system replicating COBOL logic
- **Lambda Function**: AWS serverless compute service

## Requirements

### Requirement 1

**User Story:** As a user, I want to modernize ${programName}, so that the system runs on AWS.

#### Acceptance Criteria

1. THE Modernization System SHALL replicate all business logic from ${programName}
2. THE Modernization System SHALL process requests within 3 seconds
3. THE Modernization System SHALL log all transactions
4. THE Modernization System SHALL validate all inputs
5. THE Modernization System SHALL return results in JSON format

`
}

/**
 * Fallback design generation (no LLM)
 */
function generateFallbackDesign (analysis: LogicAnalysis): string {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  return `# Design Document

## Overview

AWS architecture for ${programName} modernization using Lambda and API Gateway.

## Architecture

- API Gateway: REST endpoint
- Lambda Function: Business logic execution
- CloudWatch: Logging and monitoring

## Components and Interfaces

### Lambda Function
- Runtime: Node.js 20.x
- Handler: index.handler
- Timeout: 30 seconds

### API Gateway
- Endpoint: POST /api/${programName.toLowerCase()}
- Authentication: AWS IAM

## Error Handling

- Input validation
- Error logging
- HTTP status codes

## Testing Strategy

- Unit tests
- Integration tests
- Load tests

`
}

/**
 * Fallback tasks generation (no LLM)
 */
function generateFallbackTasks (analysis: LogicAnalysis): string {
  return `# Implementation Plan

## Tasks

- [ ] 1. Set up project structure
- [ ] 2. Implement Lambda function
- [ ] 3. Create API Gateway configuration
- [ ] 4. Implement AWS CDK infrastructure
- [ ] 5. Add monitoring and logging
- [ ]* 6. Deploy and validate

`
}

/**
 * Generate AWS Lambda and infrastructure code
 * Accepts spec document or logic analysis as input
 * Invokes Lambda, API Gateway, and CDK generators
 * Returns downloadable artifact links
 */
export async function generateAWSCode (
  input: SpecDocument | LogicAnalysis
): Promise<AWSArtifacts> {
  try {
    // Determine if input is SpecDocument or LogicAnalysis
    let analysis: LogicAnalysis

    if ('requirements' in input && 'design' in input && 'tasks' in input) {
      // Input is SpecDocument - we need to extract analysis from it
      // For now, we'll create a minimal analysis structure
      // In a real implementation, this would parse the spec documents
      analysis = {
        businessRules: [],
        dataStructures: [],
        dependencies: [],
        entryPoints: [
          {
            name: 'MAIN',
            parameters: []
          }
        ],
        patterns: []
      }
    } else {
      // Input is LogicAnalysis
      analysis = input as LogicAnalysis
    }

    // Import generator modules
    const { generateLambdaFunction } = await import(
      '../../generator/lambda-generator.js'
    )
    const { generateAPIGatewayConfig } = await import(
      '../../generator/api-gateway-generator.js'
    )
    const { generateCDKStack } = await import(
      '../../generator/cdk-generator.js'
    )

    // Generate Lambda functions
    const lambdaArtifacts = generateLambdaFunction(analysis, {
      language: 'typescript',
      runtime: 'nodejs20.x',
      includeAuditLogging: true,
      includeInputValidation: true,
      includeComments: true
    })

    // Generate API Gateway configuration
    const apiGatewayArtifacts = generateAPIGatewayConfig(analysis, {
      apiName: 'CobolModernizationAPI',
      stageName: 'prod',
      authenticationType: 'IAM',
      enableCors: true,
      enableThrottling: true
    })

    // Generate CDK infrastructure
    const cdkArtifacts = generateCDKStack(analysis, {
      stackName: 'CobolModernizationStack',
      enableWAF: true,
      enableEncryption: true,
      enableMonitoring: true
    })

    // Combine all artifacts
    const allArtifacts = [
      ...lambdaArtifacts,
      ...apiGatewayArtifacts,
      ...cdkArtifacts
    ]

    // Extract Lambda functions for the response
    const lambdaFunctions = lambdaArtifacts
      .filter(a => a.filename.endsWith('.ts') || a.filename.endsWith('.py'))
      .map(artifact => ({
        name: analysis.entryPoints[0]?.name || 'MAIN',
        runtime: 'nodejs20.x' as const,
        handler: 'handler.handler',
        code: artifact.content,
        environment: {
          NODE_ENV: 'production',
          LOG_LEVEL: 'info'
        }
      }))

    // Extract API Gateway configuration
    const apiGatewayConfig =
      apiGatewayArtifacts.find(a => a.filename === 'openapi.json')?.content ||
      '# API Gateway Configuration\n\nGenerated OpenAPI specification'

    // Extract CDK stack
    const cdkStack =
      cdkArtifacts.find(a => a.filename === 'lib/stack.ts')?.content ||
      '# CDK Stack\n\nGenerated CDK infrastructure code'

    // Package artifacts for download
    const { packageArtifacts, createArtifactPackages } = await import(
      '../../generator/artifact-packager.js'
    )

    // Create packaged artifacts
    const packages = createArtifactPackages(allArtifacts, analysis)

    // Generate README with deployment instructions
    const readme = generateDeploymentReadme(analysis, allArtifacts, packages)

    return {
      lambdaFunctions,
      apiGateway: apiGatewayConfig,
      cdkStack,
      readme,
      packages
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    console.error('AWS code generation failed:', errorMessage)

    // Return minimal artifacts on error
    return {
      lambdaFunctions: [],
      apiGateway: `# API Gateway Configuration\n\nError: ${errorMessage}`,
      cdkStack: `# CDK Stack\n\nError: ${errorMessage}`,
      readme: `# Deployment Instructions\n\nError generating code: ${errorMessage}`
    }
  }
}

/**
 * Generate README with deployment instructions
 */
function generateDeploymentReadme (
  analysis: LogicAnalysis,
  artifacts: any[],
  packages?: Record<string, any>
): string {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  let readme = `# ${programName} - AWS Deployment

## Overview

This package contains AWS infrastructure code generated from COBOL program ${programName}.

## Generated Artifacts

`

  // List all generated files
  const filesByType: Record<string, string[]> = {
    Lambda: [],
    'API Gateway': [],
    CDK: [],
    Configuration: []
  }

  for (const artifact of artifacts) {
    if (artifact.filename.includes('handler')) {
      filesByType.Lambda.push(artifact.filename)
    } else if (artifact.filename.includes('api-gateway')) {
      filesByType['API Gateway'].push(artifact.filename)
    } else if (
      artifact.filename.includes('lib/') ||
      artifact.filename.includes('bin/')
    ) {
      filesByType.CDK.push(artifact.filename)
    } else {
      filesByType.Configuration.push(artifact.filename)
    }
  }

  for (const [type, files] of Object.entries(filesByType)) {
    if (files.length > 0) {
      readme += `### ${type}\n\n`
      for (const file of files) {
        readme += `- \`${file}\`\n`
      }
      readme += `\n`
    }
  }

  // Add download links if packages are available
  if (packages) {
    readme += `## Download Packages

`
    if (packages.complete) {
      readme += `### Complete Package (Recommended)
- **File**: \`${packages.complete.filename}\`
- **Size**: ${(packages.complete.size / 1024).toFixed(2)} KB
- **Files**: ${packages.complete.files.length}
- **Download**: ${packages.complete.downloadUrl || 'Available via API'}

Includes: Lambda functions, CDK infrastructure, API Gateway config, deployment scripts, and documentation.

`
    }

    if (packages.lambda) {
      readme += `### Lambda Functions Only
- **File**: \`${packages.lambda.filename}\`
- **Size**: ${(packages.lambda.size / 1024).toFixed(2)} KB
- **Files**: ${packages.lambda.files.length}
- **Download**: ${packages.lambda.downloadUrl || 'Available via API'}

`
    }

    if (packages.cdk) {
      readme += `### CDK Infrastructure Only
- **File**: \`${packages.cdk.filename}\`
- **Size**: ${(packages.cdk.size / 1024).toFixed(2)} KB
- **Files**: ${packages.cdk.files.length}
- **Download**: ${packages.cdk.downloadUrl || 'Available via API'}

`
    }

    if (packages.apiGateway) {
      readme += `### API Gateway Configuration Only
- **File**: \`${packages.apiGateway.filename}\`
- **Size**: ${(packages.apiGateway.size / 1024).toFixed(2)} KB
- **Files**: ${packages.apiGateway.files.length}
- **Download**: ${packages.apiGateway.downloadUrl || 'Available via API'}

`
    }
  }

  readme += `## Prerequisites

- Node.js 20.x or later
- AWS CLI configured with appropriate credentials
- AWS CDK CLI installed (\`npm install -g aws-cdk\`)
- AWS account with permissions to create Lambda, API Gateway, IAM, and CloudWatch resources

## Project Structure

\`\`\`
.
├── lambda/                 # Lambda function code
│   ├── handler.ts         # Main Lambda handler
│   ├── types.ts           # TypeScript type definitions
│   ├── validation.ts      # Input validation
│   └── package.json       # Lambda dependencies
├── lib/                   # CDK infrastructure code
│   └── stack.ts           # Main CDK stack
├── bin/                   # CDK app entry point
│   └── app.ts             # CDK app
├── openapi.json           # API Gateway OpenAPI spec
├── cdk.json               # CDK configuration
└── README.md              # This file
\`\`\`

## Installation

### 1. Install Lambda Dependencies

\`\`\`bash
cd lambda
npm install
npm run build
cd ..
\`\`\`

### 2. Install CDK Dependencies

\`\`\`bash
npm install
\`\`\`

## Deployment

### Option 1: Deploy with AWS CDK (Recommended)

\`\`\`bash
# Bootstrap CDK (first time only)
cdk bootstrap

# Review changes
cdk diff

# Deploy infrastructure
cdk deploy

# Note the API Gateway URL from the output
\`\`\`

### Option 2: Deploy Lambda Manually

\`\`\`bash
# Package Lambda function
cd lambda
npm run package

# Deploy using AWS CLI
aws lambda create-function \\
  --function-name ${programName} \\
  --runtime nodejs20.x \\
  --role arn:aws:iam::ACCOUNT_ID:role/lambda-execution-role \\
  --handler handler.handler \\
  --zip-file fileb://function.zip

# Create API Gateway (see AWS Console or use AWS CLI)
\`\`\`

## Testing

### Local Testing

\`\`\`bash
# Install AWS SAM CLI
# https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/install-sam-cli.html

# Test Lambda locally
cd lambda
sam local invoke -e test-event.json
\`\`\`

### Integration Testing

\`\`\`bash
# Test deployed API
curl -X POST https://YOUR_API_ID.execute-api.REGION.amazonaws.com/prod/${programName.toLowerCase()} \\
  -H "Content-Type: application/json" \\
  -d '{"key": "value"}'
\`\`\`

## Monitoring

### CloudWatch Logs

\`\`\`bash
# View Lambda logs
aws logs tail /aws/lambda/${programName} --follow

# View API Gateway logs
aws logs tail /aws/apigateway/CobolModernizationAPI --follow
\`\`\`

### CloudWatch Dashboard

Access the CloudWatch dashboard in the AWS Console:
- Dashboard name: \`CobolModernizationDashboard\`
- Metrics: Invocations, Errors, Duration, Latency

## Cost Estimation

### AWS Free Tier (First 12 Months)
- Lambda: 1M requests/month + 400,000 GB-seconds
- API Gateway: 1M requests/month
- CloudWatch: 10 custom metrics + 5GB logs

### Beyond Free Tier
- Lambda: $0.20 per 1M requests + $0.0000166667 per GB-second
- API Gateway: $3.50 per million requests
- CloudWatch: $0.30 per custom metric + $0.50 per GB logs

**Estimated monthly cost for 100K requests**: $5-10

## Security

### Authentication
- API Gateway uses AWS IAM authentication by default
- Configure Cognito for user-based authentication if needed

### Encryption
- All data in transit uses TLS 1.2+
- Lambda environment variables can be encrypted with KMS
- S3 buckets use server-side encryption

### Audit Logging
- All API requests are logged to CloudWatch
- Audit logs include: timestamp, user ID, input, output, duration

## Troubleshooting

### Lambda Timeout
- Default timeout: 30 seconds
- Increase in CDK stack if needed: \`timeout: cdk.Duration.seconds(60)\`

### Permission Errors
- Ensure Lambda execution role has necessary permissions
- Check CloudWatch Logs for detailed error messages

### API Gateway 403 Errors
- Verify IAM authentication is configured correctly
- Check API Gateway resource policies

## Cleanup

To remove all deployed resources:

\`\`\`bash
cdk destroy
\`\`\`

## Support

For issues or questions:
- Check CloudWatch Logs for error details
- Review AWS CDK documentation: https://docs.aws.amazon.com/cdk/
- Review Lambda documentation: https://docs.aws.amazon.com/lambda/

## Generated by COBRA

This infrastructure was automatically generated from COBOL source code using COBRA (COBOL Banking Resilience Agent).

**Original COBOL Program**: ${programName}
**Generation Date**: ${new Date().toISOString()}
**Business Rules**: ${analysis.businessRules.length}
**Banking Patterns**: ${analysis.patterns.length}
`

  return readme
}

/**
 * Suggest modernization strategies based on COBOL analysis
 * Implements coupling analysis to identify low-risk modules
 * Generates prioritized modernization recommendations
 * Suggests appropriate AWS services for each COBOL component
 * Provides de-risking strategies with rationale
 */
export async function suggestModernization (
  analysis: LogicAnalysis
): Promise<ModernizationPlan> {
  // Validate input
  if (!analysis) {
    throw new Error('Invalid analysis: analysis object is required')
  }

  try {
    // Perform coupling analysis
    const couplingScores = analyzeCoupling(analysis)

    // Generate prioritized modules
    const prioritizedModules = prioritizeModules(analysis, couplingScores)

    // Generate AWS service recommendations
    const recommendations = generateRecommendations(
      analysis,
      prioritizedModules
    )

    // Generate de-risking strategies
    const deRiskingStrategies = generateDeRiskingStrategies(
      analysis,
      prioritizedModules
    )

    return {
      recommendations,
      prioritizedModules,
      deRiskingStrategies
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    console.error('Modernization suggestion failed:', errorMessage)

    // Return minimal plan on error
    return {
      recommendations: [
        {
          module: 'MAIN',
          awsService: 'Lambda',
          rationale: 'Default serverless approach',
          estimatedEffort: 'Medium'
        }
      ],
      prioritizedModules: [
        {
          name: 'MAIN',
          priority: 1,
          coupling: 0,
          complexity: 5,
          businessValue: 'High'
        }
      ],
      deRiskingStrategies: [
        {
          approach: 'Incremental Migration',
          description: 'Migrate one module at a time',
          risks: ['Integration issues'],
          mitigations: ['Comprehensive testing']
        }
      ]
    }
  }
}

/**
 * Analyze coupling between modules
 * Lower coupling = lower risk for modernization
 */
function analyzeCoupling (analysis: LogicAnalysis): Map<string, number> {
  const couplingScores = new Map<string, number>()
  const programName = analysis.entryPoints[0]?.name || 'MAIN'

  // Base coupling score
  let coupling = 0

  // Increase coupling for each dependency
  coupling += analysis.dependencies.length * 2

  // Increase coupling for shared data structures
  coupling += analysis.dataStructures.length

  // Decrease coupling if module is self-contained
  if (analysis.dependencies.length === 0) {
    coupling = Math.max(0, coupling - 5)
  }

  couplingScores.set(programName, coupling)

  return couplingScores
}

/**
 * Prioritize modules for modernization
 * Priority based on: low coupling, high business value, manageable complexity
 */
function prioritizeModules (
  analysis: LogicAnalysis,
  couplingScores: Map<string, number>
): any[] {
  const modules = []
  const programName = analysis.entryPoints[0]?.name || 'MAIN'
  const coupling = couplingScores.get(programName) || 0

  // Calculate complexity score
  const complexity = calculateModuleComplexity(analysis)

  // Determine business value based on patterns
  const businessValue = determineBusinessValue(analysis)

  // Calculate priority (lower is better)
  // Priority = coupling + (complexity / 2) - businessValueScore
  const businessValueScore =
    businessValue === 'High' ? 10 : businessValue === 'Medium' ? 5 : 0
  const priority = coupling + Math.floor(complexity / 2) - businessValueScore

  modules.push({
    name: programName,
    priority: Math.max(1, priority),
    coupling,
    complexity,
    businessValue
  })

  // Sort by priority (ascending)
  return modules.sort((a, b) => a.priority - b.priority)
}

/**
 * Calculate module complexity
 */
function calculateModuleComplexity (analysis: LogicAnalysis): number {
  let complexity = 0

  // Business rules complexity
  complexity += analysis.businessRules.length * 2

  // Data structures complexity
  complexity += analysis.dataStructures.length

  // Pattern complexity
  complexity += analysis.patterns.length * 3

  return complexity
}

/**
 * Determine business value based on patterns
 */
function determineBusinessValue (analysis: LogicAnalysis): string {
  // High value patterns
  const highValuePatterns = [
    'interest_calculation',
    'loan_amortization',
    'transaction_posting'
  ]

  // Check if any high-value patterns exist
  const hasHighValue = analysis.patterns.some(p =>
    highValuePatterns.includes(p.type)
  )

  if (hasHighValue) {
    return 'High'
  }

  // Medium value if has business rules
  if (analysis.businessRules.length > 5) {
    return 'Medium'
  }

  return 'Low'
}

/**
 * Generate AWS service recommendations
 */
function generateRecommendations (
  analysis: LogicAnalysis,
  prioritizedModules: any[]
): any[] {
  const recommendations = []

  for (const module of prioritizedModules) {
    // Determine appropriate AWS service based on patterns and complexity
    const awsService = selectAWSService(analysis, module)
    const rationale = generateRationale(analysis, module, awsService)
    const estimatedEffort = estimateEffort(module)

    recommendations.push({
      module: module.name,
      awsService,
      rationale,
      estimatedEffort
    })
  }

  return recommendations
}

/**
 * Select appropriate AWS service for module
 */
function selectAWSService (analysis: LogicAnalysis, module: any): string {
  // Check for batch processing patterns
  const hasBatchProcessing = analysis.patterns.some(
    p => p.type === 'batch_processing'
  )
  if (hasBatchProcessing) {
    return 'Step Functions + Lambda'
  }

  // Check for high complexity
  if (module.complexity > 20) {
    return 'ECS Fargate'
  }

  // Check for file dependencies
  const hasFileDependencies = analysis.dependencies.some(d => d.type === 'file')
  if (hasFileDependencies) {
    return 'Lambda + S3'
  }

  // Check for database dependencies
  const hasDatabaseDependencies = analysis.dependencies.some(
    d => d.type === 'database'
  )
  if (hasDatabaseDependencies) {
    return 'Lambda + DynamoDB'
  }

  // Default to Lambda for simple modules
  return 'Lambda + API Gateway'
}

/**
 * Generate rationale for AWS service selection
 */
function generateRationale (
  analysis: LogicAnalysis,
  module: any,
  awsService: string
): string {
  const reasons = []

  if (awsService.includes('Lambda')) {
    reasons.push('Serverless architecture reduces operational overhead')
    reasons.push('Pay-per-use pricing model is cost-effective')
  }

  if (awsService.includes('Step Functions')) {
    reasons.push('Orchestrates complex batch workflows')
    reasons.push('Provides visual workflow monitoring')
  }

  if (awsService.includes('ECS')) {
    reasons.push('Handles complex long-running processes')
    reasons.push('Provides more control over runtime environment')
  }

  if (awsService.includes('S3')) {
    reasons.push('Replaces file-based I/O with cloud storage')
    reasons.push('Provides durability and scalability')
  }

  if (awsService.includes('DynamoDB')) {
    reasons.push('Replaces database access with managed NoSQL')
    reasons.push('Provides low-latency data access')
  }

  if (module.coupling < 5) {
    reasons.push(
      'Low coupling makes this module ideal for independent deployment'
    )
  }

  if (module.businessValue === 'High') {
    reasons.push('High business value justifies modernization investment')
  }

  return reasons.join('. ')
}

/**
 * Estimate effort for modernization
 */
function estimateEffort (module: any): string {
  const score = module.complexity + module.coupling * 2

  if (score < 10) {
    return 'Low (1-2 weeks)'
  } else if (score < 25) {
    return 'Medium (3-6 weeks)'
  } else {
    return 'High (2-3 months)'
  }
}

/**
 * Generate de-risking strategies
 */
function generateDeRiskingStrategies (
  analysis: LogicAnalysis,
  prioritizedModules: any[]
): any[] {
  const strategies = []

  // Strategy 1: Incremental Migration
  strategies.push({
    approach: 'Incremental Migration',
    description:
      'Migrate modules one at a time, starting with lowest coupling and complexity. Keep COBOL system running in parallel during transition.',
    risks: [
      'Data synchronization issues between COBOL and AWS',
      'Integration complexity with remaining COBOL modules',
      'Extended timeline for complete migration'
    ],
    mitigations: [
      'Implement dual-write pattern for data consistency',
      'Use message queues (Amazon MQ) for async communication',
      'Create comprehensive integration tests',
      'Maintain feature parity validation between systems'
    ]
  })

  // Strategy 2: Strangler Fig Pattern
  strategies.push({
    approach: 'Strangler Fig Pattern',
    description:
      'Route new functionality to AWS while keeping existing COBOL code unchanged. Gradually replace COBOL modules as AWS services prove stable.',
    risks: [
      'Routing complexity at API gateway layer',
      'Maintaining two codebases simultaneously',
      'Inconsistent behavior between old and new systems'
    ],
    mitigations: [
      'Use API Gateway routing rules for gradual traffic shift',
      'Implement feature flags for controlled rollout',
      'Create automated comparison tests',
      'Monitor metrics for both systems in parallel'
    ]
  })

  // Strategy 3: API Wrapper Approach
  if (prioritizedModules.length > 0 && prioritizedModules[0].coupling < 5) {
    strategies.push({
      approach: 'API Wrapper (Recommended for Low Coupling)',
      description:
        'Create AWS Lambda wrappers that expose COBOL logic through REST APIs. COBOL code continues running on mainframe, accessed via API calls.',
      risks: [
        'Network latency between AWS and mainframe',
        'Mainframe availability becomes critical dependency',
        'Limited scalability due to mainframe constraints'
      ],
      mitigations: [
        'Implement caching layer in AWS for frequently accessed data',
        'Use Direct Connect for reliable mainframe connectivity',
        'Set up circuit breakers for mainframe failures',
        'Plan for eventual full migration after API stabilization'
      ]
    })
  }

  // Strategy 4: Rewrite with Validation
  if (analysis.patterns.length > 0) {
    strategies.push({
      approach: 'Rewrite with Validation',
      description: `Rewrite business logic in TypeScript/Python, focusing on ${analysis.patterns[0].type.replace(
        /_/g,
        ' '
      )} patterns. Validate outputs against COBOL program for identical inputs.`,
      risks: [
        'Logic translation errors',
        'Missing edge cases from COBOL code',
        'Calculation precision differences'
      ],
      mitigations: [
        'Use decimal.js for financial calculation precision',
        'Create comprehensive test suite from COBOL test cases',
        'Run parallel execution comparing COBOL vs AWS outputs',
        'Involve business analysts to validate business rules'
      ]
    })
  }

  return strategies
}
