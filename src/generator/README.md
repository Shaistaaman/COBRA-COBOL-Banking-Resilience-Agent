# AWS Code Generator

The AWS Code Generator module generates production-ready AWS infrastructure code from COBOL analysis results. It creates Lambda functions, API Gateway configurations, CDK infrastructure, and hybrid deployment components.

## Overview

The generator translates COBOL business logic into modern cloud-native applications by:

1. **Lambda Functions**: Converting COBOL procedures into TypeScript/Python Lambda handlers
2. **API Gateway**: Creating REST APIs that expose COBOL functionality
3. **CDK Infrastructure**: Generating Infrastructure as Code for deployment
4. **Hybrid Components**: Building bridges between mainframe and cloud systems

## Modules

### Lambda Generator (`lambda-generator.ts`)

Generates AWS Lambda functions from COBOL business logic.

**Features:**

- TypeScript and Python code generation
- Input validation based on COBOL PIC clauses
- Audit logging for compliance
- Error handling with HTTP status code mapping
- Business rule translation
- Banking pattern recognition (interest calculation, transaction posting)

**Usage:**

```typescript
import { generateLambdaFunction } from './generator'

const artifacts = generateLambdaFunction(analysis, {
  language: 'typescript',
  runtime: 'nodejs20.x',
  includeAuditLogging: true,
  includeInputValidation: true
})
```

**Generated Files:**

- `handler.ts` - Main Lambda handler
- `types.ts` - TypeScript type definitions
- `validation.ts` - Input validation module
- `package.json` - Dependencies
- `tsconfig.json` - TypeScript configuration

### API Gateway Generator (`api-gateway-generator.ts`)

Creates API Gateway REST API configurations.

**Features:**

- OpenAPI 3.0 specification generation
- Request/response model definitions
- IAM and Cognito authentication
- CORS configuration
- Rate limiting and throttling
- CDK construct for API Gateway

**Usage:**

```typescript
import { generateAPIGatewayConfig } from './generator'

const artifacts = generateAPIGatewayConfig(analysis, {
  apiName: 'CobolModernizationAPI',
  stageName: 'prod',
  authenticationType: 'IAM',
  enableCors: true,
  enableThrottling: true
})
```

**Generated Files:**

- `openapi.json` - OpenAPI specification
- `request-models.ts` - Request type definitions
- `response-models.ts` - Response type definitions
- `api-gateway-construct.ts` - CDK construct

### CDK Generator (`cdk-generator.ts`)

Generates AWS CDK infrastructure code.

**Features:**

- Complete CDK stack with Lambda functions
- IAM roles with least-privilege permissions
- CloudWatch log groups and dashboards
- AWS WAF rules for API protection
- S3 buckets with encryption
- VPC networking (optional)
- Monitoring and alerting

**Usage:**

```typescript
import { generateCDKStack } from './generator'

const artifacts = generateCDKStack(analysis, {
  stackName: 'CobolModernizationStack',
  enableWAF: true,
  enableEncryption: true,
  enableMonitoring: true,
  vpcConfig: { enableVpc: true }
})
```

**Generated Files:**

- `lib/stack.ts` - Main CDK stack
- `bin/app.ts` - CDK app entry point
- `package.json` - CDK dependencies
- `cdk.json` - CDK configuration
- `tsconfig.json` - TypeScript configuration

### Hybrid Infrastructure Generator (`hybrid-infrastructure-generator.ts`)

Creates infrastructure for mainframe-to-cloud integration.

**Features:**

- Amazon MQ broker for messaging
- S3 buckets for batch file synchronization
- Step Functions workflows for batch processing
- VPC with Direct Connect/VPN support
- CloudWatch monitoring for data synchronization

**Usage:**

```typescript
import { generateHybridInfrastructure } from './generator'

const artifacts = generateHybridInfrastructure(analysis, {
  enableMQ: true,
  enableS3Batch: true,
  enableStepFunctions: true,
  enableVPC: true,
  enableDirectConnect: false
})
```

**Generated Files:**

- `lib/amazonmq-construct.ts` - Amazon MQ configuration
- `lib/s3-batch-construct.ts` - S3 batch processing
- `lib/step-functions-construct.ts` - Step Functions workflow
- `lib/vpc-construct.ts` - VPC networking
- `lib/hybrid-monitoring-construct.ts` - Monitoring setup
- `workflows/batch-processing.json` - Step Functions definition

## COBOL to AWS Mapping

### Data Type Mapping

| COBOL PIC Clause | TypeScript Type | JSON Schema Type   |
| ---------------- | --------------- | ------------------ |
| `9(n)`           | `number`        | `integer`          |
| `9(n)V9(m)`      | `number`        | `number`           |
| `S9(n)`          | `number`        | `integer` (signed) |
| `X(n)`           | `string`        | `string`           |

### Business Logic Translation

| COBOL Pattern      | AWS Service    | Implementation                 |
| ------------------ | -------------- | ------------------------------ |
| COMPUTE statements | Lambda         | TypeScript/Python calculations |
| IF-THEN-ELSE       | Lambda         | Conditional logic              |
| PERFORM loops      | Lambda         | Iteration logic                |
| CALL statements    | Step Functions | Service orchestration          |
| File I/O           | S3             | Object storage                 |
| Database access    | DynamoDB/RDS   | NoSQL/SQL database             |
| Batch processing   | Step Functions | Workflow orchestration         |
| Message queues     | Amazon MQ      | Message broker                 |

### Return Code Mapping

| COBOL Return Code | HTTP Status Code | Description         |
| ----------------- | ---------------- | ------------------- |
| 0                 | 200              | Success             |
| 4                 | 400              | Validation error    |
| 8                 | 500              | Processing error    |
| 12                | 500              | Severe error        |
| 16                | 503              | Service unavailable |

## Generated Code Structure

### Lambda Function Structure

```
lambda/
├── src/
│   ├── handler.ts          # Main Lambda handler
│   ├── types.ts            # Type definitions
│   ├── validation.ts       # Input validation
│   └── audit.ts            # Audit logging (optional)
├── package.json
└── tsconfig.json
```

### CDK Project Structure

```
cdk/
├── bin/
│   └── app.ts              # CDK app entry point
├── lib/
│   ├── stack.ts            # Main stack
│   ├── api-gateway-construct.ts
│   ├── amazonmq-construct.ts
│   ├── s3-batch-construct.ts
│   ├── step-functions-construct.ts
│   ├── vpc-construct.ts
│   └── hybrid-monitoring-construct.ts
├── workflows/
│   └── batch-processing.json
├── package.json
├── cdk.json
└── tsconfig.json
```

## Security Features

All generated code includes:

- **Input Validation**: Based on COBOL PIC clauses
- **Audit Logging**: Compliance-ready logging
- **Encryption**: At rest (S3) and in transit (TLS)
- **IAM Roles**: Least-privilege permissions
- **WAF Rules**: API protection against common attacks
- **VPC Isolation**: Network security for sensitive workloads

## Deployment

### Deploy Lambda Function

```bash
cd lambda
npm install
npm run build
npm run package
aws lambda update-function-code --function-name MyFunction --zip-file fileb://function.zip
```

### Deploy CDK Stack

```bash
cd cdk
npm install
cdk bootstrap  # First time only
cdk synth      # Preview changes
cdk deploy     # Deploy to AWS
```

### Deploy with CI/CD

```bash
# GitHub Actions, GitLab CI, or AWS CodePipeline
cdk deploy --require-approval never
```

## Examples

See `examples/code-generator-usage.ts` for complete examples of:

- Generating Lambda functions
- Creating API Gateway configurations
- Building CDK stacks
- Setting up hybrid infrastructure

Run examples:

```bash
npm run build
node dist/examples/code-generator-usage.js
```

## Requirements Mapping

This module implements the following requirements from the COBRA specification:

- **Requirement 2.1**: Generate Lambda wrapper code for COBOL business logic
- **Requirement 2.2**: Create API Gateway configuration files
- **Requirement 2.3**: Include input validation matching COBOL rules
- **Requirement 2.4**: Generate error handling with return code mapping
- **Requirement 2.5**: Produce AWS CDK constructs for deployment
- **Requirement 3.1**: Generate audit logging code
- **Requirement 3.2**: Configure IAM roles with least-privilege
- **Requirement 3.3**: Implement authentication (IAM/Cognito)
- **Requirement 3.4**: Enable encryption at rest and in transit
- **Requirement 3.5**: Generate CloudWatch dashboards
- **Requirement 4.1**: Generate Amazon MQ broker configurations
- **Requirement 4.2**: Create S3 bucket setups for batch files
- **Requirement 4.3**: Build Step Functions workflows
- **Requirement 4.4**: Configure VPC networking
- **Requirement 4.5**: Add monitoring for data synchronization

## Testing

Unit tests for generators are located in `tests/unit/generator/`:

```bash
npm test -- generator
```

## Future Enhancements

- [ ] Python Lambda function generation improvements
- [ ] GraphQL API support (in addition to REST)
- [ ] EventBridge integration for event-driven architectures
- [ ] DynamoDB table generation from COBOL data structures
- [ ] RDS schema generation for relational data
- [ ] Terraform output (in addition to CDK)
- [ ] Pulumi output support
- [ ] Cost estimation for generated infrastructure
