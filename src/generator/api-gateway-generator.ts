/**
 * API Gateway Configuration Generator
 * Generates API Gateway REST API definitions from COBOL analysis
 */

import type {
  LogicAnalysis,
  EntryPoint,
  DataElement,
  DataStructure
} from '../mcp-server/types.js'
import type { GeneratorOptions, CodeArtifact } from './types.js'

export interface APIGatewayGeneratorOptions extends GeneratorOptions {
  apiName?: string
  stageName?: string
  authenticationType?: 'IAM' | 'COGNITO' | 'NONE'
  enableCors?: boolean
  enableThrottling?: boolean
}

export interface APIEndpoint {
  path: string
  method: 'GET' | 'POST' | 'PUT' | 'DELETE'
  lambdaFunction: string
  requestModel: string
  responseModel: string
  authentication: string
}

/**
 * Generate API Gateway configuration from COBOL analysis
 */
export function generateAPIGatewayConfig (
  analysis: LogicAnalysis,
  options: APIGatewayGeneratorOptions = {}
): CodeArtifact[] {
  const {
    apiName = 'CobolModernizationAPI',
    stageName = 'prod',
    authenticationType = 'IAM',
    enableCors = true,
    enableThrottling = true
  } = options

  const artifacts: CodeArtifact[] = []

  // Generate OpenAPI specification
  artifacts.push(
    generateOpenAPISpec(analysis, {
      apiName,
      stageName,
      authenticationType,
      enableCors
    })
  )

  // Generate request/response models
  artifacts.push(generateRequestModels(analysis))
  artifacts.push(generateResponseModels(analysis))

  // Generate API Gateway CDK construct (for integration with CDK)
  artifacts.push(
    generateAPIGatewayCDKConstruct(analysis, {
      apiName,
      stageName,
      authenticationType,
      enableCors,
      enableThrottling
    })
  )

  return artifacts
}

/**
 * Generate OpenAPI 3.0 specification
 */
function generateOpenAPISpec (
  analysis: LogicAnalysis,
  options: {
    apiName: string
    stageName: string
    authenticationType: string
    enableCors: boolean
  }
): CodeArtifact {
  const { entryPoints, dataStructures } = analysis
  const { apiName, stageName, authenticationType, enableCors } = options

  const spec: any = {
    openapi: '3.0.0',
    info: {
      title: apiName,
      description: 'Modernized COBOL banking operations API',
      version: '1.0.0'
    },
    servers: [
      {
        url: `https://api.example.com/${stageName}`,
        description: `${stageName} environment`
      }
    ],
    paths: {},
    components: {
      schemas: {},
      securitySchemes: {}
    }
  }

  // Add security schemes
  if (authenticationType === 'IAM') {
    spec.components.securitySchemes.AWS_IAM = {
      type: 'apiKey',
      name: 'Authorization',
      in: 'header',
      'x-amazon-apigateway-authtype': 'awsSigv4'
    }
  } else if (authenticationType === 'COGNITO') {
    spec.components.securitySchemes.CognitoAuthorizer = {
      type: 'apiKey',
      name: 'Authorization',
      in: 'header',
      'x-amazon-apigateway-authtype': 'cognito_user_pools'
    }
  }

  // Generate paths from entry points
  for (const entryPoint of entryPoints) {
    const path = `/${entryPoint.name.toLowerCase()}`
    const operationId = entryPoint.name

    spec.paths[path] = {
      post: {
        summary: `Execute ${entryPoint.name} operation`,
        description: `Invokes the ${entryPoint.name} Lambda function`,
        operationId,
        requestBody: {
          required: true,
          content: {
            'application/json': {
              schema: {
                $ref: `#/components/schemas/${entryPoint.name}Request`
              }
            }
          }
        },
        responses: {
          '200': {
            description: 'Successful operation',
            content: {
              'application/json': {
                schema: {
                  $ref: `#/components/schemas/${entryPoint.name}Response`
                }
              }
            }
          },
          '400': {
            description: 'Invalid input',
            content: {
              'application/json': {
                schema: {
                  $ref: '#/components/schemas/ErrorResponse'
                }
              }
            }
          },
          '500': {
            description: 'Internal server error',
            content: {
              'application/json': {
                schema: {
                  $ref: '#/components/schemas/ErrorResponse'
                }
              }
            }
          }
        },
        security:
          authenticationType !== 'NONE'
            ? [
                {
                  [authenticationType === 'IAM'
                    ? 'AWS_IAM'
                    : 'CognitoAuthorizer']: []
                }
              ]
            : [],
        'x-amazon-apigateway-integration': {
          type: 'aws_proxy',
          httpMethod: 'POST',
          uri: `arn:aws:apigateway:\${AWS::Region}:lambda:path/2015-03-31/functions/\${${entryPoint.name}LambdaFunction.Arn}/invocations`,
          passthroughBehavior: 'when_no_match'
        }
      }
    }

    // Add CORS options if enabled
    if (enableCors) {
      spec.paths[path].options = {
        summary: 'CORS support',
        responses: {
          '200': {
            description: 'CORS headers',
            headers: {
              'Access-Control-Allow-Origin': {
                schema: { type: 'string' }
              },
              'Access-Control-Allow-Methods': {
                schema: { type: 'string' }
              },
              'Access-Control-Allow-Headers': {
                schema: { type: 'string' }
              }
            }
          }
        },
        'x-amazon-apigateway-integration': {
          type: 'mock',
          requestTemplates: {
            'application/json': '{"statusCode": 200}'
          },
          responses: {
            default: {
              statusCode: '200',
              responseParameters: {
                'method.response.header.Access-Control-Allow-Origin': "'*'",
                'method.response.header.Access-Control-Allow-Methods':
                  "'POST,OPTIONS'",
                'method.response.header.Access-Control-Allow-Headers':
                  "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'"
              }
            }
          }
        }
      }
    }
  }

  // Generate schemas from data structures
  for (const entryPoint of entryPoints) {
    // Request schema
    spec.components.schemas[`${entryPoint.name}Request`] = {
      type: 'object',
      properties: generateSchemaProperties(entryPoint.parameters),
      required: entryPoint.parameters.map(p => p.name)
    }

    // Response schema
    spec.components.schemas[`${entryPoint.name}Response`] = {
      type: 'object',
      properties: {
        success: { type: 'boolean' },
        message: { type: 'string' },
        data: { type: 'object' }
      }
    }
  }

  // Error response schema
  spec.components.schemas.ErrorResponse = {
    type: 'object',
    properties: {
      error: { type: 'string' },
      requestId: { type: 'string' },
      details: { type: 'array', items: { type: 'string' } }
    }
  }

  return {
    filename: 'openapi.json',
    content: JSON.stringify(spec, null, 2),
    language: 'json'
  }
}

/**
 * Generate schema properties from data elements
 */
function generateSchemaProperties (
  elements: DataElement[]
): Record<string, any> {
  const properties: Record<string, any> = {}

  for (const element of elements) {
    const schema = cobolPicToJsonSchema(element.picture || '')
    properties[element.name] = schema
  }

  return properties
}

/**
 * Convert COBOL PIC clause to JSON Schema type
 */
function cobolPicToJsonSchema (picture: string): any {
  if (!picture) {
    return { type: 'string' }
  }

  // Numeric types
  if (/^9+$/.test(picture)) {
    return {
      type: 'integer',
      minimum: 0,
      maximum: Math.pow(10, picture.length) - 1
    }
  }

  // Decimal types
  if (/^9+V9+$/.test(picture)) {
    const parts = picture.split('V')
    const integerDigits = parts[0].length
    const decimalDigits = parts[1].length

    return {
      type: 'number',
      minimum: 0,
      maximum: Math.pow(10, integerDigits) - 1,
      multipleOf: Math.pow(10, -decimalDigits)
    }
  }

  // Signed numeric
  if (/^S9+/.test(picture)) {
    const digits = picture.replace('S', '').length
    return {
      type: 'integer',
      minimum: -Math.pow(10, digits - 1),
      maximum: Math.pow(10, digits - 1) - 1
    }
  }

  // Alphanumeric types
  if (/^X+$/.test(picture)) {
    return {
      type: 'string',
      maxLength: picture.length
    }
  }

  // Default to string
  return { type: 'string' }
}

/**
 * Generate request models
 */
function generateRequestModels (analysis: LogicAnalysis): CodeArtifact {
  const { entryPoints } = analysis

  let code = `/**
 * API Gateway Request Models
 * Generated from COBOL data structures
 */

`

  for (const entryPoint of entryPoints) {
    code += `export interface ${entryPoint.name}Request {\n`

    for (const param of entryPoint.parameters) {
      const tsType = cobolPicToTypeScript(param.picture || '')
      code += `  ${param.name}: ${tsType}\n`
    }

    code += `}\n\n`
  }

  return {
    filename: 'request-models.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate response models
 */
function generateResponseModels (analysis: LogicAnalysis): CodeArtifact {
  const { entryPoints } = analysis

  let code = `/**
 * API Gateway Response Models
 * Generated from COBOL data structures
 */

export interface BaseResponse {
  success: boolean
  message: string
  requestId?: string
}

export interface ErrorResponse {
  error: string
  requestId: string
  details?: string[]
}

`

  for (const entryPoint of entryPoints) {
    code += `export interface ${entryPoint.name}Response extends BaseResponse {\n`
    code += `  data: any // TODO: Define specific response data structure\n`
    code += `}\n\n`
  }

  return {
    filename: 'response-models.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Convert COBOL PIC to TypeScript type
 */
function cobolPicToTypeScript (picture: string): string {
  if (!picture) return 'any'

  if (/^9+$/.test(picture) || /^9+V9+$/.test(picture) || /^S9+/.test(picture)) {
    return 'number'
  }

  if (/^X+$/.test(picture)) {
    return 'string'
  }

  return 'string'
}

/**
 * Generate API Gateway CDK construct
 */
function generateAPIGatewayCDKConstruct (
  analysis: LogicAnalysis,
  options: {
    apiName: string
    stageName: string
    authenticationType: string
    enableCors: boolean
    enableThrottling: boolean
  }
): CodeArtifact {
  const { entryPoints } = analysis
  const {
    apiName,
    stageName,
    authenticationType,
    enableCors,
    enableThrottling
  } = options

  let code = `/**
 * API Gateway CDK Construct
 * Generated from COBOL analysis
 */

import * as cdk from 'aws-cdk-lib'
import * as apigateway from 'aws-cdk-lib/aws-apigateway'
import * as lambda from 'aws-cdk-lib/aws-lambda'
import * as iam from 'aws-cdk-lib/aws-iam'
import * as cognito from 'aws-cdk-lib/aws-cognito'
import { Construct } from 'constructs'

export interface APIGatewayConstructProps {
  lambdaFunctions: Record<string, lambda.Function>
  userPool?: cognito.UserPool
}

export class CobolAPIGatewayConstruct extends Construct {
  public readonly api: apigateway.RestApi
  public readonly deployment: apigateway.Deployment

  constructor(scope: Construct, id: string, props: APIGatewayConstructProps) {
    super(scope, id)

    // Create REST API
    this.api = new apigateway.RestApi(this, 'API', {
      restApiName: '${apiName}',
      description: 'Modernized COBOL banking operations API',
      deployOptions: {
        stageName: '${stageName}',
        loggingLevel: apigateway.MethodLoggingLevel.INFO,
        dataTraceEnabled: true,
        metricsEnabled: true,
${
  enableThrottling
    ? `        throttlingRateLimit: 1000,\n        throttlingBurstLimit: 2000,`
    : ''
}
      },
${
  enableCors
    ? `      defaultCorsPreflightOptions: {\n        allowOrigins: apigateway.Cors.ALL_ORIGINS,\n        allowMethods: apigateway.Cors.ALL_METHODS,\n        allowHeaders: [\n          'Content-Type',\n          'X-Amz-Date',\n          'Authorization',\n          'X-Api-Key',\n          'X-Amz-Security-Token'\n        ]\n      },`
    : ''
}
    })

`

  // Add authentication
  if (authenticationType === 'IAM') {
    code += `    // IAM authentication\n`
    code += `    const iamAuthorizer = new apigateway.RequestAuthorizer(this, 'IAMAuthorizer', {\n`
    code += `      handler: props.lambdaFunctions['authorizer'],\n`
    code += `      identitySources: [apigateway.IdentitySource.header('Authorization')]\n`
    code += `    })\n\n`
  } else if (authenticationType === 'COGNITO') {
    code += `    // Cognito authentication\n`
    code += `    const cognitoAuthorizer = new apigateway.CognitoUserPoolsAuthorizer(this, 'CognitoAuthorizer', {\n`
    code += `      cognitoUserPools: [props.userPool!]\n`
    code += `    })\n\n`
  }

  // Add endpoints for each entry point
  for (const entryPoint of entryPoints) {
    const resourceName = entryPoint.name.toLowerCase()
    const functionName = entryPoint.name

    code += `    // ${entryPoint.name} endpoint\n`
    code += `    const ${resourceName}Resource = this.api.root.addResource('${resourceName}')\n`
    code += `    const ${resourceName}Integration = new apigateway.LambdaIntegration(\n`
    code += `      props.lambdaFunctions['${functionName}'],\n`
    code += `      {\n`
    code += `        proxy: true,\n`
    code += `        allowTestInvoke: true\n`
    code += `      }\n`
    code += `    )\n\n`

    code += `    ${resourceName}Resource.addMethod('POST', ${resourceName}Integration`

    if (authenticationType !== 'NONE') {
      code += `, {\n`
      code += `      authorizer: ${
        authenticationType === 'IAM' ? 'iamAuthorizer' : 'cognitoAuthorizer'
      },\n`
      code += `      authorizationType: apigateway.AuthorizationType.${
        authenticationType === 'IAM' ? 'IAM' : 'COGNITO'
      }\n`
      code += `    }`
    }

    code += `)\n\n`
  }

  // Add request validation
  code += `    // Request validation\n`
  code += `    const requestValidator = new apigateway.RequestValidator(this, 'RequestValidator', {\n`
  code += `      restApi: this.api,\n`
  code += `      validateRequestBody: true,\n`
  code += `      validateRequestParameters: true\n`
  code += `    })\n\n`

  // Add CloudWatch role
  code += `    // CloudWatch logging role\n`
  code += `    const cloudWatchRole = new iam.Role(this, 'CloudWatchRole', {\n`
  code += `      assumedBy: new iam.ServicePrincipal('apigateway.amazonaws.com'),\n`
  code += `      managedPolicies: [\n`
  code += `        iam.ManagedPolicy.fromAwsManagedPolicyName(\n`
  code += `          'service-role/AmazonAPIGatewayPushToCloudWatchLogs'\n`
  code += `        )\n`
  code += `      ]\n`
  code += `    })\n\n`

  // Add usage plan
  if (enableThrottling) {
    code += `    // Usage plan for rate limiting\n`
    code += `    const usagePlan = this.api.addUsagePlan('UsagePlan', {\n`
    code += `      name: '${apiName}-usage-plan',\n`
    code += `      throttle: {\n`
    code += `        rateLimit: 1000,\n`
    code += `        burstLimit: 2000\n`
    code += `      },\n`
    code += `      quota: {\n`
    code += `        limit: 1000000,\n`
    code += `        period: apigateway.Period.MONTH\n`
    code += `      }\n`
    code += `    })\n\n`

    code += `    usagePlan.addApiStage({\n`
    code += `      stage: this.api.deploymentStage\n`
    code += `    })\n\n`
  }

  // Output API URL
  code += `    // Output API URL\n`
  code += `    new cdk.CfnOutput(this, 'APIUrl', {\n`
  code += `      value: this.api.url,\n`
  code += `      description: 'API Gateway URL',\n`
  code += `      exportName: '${apiName}Url'\n`
  code += `    })\n`

  code += `  }\n`
  code += `}\n`

  return {
    filename: 'api-gateway-construct.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Map COBOL entry points to API endpoints
 */
export function mapEntryPointsToEndpoints (
  entryPoints: EntryPoint[]
): APIEndpoint[] {
  return entryPoints.map(ep => ({
    path: `/${ep.name.toLowerCase()}`,
    method: 'POST',
    lambdaFunction: ep.name,
    requestModel: `${ep.name}Request`,
    responseModel: `${ep.name}Response`,
    authentication: 'IAM'
  }))
}
