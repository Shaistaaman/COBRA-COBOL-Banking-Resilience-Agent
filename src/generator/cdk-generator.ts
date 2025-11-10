/**
 * AWS CDK Construct Generator
 * Generates CDK infrastructure code from COBOL analysis
 */

import type { LogicAnalysis, EntryPoint } from '../mcp-server/types.js'
import type { GeneratorOptions, CodeArtifact } from './types.js'

export interface CDKGeneratorOptions extends GeneratorOptions {
  stackName?: string
  enableWAF?: boolean
  enableEncryption?: boolean
  enableMonitoring?: boolean
  vpcConfig?: {
    enableVpc: boolean
    cidr?: string
  }
}

/**
 * Generate CDK infrastructure code from COBOL analysis
 */
export function generateCDKStack (
  analysis: LogicAnalysis,
  options: CDKGeneratorOptions = {}
): CodeArtifact[] {
  const {
    stackName = 'CobolModernizationStack',
    enableWAF = true,
    enableEncryption = true,
    enableMonitoring = true
  } = options

  const artifacts: CodeArtifact[] = []

  // Generate main CDK stack
  artifacts.push(generateMainStack(analysis, options))

  // Generate CDK app entry point
  artifacts.push(generateCDKApp(stackName))

  // Generate package.json
  artifacts.push(generateCDKPackageJson())

  // Generate cdk.json
  artifacts.push(generateCDKConfig())

  // Generate tsconfig.json
  artifacts.push(generateCDKTsConfig())

  return artifacts
}

/**
 * Generate main CDK stack
 */
function generateMainStack (
  analysis: LogicAnalysis,
  options: CDKGeneratorOptions
): CodeArtifact {
  const {
    stackName = 'CobolModernizationStack',
    enableWAF = true,
    enableEncryption = true,
    enableMonitoring = true,
    vpcConfig
  } = options

  const { entryPoints } = analysis

  let code = `/**
 * CDK Stack - Generated from COBOL
 * Infrastructure as Code for modernized COBOL applications
 */

import * as cdk from 'aws-cdk-lib'
import * as lambda from 'aws-cdk-lib/aws-lambda'
import * as apigateway from 'aws-cdk-lib/aws-apigateway'
import * as iam from 'aws-cdk-lib/aws-iam'
import * as logs from 'aws-cdk-lib/aws-logs'
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch'
import * as s3 from 'aws-cdk-lib/aws-s3'
`

  if (enableWAF) {
    code += `import * as wafv2 from 'aws-cdk-lib/aws-wafv2'\n`
  }

  if (vpcConfig?.enableVpc) {
    code += `import * as ec2 from 'aws-cdk-lib/aws-ec2'\n`
  }

  code += `import { Construct } from 'constructs'

export class ${stackName} extends cdk.Stack {
  public readonly api: apigateway.RestApi
  public readonly lambdaFunctions: Record<string, lambda.Function> = {}

  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props)

`

  // VPC configuration
  if (vpcConfig?.enableVpc) {
    code += `    // VPC for secure networking\n`
    code += `    const vpc = new ec2.Vpc(this, 'VPC', {\n`
    code += `      maxAzs: 2,\n`
    code += `      natGateways: 1,\n`
    code += `      subnetConfiguration: [\n`
    code += `        {\n`
    code += `          name: 'Public',\n`
    code += `          subnetType: ec2.SubnetType.PUBLIC,\n`
    code += `          cidrMask: 24\n`
    code += `        },\n`
    code += `        {\n`
    code += `          name: 'Private',\n`
    code += `          subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS,\n`
    code += `          cidrMask: 24\n`
    code += `        }\n`
    code += `      ]\n`
    code += `    })\n\n`
  }

  // IAM role for Lambda
  code += `    // IAM role for Lambda functions with least-privilege permissions\n`
  code += `    const lambdaRole = new iam.Role(this, 'LambdaExecutionRole', {\n`
  code += `      assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com'),\n`
  code += `      managedPolicies: [\n`
  code += `        iam.ManagedPolicy.fromAwsManagedPolicyName(\n`
  code += `          'service-role/AWSLambdaBasicExecutionRole'\n`
  code += `        )\n`
  code += `      ]\n`
  code += `    })\n\n`

  // Add CloudWatch permissions
  code += `    // CloudWatch Logs permissions\n`
  code += `    lambdaRole.addToPolicy(\n`
  code += `      new iam.PolicyStatement({\n`
  code += `        actions: [\n`
  code += `          'logs:CreateLogGroup',\n`
  code += `          'logs:CreateLogStream',\n`
  code += `          'logs:PutLogEvents'\n`
  code += `        ],\n`
  code += `        resources: ['*']\n`
  code += `      })\n`
  code += `    )\n\n`

  // Generate Lambda functions
  for (const entryPoint of entryPoints) {
    const functionName = entryPoint.name

    code += `    // Lambda function: ${functionName}\n`
    code += `    const ${functionName}Function = new lambda.Function(this, '${functionName}Function', {\n`
    code += `      runtime: lambda.Runtime.NODEJS_20_X,\n`
    code += `      handler: 'handler.handler',\n`
    code += `      code: lambda.Code.fromAsset('lambda/${functionName}'),\n`
    code += `      role: lambdaRole,\n`
    code += `      timeout: cdk.Duration.seconds(30),\n`
    code += `      memorySize: 512,\n`
    code += `      environment: {\n`
    code += `        NODE_ENV: 'production',\n`
    code += `        LOG_LEVEL: 'info'\n`
    code += `      },\n`

    if (vpcConfig?.enableVpc) {
      code += `      vpc,\n`
      code += `      vpcSubnets: { subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS },\n`
    }

    code += `      logRetention: logs.RetentionDays.ONE_WEEK\n`
    code += `    })\n\n`

    code += `    this.lambdaFunctions['${functionName}'] = ${functionName}Function\n\n`

    // CloudWatch log group
    code += `    // CloudWatch log group for ${functionName}\n`
    code += `    const ${functionName}LogGroup = new logs.LogGroup(this, '${functionName}LogGroup', {\n`
    code += `      logGroupName: \`/aws/lambda/\${${functionName}Function.functionName}\`,\n`
    code += `      retention: logs.RetentionDays.ONE_WEEK,\n`
    code += `      removalPolicy: cdk.RemovalPolicy.DESTROY\n`
    code += `    })\n\n`
  }

  // API Gateway
  code += `    // API Gateway REST API\n`
  code += `    this.api = new apigateway.RestApi(this, 'API', {\n`
  code += `      restApiName: 'CobolModernizationAPI',\n`
  code += `      description: 'Modernized COBOL banking operations',\n`
  code += `      deployOptions: {\n`
  code += `        stageName: 'prod',\n`
  code += `        loggingLevel: apigateway.MethodLoggingLevel.INFO,\n`
  code += `        dataTraceEnabled: true,\n`
  code += `        metricsEnabled: true,\n`
  code += `        throttlingRateLimit: 1000,\n`
  code += `        throttlingBurstLimit: 2000\n`
  code += `      },\n`
  code += `      defaultCorsPreflightOptions: {\n`
  code += `        allowOrigins: apigateway.Cors.ALL_ORIGINS,\n`
  code += `        allowMethods: apigateway.Cors.ALL_METHODS\n`
  code += `      }\n`
  code += `    })\n\n`

  // Add API endpoints
  for (const entryPoint of entryPoints) {
    const functionName = entryPoint.name
    const resourceName = functionName.toLowerCase()

    code += `    // ${functionName} endpoint\n`
    code += `    const ${resourceName}Resource = this.api.root.addResource('${resourceName}')\n`
    code += `    ${resourceName}Resource.addMethod(\n`
    code += `      'POST',\n`
    code += `      new apigateway.LambdaIntegration(this.lambdaFunctions['${functionName}'])\n`
    code += `    )\n\n`
  }

  // WAF configuration
  if (enableWAF) {
    code += generateWAFConfiguration()
  }

  // CloudWatch dashboard
  if (enableMonitoring) {
    code += generateCloudWatchDashboard(entryPoints)
  }

  // Encryption configuration
  if (enableEncryption) {
    code += generateEncryptionConfiguration()
  }

  // Outputs
  code += `    // Stack outputs\n`
  code += `    new cdk.CfnOutput(this, 'APIUrl', {\n`
  code += `      value: this.api.url,\n`
  code += `      description: 'API Gateway URL',\n`
  code += `      exportName: 'CobolAPIUrl'\n`
  code += `    })\n\n`

  for (const entryPoint of entryPoints) {
    code += `    new cdk.CfnOutput(this, '${entryPoint.name}FunctionArn', {\n`
    code += `      value: this.lambdaFunctions['${entryPoint.name}'].functionArn,\n`
    code += `      description: '${entryPoint.name} Lambda function ARN'\n`
    code += `    })\n\n`
  }

  code += `  }\n`
  code += `}\n`

  return {
    filename: 'lib/stack.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate WAF configuration
 */
function generateWAFConfiguration (): string {
  return `    // AWS WAF for API protection
    const webAcl = new wafv2.CfnWebACL(this, 'WebACL', {
      scope: 'REGIONAL',
      defaultAction: { allow: {} },
      visibilityConfig: {
        sampledRequestsEnabled: true,
        cloudWatchMetricsEnabled: true,
        metricName: 'CobolAPIWebACL'
      },
      rules: [
        {
          name: 'RateLimitRule',
          priority: 1,
          statement: {
            rateBasedStatement: {
              limit: 2000,
              aggregateKeyType: 'IP'
            }
          },
          action: { block: {} },
          visibilityConfig: {
            sampledRequestsEnabled: true,
            cloudWatchMetricsEnabled: true,
            metricName: 'RateLimitRule'
          }
        },
        {
          name: 'AWSManagedRulesCommonRuleSet',
          priority: 2,
          statement: {
            managedRuleGroupStatement: {
              vendorName: 'AWS',
              name: 'AWSManagedRulesCommonRuleSet'
            }
          },
          overrideAction: { none: {} },
          visibilityConfig: {
            sampledRequestsEnabled: true,
            cloudWatchMetricsEnabled: true,
            metricName: 'AWSManagedRulesCommonRuleSet'
          }
        },
        {
          name: 'AWSManagedRulesKnownBadInputsRuleSet',
          priority: 3,
          statement: {
            managedRuleGroupStatement: {
              vendorName: 'AWS',
              name: 'AWSManagedRulesKnownBadInputsRuleSet'
            }
          },
          overrideAction: { none: {} },
          visibilityConfig: {
            sampledRequestsEnabled: true,
            cloudWatchMetricsEnabled: true,
            metricName: 'AWSManagedRulesKnownBadInputsRuleSet'
          }
        }
      ]
    })

    // Associate WAF with API Gateway
    new wafv2.CfnWebACLAssociation(this, 'WebACLAssociation', {
      resourceArn: this.api.deploymentStage.stageArn,
      webAclArn: webAcl.attrArn
    })

`
}

/**
 * Generate CloudWatch dashboard
 */
function generateCloudWatchDashboard (entryPoints: EntryPoint[]): string {
  let code = `    // CloudWatch Dashboard for monitoring\n`
  code += `    const dashboard = new cloudwatch.Dashboard(this, 'Dashboard', {\n`
  code += `      dashboardName: 'CobolModernizationDashboard'\n`
  code += `    })\n\n`

  // Add widgets for each Lambda function
  for (const entryPoint of entryPoints) {
    const functionName = entryPoint.name

    code += `    // ${functionName} metrics\n`
    code += `    dashboard.addWidgets(\n`
    code += `      new cloudwatch.GraphWidget({\n`
    code += `        title: '${functionName} - Invocations',\n`
    code += `        left: [\n`
    code += `          this.lambdaFunctions['${functionName}'].metricInvocations()\n`
    code += `        ]\n`
    code += `      }),\n`
    code += `      new cloudwatch.GraphWidget({\n`
    code += `        title: '${functionName} - Errors',\n`
    code += `        left: [\n`
    code += `          this.lambdaFunctions['${functionName}'].metricErrors()\n`
    code += `        ]\n`
    code += `      }),\n`
    code += `      new cloudwatch.GraphWidget({\n`
    code += `        title: '${functionName} - Duration',\n`
    code += `        left: [\n`
    code += `          this.lambdaFunctions['${functionName}'].metricDuration()\n`
    code += `        ]\n`
    code += `      })\n`
    code += `    )\n\n`
  }

  // API Gateway metrics
  code += `    // API Gateway metrics\n`
  code += `    dashboard.addWidgets(\n`
  code += `      new cloudwatch.GraphWidget({\n`
  code += `        title: 'API Gateway - Requests',\n`
  code += `        left: [\n`
  code += `          this.api.metricCount()\n`
  code += `        ]\n`
  code += `      }),\n`
  code += `      new cloudwatch.GraphWidget({\n`
  code += `        title: 'API Gateway - Latency',\n`
  code += `        left: [\n`
  code += `          this.api.metricLatency()\n`
  code += `        ]\n`
  code += `      }),\n`
  code += `      new cloudwatch.GraphWidget({\n`
  code += `        title: 'API Gateway - 4XX Errors',\n`
  code += `        left: [\n`
  code += `          this.api.metricClientError()\n`
  code += `        ]\n`
  code += `      }),\n`
  code += `      new cloudwatch.GraphWidget({\n`
  code += `        title: 'API Gateway - 5XX Errors',\n`
  code += `        left: [\n`
  code += `          this.api.metricServerError()\n`
  code += `        ]\n`
  code += `      })\n`
  code += `    )\n\n`

  return code
}

/**
 * Generate encryption configuration
 */
function generateEncryptionConfiguration (): string {
  return `    // S3 bucket for data storage with encryption
    const dataBucket = new s3.Bucket(this, 'DataBucket', {
      encryption: s3.BucketEncryption.S3_MANAGED,
      enforceSSL: true,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      removalPolicy: cdk.RemovalPolicy.RETAIN,
      versioned: true,
      lifecycleRules: [
        {
          expiration: cdk.Duration.days(90),
          transitions: [
            {
              storageClass: s3.StorageClass.INFREQUENT_ACCESS,
              transitionAfter: cdk.Duration.days(30)
            }
          ]
        }
      ]
    })

    // Grant Lambda functions access to S3 bucket
    Object.values(this.lambdaFunctions).forEach(fn => {
      dataBucket.grantReadWrite(fn)
    })

`
}

/**
 * Generate CDK app entry point
 */
function generateCDKApp (stackName: string): CodeArtifact {
  const code = `#!/usr/bin/env node
/**
 * CDK App Entry Point
 */

import * as cdk from 'aws-cdk-lib'
import { ${stackName} } from '../lib/stack'

const app = new cdk.App()

new ${stackName}(app, '${stackName}', {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  },
  description: 'COBOL Modernization Infrastructure Stack',
  tags: {
    Project: 'COBRA',
    Environment: 'Production',
    ManagedBy: 'CDK'
  }
})

app.synth()
`

  return {
    filename: 'bin/app.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate CDK package.json
 */
function generateCDKPackageJson (): CodeArtifact {
  const content = {
    name: 'cobol-modernization-cdk',
    version: '1.0.0',
    description: 'CDK infrastructure for COBOL modernization',
    bin: {
      app: 'bin/app.js'
    },
    scripts: {
      build: 'tsc',
      watch: 'tsc -w',
      test: 'jest',
      cdk: 'cdk',
      synth: 'cdk synth',
      deploy: 'cdk deploy',
      diff: 'cdk diff',
      destroy: 'cdk destroy'
    },
    dependencies: {
      'aws-cdk-lib': '^2.100.0',
      constructs: '^10.0.0'
    },
    devDependencies: {
      '@types/node': '^20.0.0',
      typescript: '^5.0.0',
      'aws-cdk': '^2.100.0',
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
 * Generate cdk.json configuration
 */
function generateCDKConfig (): CodeArtifact {
  const content = {
    app: 'npx ts-node --prefer-ts-exts bin/app.ts',
    watch: {
      include: ['**'],
      exclude: [
        'README.md',
        'cdk*.json',
        '**/*.d.ts',
        '**/*.js',
        'tsconfig.json',
        'package*.json',
        'yarn.lock',
        'node_modules'
      ]
    },
    context: {
      '@aws-cdk/aws-lambda:recognizeLayerVersion': true,
      '@aws-cdk/core:checkSecretUsage': true,
      '@aws-cdk/core:target-partitions': ['aws', 'aws-cn'],
      '@aws-cdk-containers/ecs-service-extensions:enableDefaultLogDriver': true,
      '@aws-cdk/aws-ec2:uniqueImdsv2TemplateName': true,
      '@aws-cdk/aws-ecs:arnFormatIncludesClusterName': true,
      '@aws-cdk/aws-iam:minimizePolicies': true,
      '@aws-cdk/core:validateSnapshotRemovalPolicy': true,
      '@aws-cdk/aws-codepipeline:crossAccountKeyAliasStackSafeResourceName':
        true,
      '@aws-cdk/aws-s3:createDefaultLoggingPolicy': true,
      '@aws-cdk/aws-sns-subscriptions:restrictSqsDescryption': true,
      '@aws-cdk/aws-apigateway:disableCloudWatchRole': false,
      '@aws-cdk/core:enablePartitionLiterals': true,
      '@aws-cdk/aws-events:eventsTargetQueueSameAccount': true,
      '@aws-cdk/aws-iam:standardizedServicePrincipals': true,
      '@aws-cdk/aws-ecs:disableExplicitDeploymentControllerForCircuitBreaker':
        true,
      '@aws-cdk/aws-iam:importedRoleStackSafeDefaultPolicyName': true,
      '@aws-cdk/aws-s3:serverAccessLogsUseBucketPolicy': true,
      '@aws-cdk/aws-route53-patters:useCertificate': true,
      '@aws-cdk/customresources:installLatestAwsSdkDefault': false,
      '@aws-cdk/aws-rds:databaseProxyUniqueResourceName': true,
      '@aws-cdk/aws-codedeploy:removeAlarmsFromDeploymentGroup': true,
      '@aws-cdk/aws-apigateway:authorizerChangeDeploymentLogicalId': true,
      '@aws-cdk/aws-ec2:launchTemplateDefaultUserData': true,
      '@aws-cdk/aws-secretsmanager:useAttachedSecretResourcePolicyForSecretTargetAttachments':
        true,
      '@aws-cdk/aws-redshift:columnId': true,
      '@aws-cdk/aws-stepfunctions-tasks:enableEmrServicePolicyV2': true,
      '@aws-cdk/aws-ec2:restrictDefaultSecurityGroup': true,
      '@aws-cdk/aws-apigateway:requestValidatorUniqueId': true,
      '@aws-cdk/aws-kms:aliasNameRef': true,
      '@aws-cdk/aws-autoscaling:generateLaunchTemplateInsteadOfLaunchConfig':
        true,
      '@aws-cdk/core:includePrefixInUniqueNameGeneration': true
    }
  }

  return {
    filename: 'cdk.json',
    content: JSON.stringify(content, null, 2),
    language: 'json'
  }
}

/**
 * Generate tsconfig.json for CDK
 */
function generateCDKTsConfig (): CodeArtifact {
  const content = {
    compilerOptions: {
      target: 'ES2022',
      module: 'commonjs',
      lib: ['ES2022'],
      declaration: true,
      strict: true,
      noImplicitAny: true,
      strictNullChecks: true,
      noImplicitThis: true,
      alwaysStrict: true,
      noUnusedLocals: false,
      noUnusedParameters: false,
      noImplicitReturns: true,
      noFallthroughCasesInSwitch: false,
      inlineSourceMap: true,
      inlineSources: true,
      experimentalDecorators: true,
      strictPropertyInitialization: false,
      typeRoots: ['./node_modules/@types'],
      esModuleInterop: true,
      skipLibCheck: true,
      forceConsistentCasingInFileNames: true,
      resolveJsonModule: true
    },
    exclude: ['node_modules', 'cdk.out']
  }

  return {
    filename: 'tsconfig.json',
    content: JSON.stringify(content, null, 2),
    language: 'json'
  }
}
