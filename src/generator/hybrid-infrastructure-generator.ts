/**
 * Hybrid Deployment Infrastructure Generator
 * Generates infrastructure for connecting mainframe COBOL to AWS
 */

import type { LogicAnalysis, BankingPattern } from '../mcp-server/types.js'
import type { GeneratorOptions, CodeArtifact } from './types.js'

export interface HybridInfrastructureOptions extends GeneratorOptions {
  enableMQ?: boolean
  enableS3Batch?: boolean
  enableStepFunctions?: boolean
  enableVPC?: boolean
  enableDirectConnect?: boolean
}

/**
 * Generate hybrid deployment infrastructure
 */
export function generateHybridInfrastructure (
  analysis: LogicAnalysis,
  options: HybridInfrastructureOptions = {}
): CodeArtifact[] {
  const {
    enableMQ = true,
    enableS3Batch = true,
    enableStepFunctions = true,
    enableVPC = true,
    enableDirectConnect = false
  } = options

  const artifacts: CodeArtifact[] = []

  // Generate Amazon MQ configuration
  if (enableMQ) {
    artifacts.push(generateAmazonMQConfig(analysis))
  }

  // Generate S3 batch processing configuration
  if (enableS3Batch) {
    artifacts.push(generateS3BatchConfig(analysis))
  }

  // Generate Step Functions workflows
  if (enableStepFunctions) {
    artifacts.push(...generateStepFunctionsWorkflows(analysis))
  }

  // Generate VPC networking configuration
  if (enableVPC) {
    artifacts.push(generateVPCConfig(enableDirectConnect))
  }

  // Generate monitoring configuration
  artifacts.push(generateHybridMonitoring())

  return artifacts
}

/**
 * Generate Amazon MQ broker configuration
 */
function generateAmazonMQConfig (analysis: LogicAnalysis): CodeArtifact {
  let code = `/**
 * Amazon MQ Configuration for Mainframe Messaging
 * Enables message-based integration between COBOL and AWS
 */

import * as cdk from 'aws-cdk-lib'
import * as amazonmq from 'aws-cdk-lib/aws-amazonmq'
import * as ec2 from 'aws-cdk-lib/aws-ec2'
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager'
import { Construct } from 'constructs'

export interface AmazonMQConstructProps {
  vpc: ec2.Vpc
  brokerName?: string
  instanceType?: string
}

export class AmazonMQConstruct extends Construct {
  public readonly broker: amazonmq.CfnBroker
  public readonly credentials: secretsmanager.Secret

  constructor(scope: Construct, id: string, props: AmazonMQConstructProps) {
    super(scope, id)

    const { vpc, brokerName = 'CobolMainframeBroker', instanceType = 'mq.t3.micro' } = props

    // Create credentials secret
    this.credentials = new secretsmanager.Secret(this, 'BrokerCredentials', {
      secretName: 'amazonmq-broker-credentials',
      generateSecretString: {
        secretStringTemplate: JSON.stringify({ username: 'admin' }),
        generateStringKey: 'password',
        excludePunctuation: true,
        passwordLength: 32
      }
    })

    // Security group for MQ broker
    const brokerSecurityGroup = new ec2.SecurityGroup(this, 'BrokerSecurityGroup', {
      vpc,
      description: 'Security group for Amazon MQ broker',
      allowAllOutbound: true
    })

    // Allow mainframe to connect (configure with actual mainframe IP range)
    brokerSecurityGroup.addIngressRule(
      ec2.Peer.ipv4('10.0.0.0/8'), // Replace with actual mainframe CIDR
      ec2.Port.tcp(61617),
      'Allow OpenWire from mainframe'
    )

    brokerSecurityGroup.addIngressRule(
      ec2.Peer.ipv4('10.0.0.0/8'),
      ec2.Port.tcp(5671),
      'Allow AMQP from mainframe'
    )

    // Create Amazon MQ broker
    this.broker = new amazonmq.CfnBroker(this, 'Broker', {
      brokerName,
      engineType: 'ACTIVEMQ',
      engineVersion: '5.17',
      hostInstanceType: instanceType,
      deploymentMode: 'ACTIVE_STANDBY_MULTI_AZ',
      publiclyAccessible: false,
      autoMinorVersionUpgrade: true,
      users: [
        {
          username: 'admin',
          password: this.credentials.secretValueFromJson('password').unsafeUnwrap()
        }
      ],
      subnetIds: vpc.selectSubnets({
        subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS
      }).subnetIds,
      securityGroups: [brokerSecurityGroup.securityGroupId],
      logs: {
        general: true,
        audit: true
      },
      maintenanceWindowStartTime: {
        dayOfWeek: 'SUNDAY',
        timeOfDay: '03:00',
        timeZone: 'UTC'
      }
    })

    // Output broker endpoints
    new cdk.CfnOutput(this, 'BrokerEndpoint', {
      value: this.broker.attrOpenWireEndpoints[0] || 'N/A',
      description: 'Amazon MQ OpenWire endpoint'
    })

    new cdk.CfnOutput(this, 'BrokerConsoleUrl', {
      value: \`https://\${cdk.Aws.REGION}.console.aws.amazon.com/amazon-mq/home?region=\${cdk.Aws.REGION}#/brokers/\${this.broker.ref}\`,
      description: 'Amazon MQ console URL'
    })
  }
}
`

  return {
    filename: 'lib/amazonmq-construct.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate S3 batch processing configuration
 */
function generateS3BatchConfig (analysis: LogicAnalysis): CodeArtifact {
  const hasBatchPattern = analysis.patterns.some(
    p => p.type === 'batch_processing'
  )

  let code = `/**
 * S3 Batch Processing Configuration
 * Handles batch file synchronization between mainframe and AWS
 */

import * as cdk from 'aws-cdk-lib'
import * as s3 from 'aws-cdk-lib/aws-s3'
import * as lambda from 'aws-cdk-lib/aws-lambda'
import * as s3n from 'aws-cdk-lib/aws-s3-notifications'
import * as iam from 'aws-cdk-lib/aws-iam'
import { Construct } from 'constructs'

export interface S3BatchConstructProps {
  bucketName?: string
}

export class S3BatchConstruct extends Construct {
  public readonly inboundBucket: s3.Bucket
  public readonly outboundBucket: s3.Bucket
  public readonly processingFunction: lambda.Function

  constructor(scope: Construct, id: string, props: S3BatchConstructProps = {}) {
    super(scope, id)

    const { bucketName = 'cobol-batch-files' } = props

    // Inbound bucket for files from mainframe
    this.inboundBucket = new s3.Bucket(this, 'InboundBucket', {
      bucketName: \`\${bucketName}-inbound\`,
      encryption: s3.BucketEncryption.S3_MANAGED,
      enforceSSL: true,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      versioned: true,
      lifecycleRules: [
        {
          id: 'ArchiveOldFiles',
          enabled: true,
          transitions: [
            {
              storageClass: s3.StorageClass.GLACIER,
              transitionAfter: cdk.Duration.days(90)
            }
          ],
          expiration: cdk.Duration.days(365)
        }
      ]
    })

    // Outbound bucket for files to mainframe
    this.outboundBucket = new s3.Bucket(this, 'OutboundBucket', {
      bucketName: \`\${bucketName}-outbound\`,
      encryption: s3.BucketEncryption.S3_MANAGED,
      enforceSSL: true,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      versioned: true,
      lifecycleRules: [
        {
          id: 'CleanupProcessedFiles',
          enabled: true,
          expiration: cdk.Duration.days(30)
        }
      ]
    })

    // Lambda function to process batch files
    this.processingFunction = new lambda.Function(this, 'BatchProcessor', {
      runtime: lambda.Runtime.NODEJS_20_X,
      handler: 'index.handler',
      code: lambda.Code.fromInline(\`
        exports.handler = async (event) => {
          console.log('Processing batch file:', JSON.stringify(event, null, 2));
          
          // Extract S3 event details
          const record = event.Records[0];
          const bucket = record.s3.bucket.name;
          const key = record.s3.object.key;
          
          console.log(\\\`File uploaded: s3://\\\${bucket}/\\\${key}\\\`);
          
          // TODO: Implement batch file processing logic
          // 1. Download file from S3
          // 2. Parse COBOL-formatted data
          // 3. Transform to modern format
          // 4. Store results or trigger downstream processing
          
          return {
            statusCode: 200,
            body: JSON.stringify({ message: 'Batch file processed successfully' })
          };
        };
      \`),
      timeout: cdk.Duration.minutes(5),
      memorySize: 1024,
      environment: {
        INBOUND_BUCKET: this.inboundBucket.bucketName,
        OUTBOUND_BUCKET: this.outboundBucket.bucketName
      }
    })

    // Grant Lambda permissions to access buckets
    this.inboundBucket.grantRead(this.processingFunction)
    this.outboundBucket.grantReadWrite(this.processingFunction)

    // Trigger Lambda on file upload
    this.inboundBucket.addEventNotification(
      s3.EventType.OBJECT_CREATED,
      new s3n.LambdaDestination(this.processingFunction)
    )

    // Outputs
    new cdk.CfnOutput(this, 'InboundBucketName', {
      value: this.inboundBucket.bucketName,
      description: 'S3 bucket for inbound batch files from mainframe'
    })

    new cdk.CfnOutput(this, 'OutboundBucketName', {
      value: this.outboundBucket.bucketName,
      description: 'S3 bucket for outbound batch files to mainframe'
    })
  }
}
`

  return {
    filename: 'lib/s3-batch-construct.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate Step Functions workflows
 */
function generateStepFunctionsWorkflows (
  analysis: LogicAnalysis
): CodeArtifact[] {
  const artifacts: CodeArtifact[] = []

  // Generate CDK construct
  artifacts.push(generateStepFunctionsCDK(analysis))

  // Generate workflow definition
  artifacts.push(generateStepFunctionsDefinition(analysis))

  return artifacts
}

/**
 * Generate Step Functions CDK construct
 */
function generateStepFunctionsCDK (analysis: LogicAnalysis): CodeArtifact {
  let code = `/**
 * Step Functions Workflow for Batch Processing
 * Orchestrates batch processing operations
 */

import * as cdk from 'aws-cdk-lib'
import * as sfn from 'aws-cdk-lib/aws-stepfunctions'
import * as tasks from 'aws-cdk-lib/aws-stepfunctions-tasks'
import * as lambda from 'aws-cdk-lib/aws-lambda'
import * as logs from 'aws-cdk-lib/aws-logs'
import { Construct } from 'constructs'

export interface StepFunctionsConstructProps {
  lambdaFunctions: Record<string, lambda.Function>
}

export class BatchProcessingWorkflow extends Construct {
  public readonly stateMachine: sfn.StateMachine

  constructor(scope: Construct, id: string, props: StepFunctionsConstructProps) {
    super(scope, id)

    // Define workflow steps
    const validateInput = new tasks.LambdaInvoke(this, 'ValidateInput', {
      lambdaFunction: props.lambdaFunctions['validator'] || props.lambdaFunctions['MAIN'],
      outputPath: '$.Payload'
    })

    const processRecords = new tasks.LambdaInvoke(this, 'ProcessRecords', {
      lambdaFunction: props.lambdaFunctions['processor'] || props.lambdaFunctions['MAIN'],
      outputPath: '$.Payload'
    })

    const generateReport = new tasks.LambdaInvoke(this, 'GenerateReport', {
      lambdaFunction: props.lambdaFunctions['reporter'] || props.lambdaFunctions['MAIN'],
      outputPath: '$.Payload'
    })

    const handleError = new sfn.Fail(this, 'HandleError', {
      cause: 'Batch processing failed',
      error: 'ProcessingError'
    })

    const success = new sfn.Succeed(this, 'Success')

    // Define workflow
    const definition = validateInput
      .next(
        new sfn.Choice(this, 'ValidationCheck')
          .when(sfn.Condition.booleanEquals('$.valid', true), processRecords)
          .otherwise(handleError)
      )

    processRecords.next(
      new sfn.Choice(this, 'ProcessingCheck')
        .when(sfn.Condition.booleanEquals('$.success', true), generateReport)
        .otherwise(handleError)
    )

    generateReport.next(success)

    // Create log group
    const logGroup = new logs.LogGroup(this, 'WorkflowLogs', {
      logGroupName: '/aws/stepfunctions/batch-processing',
      retention: logs.RetentionDays.ONE_WEEK,
      removalPolicy: cdk.RemovalPolicy.DESTROY
    })

    // Create state machine
    this.stateMachine = new sfn.StateMachine(this, 'StateMachine', {
      stateMachineName: 'CobolBatchProcessing',
      definition,
      logs: {
        destination: logGroup,
        level: sfn.LogLevel.ALL
      },
      tracingEnabled: true
    })

    // Output
    new cdk.CfnOutput(this, 'StateMachineArn', {
      value: this.stateMachine.stateMachineArn,
      description: 'Step Functions state machine ARN'
    })
  }
}
`

  return {
    filename: 'lib/step-functions-construct.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate Step Functions workflow definition
 */
function generateStepFunctionsDefinition (
  analysis: LogicAnalysis
): CodeArtifact {
  const definition = {
    Comment: 'COBOL Batch Processing Workflow',
    StartAt: 'ValidateInput',
    States: {
      ValidateInput: {
        Type: 'Task',
        Resource: 'arn:aws:states:::lambda:invoke',
        Parameters: {
          FunctionName: '${ValidatorFunction}',
          'Payload.$': '$'
        },
        Next: 'ValidationCheck',
        Catch: [
          {
            ErrorEquals: ['States.ALL'],
            Next: 'HandleError'
          }
        ]
      },
      ValidationCheck: {
        Type: 'Choice',
        Choices: [
          {
            Variable: '$.Payload.valid',
            BooleanEquals: true,
            Next: 'ProcessRecords'
          }
        ],
        Default: 'HandleError'
      },
      ProcessRecords: {
        Type: 'Task',
        Resource: 'arn:aws:states:::lambda:invoke',
        Parameters: {
          FunctionName: '${ProcessorFunction}',
          'Payload.$': '$.Payload'
        },
        Next: 'ProcessingCheck',
        Catch: [
          {
            ErrorEquals: ['States.ALL'],
            Next: 'HandleError'
          }
        ]
      },
      ProcessingCheck: {
        Type: 'Choice',
        Choices: [
          {
            Variable: '$.Payload.success',
            BooleanEquals: true,
            Next: 'GenerateReport'
          }
        ],
        Default: 'HandleError'
      },
      GenerateReport: {
        Type: 'Task',
        Resource: 'arn:aws:states:::lambda:invoke',
        Parameters: {
          FunctionName: '${ReporterFunction}',
          'Payload.$': '$.Payload'
        },
        Next: 'Success',
        Catch: [
          {
            ErrorEquals: ['States.ALL'],
            Next: 'HandleError'
          }
        ]
      },
      HandleError: {
        Type: 'Fail',
        Cause: 'Batch processing failed',
        Error: 'ProcessingError'
      },
      Success: {
        Type: 'Succeed'
      }
    }
  }

  return {
    filename: 'workflows/batch-processing.json',
    content: JSON.stringify(definition, null, 2),
    language: 'json'
  }
}

/**
 * Generate VPC networking configuration
 */
function generateVPCConfig (enableDirectConnect: boolean): CodeArtifact {
  let code = `/**
 * VPC Networking Configuration
 * Secure networking for mainframe-to-cloud connectivity
 */

import * as cdk from 'aws-cdk-lib'
import * as ec2 from 'aws-cdk-lib/aws-ec2'
import { Construct } from 'constructs'

export interface VPCConstructProps {
  cidr?: string
  enableDirectConnect?: boolean
}

export class HybridVPCConstruct extends Construct {
  public readonly vpc: ec2.Vpc
  public readonly privateSubnets: ec2.ISubnet[]

  constructor(scope: Construct, id: string, props: VPCConstructProps = {}) {
    super(scope, id)

    const { cidr = '10.0.0.0/16', enableDirectConnect = ${enableDirectConnect} } = props

    // Create VPC
    this.vpc = new ec2.Vpc(this, 'VPC', {
      ipAddresses: ec2.IpAddresses.cidr(cidr),
      maxAzs: 2,
      natGateways: 2,
      subnetConfiguration: [
        {
          name: 'Public',
          subnetType: ec2.SubnetType.PUBLIC,
          cidrMask: 24
        },
        {
          name: 'Private',
          subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS,
          cidrMask: 24
        },
        {
          name: 'Isolated',
          subnetType: ec2.SubnetType.PRIVATE_ISOLATED,
          cidrMask: 24
        }
      ],
      enableDnsHostnames: true,
      enableDnsSupport: true
    })

    this.privateSubnets = this.vpc.privateSubnets

    // VPC Flow Logs
    const flowLogRole = new cdk.aws_iam.Role(this, 'FlowLogRole', {
      assumedBy: new cdk.aws_iam.ServicePrincipal('vpc-flow-logs.amazonaws.com')
    })

    const flowLogGroup = new cdk.aws_logs.LogGroup(this, 'FlowLogGroup', {
      logGroupName: '/aws/vpc/flowlogs',
      retention: cdk.aws_logs.RetentionDays.ONE_WEEK,
      removalPolicy: cdk.RemovalPolicy.DESTROY
    })

    new ec2.FlowLog(this, 'FlowLog', {
      resourceType: ec2.FlowLogResourceType.fromVpc(this.vpc),
      destination: ec2.FlowLogDestination.toCloudWatchLogs(flowLogGroup, flowLogRole)
    })

`

  if (enableDirectConnect) {
    code += `
    // Direct Connect Gateway (placeholder - requires manual setup)
    // Note: Direct Connect requires physical connection setup with AWS
    // This is a placeholder for the configuration
    
    // Virtual Private Gateway for Direct Connect
    const vpnGateway = new ec2.CfnVPNGateway(this, 'VPNGateway', {
      type: 'ipsec.1',
      amazonSideAsn: 64512
    })

    new ec2.CfnVPCGatewayAttachment(this, 'VPNGatewayAttachment', {
      vpcId: this.vpc.vpcId,
      vpnGatewayId: vpnGateway.ref
    })

    // Customer Gateway (configure with mainframe IP)
    const customerGateway = new ec2.CfnCustomerGateway(this, 'CustomerGateway', {
      bgpAsn: 65000,
      ipAddress: '203.0.113.1', // Replace with actual mainframe public IP
      type: 'ipsec.1'
    })

    // VPN Connection
    const vpnConnection = new ec2.CfnVPNConnection(this, 'VPNConnection', {
      type: 'ipsec.1',
      customerGatewayId: customerGateway.ref,
      vpnGatewayId: vpnGateway.ref,
      staticRoutesOnly: false
    })

    new cdk.CfnOutput(this, 'VPNConnectionId', {
      value: vpnConnection.ref,
      description: 'VPN Connection ID for mainframe connectivity'
    })
`
  } else {
    code += `
    // VPN Gateway for site-to-site VPN (alternative to Direct Connect)
    // Configure this with your mainframe network details
    
    new cdk.CfnOutput(this, 'VPCNote', {
      value: 'Configure VPN or Direct Connect for mainframe connectivity',
      description: 'Next steps for hybrid connectivity'
    })
`
  }

  code += `
    // Outputs
    new cdk.CfnOutput(this, 'VPCId', {
      value: this.vpc.vpcId,
      description: 'VPC ID',
      exportName: 'HybridVPCId'
    })

    new cdk.CfnOutput(this, 'PrivateSubnetIds', {
      value: this.vpc.privateSubnets.map(s => s.subnetId).join(','),
      description: 'Private subnet IDs'
    })
  }
}
`

  return {
    filename: 'lib/vpc-construct.ts',
    content: code,
    language: 'typescript'
  }
}

/**
 * Generate hybrid monitoring configuration
 */
function generateHybridMonitoring (): CodeArtifact {
  let code = `/**
 * Hybrid Monitoring Configuration
 * Monitors data synchronization between mainframe and AWS
 */

import * as cdk from 'aws-cdk-lib'
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch'
import * as sns from 'aws-cdk-lib/aws-sns'
import * as subscriptions from 'aws-cdk-lib/aws-sns-subscriptions'
import * as cloudwatch_actions from 'aws-cdk-lib/aws-cloudwatch-actions'
import { Construct } from 'constructs'

export interface HybridMonitoringProps {
  alarmEmail?: string
}

export class HybridMonitoringConstruct extends Construct {
  public readonly dashboard: cloudwatch.Dashboard
  public readonly alarmTopic: sns.Topic

  constructor(scope: Construct, id: string, props: HybridMonitoringProps = {}) {
    super(scope, id)

    const { alarmEmail } = props

    // SNS topic for alarms
    this.alarmTopic = new sns.Topic(this, 'AlarmTopic', {
      displayName: 'Hybrid Infrastructure Alarms',
      topicName: 'cobol-hybrid-alarms'
    })

    if (alarmEmail) {
      this.alarmTopic.addSubscription(
        new subscriptions.EmailSubscription(alarmEmail)
      )
    }

    // CloudWatch Dashboard
    this.dashboard = new cloudwatch.Dashboard(this, 'Dashboard', {
      dashboardName: 'CobolHybridInfrastructure'
    })

    // Data synchronization metrics
    const syncSuccessMetric = new cloudwatch.Metric({
      namespace: 'COBRA/Hybrid',
      metricName: 'SyncSuccess',
      statistic: 'Sum',
      period: cdk.Duration.minutes(5)
    })

    const syncFailureMetric = new cloudwatch.Metric({
      namespace: 'COBRA/Hybrid',
      metricName: 'SyncFailure',
      statistic: 'Sum',
      period: cdk.Duration.minutes(5)
    })

    const syncLatencyMetric = new cloudwatch.Metric({
      namespace: 'COBRA/Hybrid',
      metricName: 'SyncLatency',
      statistic: 'Average',
      period: cdk.Duration.minutes(5)
    })

    // Add widgets to dashboard
    this.dashboard.addWidgets(
      new cloudwatch.GraphWidget({
        title: 'Data Synchronization Status',
        left: [syncSuccessMetric, syncFailureMetric]
      }),
      new cloudwatch.GraphWidget({
        title: 'Synchronization Latency',
        left: [syncLatencyMetric]
      })
    )

    // Alarms
    const syncFailureAlarm = new cloudwatch.Alarm(this, 'SyncFailureAlarm', {
      metric: syncFailureMetric,
      threshold: 5,
      evaluationPeriods: 2,
      alarmDescription: 'Alert when sync failures exceed threshold',
      alarmName: 'CobolSyncFailures'
    })

    syncFailureAlarm.addAlarmAction(
      new cloudwatch_actions.SnsAction(this.alarmTopic)
    )

    const syncLatencyAlarm = new cloudwatch.Alarm(this, 'SyncLatencyAlarm', {
      metric: syncLatencyMetric,
      threshold: 60000, // 60 seconds
      evaluationPeriods: 3,
      alarmDescription: 'Alert when sync latency is high',
      alarmName: 'CobolSyncLatency'
    })

    syncLatencyAlarm.addAlarmAction(
      new cloudwatch_actions.SnsAction(this.alarmTopic)
    )

    // Outputs
    new cdk.CfnOutput(this, 'DashboardUrl', {
      value: \`https://console.aws.amazon.com/cloudwatch/home?region=\${cdk.Aws.REGION}#dashboards:name=\${this.dashboard.dashboardName}\`,
      description: 'CloudWatch Dashboard URL'
    })

    new cdk.CfnOutput(this, 'AlarmTopicArn', {
      value: this.alarmTopic.topicArn,
      description: 'SNS topic ARN for alarms'
    })
  }
}
`

  return {
    filename: 'lib/hybrid-monitoring-construct.ts',
    content: code,
    language: 'typescript'
  }
}
