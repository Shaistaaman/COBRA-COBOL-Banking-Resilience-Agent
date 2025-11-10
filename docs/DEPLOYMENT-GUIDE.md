# COBRA Deployment Guide

Complete guide for deploying COBRA-generated AWS infrastructure, from free tier to production.

## Table of Contents

1. [Deployment Options](#deployment-options)
2. [Free Tier Deployment](#free-tier-deployment)
3. [Production Deployment](#production-deployment)
4. [Monitoring and Maintenance](#monitoring-and-maintenance)
5. [Cost Optimization](#cost-optimization)
6. [Troubleshooting](#troubleshooting)

## Deployment Options

### Option 1: Local Only (Zero Cost)

**Cost**: $0/month  
**Use Case**: Development, demos, proof-of-concept

**What Runs Locally**:

- MCP server for Kiro integration
- COBOL parser and analyzer
- Code generators
- Demo web interface (localhost)

**No AWS Deployment Required**

### Option 2: Static Demo Site (Zero Cost)

**Cost**: $0/month  
**Use Case**: Public demo, portfolio, stakeholder sharing

**Hosting Options**:

- GitHub Pages (free, unlimited)
- Vercel (free tier: 100GB bandwidth)
- Netlify (free tier: 100GB bandwidth)

**What's Deployed**:

- React frontend (static files)
- Example COBOL snippets
- Architecture diagrams
- Documentation

**No Backend Required** (all processing done client-side or pre-generated)

### Option 3: AWS Free Tier (Zero Cost for 12 Months)

**Cost**: $0/month (within free tier limits)  
**Use Case**: Low-traffic production, pilot projects

**Free Tier Limits**:

- Lambda: 1M requests/month + 400,000 GB-seconds
- API Gateway: 1M requests/month (12 months)
- CloudWatch: 10 custom metrics + 5GB logs
- S3: 5GB storage + 20,000 GET requests (12 months)

**What's Deployed**:

- Lambda functions for business logic
- API Gateway REST API
- CloudWatch logs and metrics
- IAM roles and policies

### Option 4: Full Production (Paid)

**Cost**: $50-300/month  
**Use Case**: Enterprise production, high-traffic applications

**What's Deployed**:

- All Free Tier components
- Amazon MQ for hybrid integration
- VPC with Direct Connect/VPN
- ECS Fargate for complex workloads
- Step Functions for batch processing
- DynamoDB for data storage
- CloudWatch dashboards and alarms
- AWS WAF for security

## Free Tier Deployment

### Prerequisites

```bash
# Install AWS CLI
# macOS
brew install awscli

# Linux
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip awscliv2.zip
sudo ./aws/install

# Windows
# Download from https://aws.amazon.com/cli/

# Verify installation
aws --version
```

```bash
# Install AWS CDK
npm install -g aws-cdk

# Verify installation
cdk --version
```

```bash
# Configure AWS credentials
aws configure

# Enter:
# - AWS Access Key ID
# - AWS Secret Access Key
# - Default region (e.g., us-east-1)
# - Default output format (json)
```

### Step 1: Review Generated Code

```bash
# Navigate to generated project
cd generated/interest-calculation-example

# Review generated files
ls -la

# Key files:
# - lambda-handler.ts: Lambda function code
# - cdk-stack.ts: Infrastructure definition
# - package.json: Dependencies
# - README.md: Deployment instructions
```

### Step 2: Install Dependencies

```bash
# Install Lambda dependencies
npm install

# Expected output:
# added 50 packages in 10s
```

### Step 3: Build Lambda Code

```bash
# Compile TypeScript to JavaScript
npm run build

# Expected output:
# ✓ Lambda code compiled successfully
```

### Step 4: Bootstrap CDK (First Time Only)

```bash
# Bootstrap CDK in your AWS account
cdk bootstrap

# This creates:
# - S3 bucket for CDK assets
# - IAM roles for CDK operations
# - CloudFormation stack

# Expected output:
# ✅  Environment aws://123456789012/us-east-1 bootstrapped
```

### Step 5: Review Changes

```bash
# Preview what will be deployed
cdk diff

# Shows:
# - Resources to be created
# - IAM permissions
# - Estimated costs
```

### Step 6: Deploy

```bash
# Deploy the stack
cdk deploy

# Confirm when prompted:
# Do you wish to deploy these changes (y/n)? y

# Deployment takes 2-5 minutes

# Expected output:
# ✅  InterestCalculationStack

# Outputs:
# InterestCalculationStack.APIEndpoint = https://xxxxx.execute-api.us-east-1.amazonaws.com/prod/
# InterestCalculationStack.APIKey = xxxxxxxxxxxxxx
# InterestCalculationStack.LambdaFunctionName = InterestCalculationStack-InterestCalculationFunction-xxxxx
```

### Step 7: Test Deployed API

```bash
# Get API endpoint from CDK output
API_URL="https://xxxxx.execute-api.us-east-1.amazonaws.com/prod"

# Get API key from AWS Console
# Navigate to API Gateway > API Keys > interest-calculation-key
API_KEY="your-api-key-value"

# Test the API
curl -X POST $API_URL/interest \
  -H "Content-Type: application/json" \
  -H "x-api-key: $API_KEY" \
  -d '{
    "accountNumber": "1234567890",
    "accountBalance": "10000.00",
    "interestRate": "5.0",
    "daysInPeriod": "30"
  }'

# Expected response:
# {
#   "accountNumber": "1234567890",
#   "originalBalance": "10000.00",
#   "interestRate": "5.0000",
#   "daysInPeriod": 30,
#   "interestAmount": "41.10",
#   "newBalance": "10041.10",
#   "calculationDate": "2024-11-08T12:00:00.000Z",
#   "status": "success"
# }
```

### Step 8: Monitor Free Tier Usage

```bash
# Check Lambda invocations
aws cloudwatch get-metric-statistics \
  --namespace AWS/Lambda \
  --metric-name Invocations \
  --dimensions Name=FunctionName,Value=InterestCalculationStack-InterestCalculationFunction-xxxxx \
  --start-time 2024-11-01T00:00:00Z \
  --end-time 2024-11-08T23:59:59Z \
  --period 86400 \
  --statistics Sum

# Check API Gateway requests
aws cloudwatch get-metric-statistics \
  --namespace AWS/ApiGateway \
  --metric-name Count \
  --dimensions Name=ApiName,Value=InterestCalculationAPI \
  --start-time 2024-11-01T00:00:00Z \
  --end-time 2024-11-08T23:59:59Z \
  --period 86400 \
  --statistics Sum
```

### Step 9: Set Up Billing Alerts

```bash
# Create billing alarm (requires CloudWatch in us-east-1)
aws cloudwatch put-metric-alarm \
  --alarm-name "FreeTierExceeded" \
  --alarm-description "Alert when estimated charges exceed $1" \
  --metric-name EstimatedCharges \
  --namespace AWS/Billing \
  --statistic Maximum \
  --period 21600 \
  --evaluation-periods 1 \
  --threshold 1.0 \
  --comparison-operator GreaterThanThreshold \
  --dimensions Name=Currency,Value=USD \
  --region us-east-1
```

## Production Deployment

### Prerequisites

- AWS account with production access
- VPC configured (if using hybrid architecture)
- Domain name (optional, for custom API endpoint)
- SSL certificate (optional, for custom domain)

### Step 1: Review Production Requirements

**Checklist**:

- [ ] High availability (multi-AZ deployment)
- [ ] Disaster recovery (multi-region)
- [ ] Security hardening (WAF, encryption)
- [ ] Monitoring and alerting
- [ ] Backup and retention policies
- [ ] Compliance requirements (SOX, PCI-DSS, GLBA)

### Step 2: Customize CDK Stack

Edit `cdk-stack.ts` for production:

```typescript
// Increase Lambda memory and timeout
const interestCalculationFunction = new lambda.Function(this, 'Function', {
  runtime: lambda.Runtime.NODEJS_20_X,
  handler: 'lambda-handler.handler',
  code: lambda.Code.fromAsset('lambda'),
  timeout: cdk.Duration.seconds(60), // Increased from 30
  memorySize: 512, // Increased from 256
  reservedConcurrentExecutions: 100, // Reserve capacity
  environment: {
    NODE_ENV: 'production',
    LOG_LEVEL: 'info',
    ENABLE_XRAY: 'true' // Enable X-Ray tracing
  },
  tracing: lambda.Tracing.ACTIVE
})

// Add DynamoDB for state management
const table = new dynamodb.Table(this, 'TransactionHistory', {
  partitionKey: { name: 'accountNumber', type: dynamodb.AttributeType.STRING },
  sortKey: { name: 'timestamp', type: dynamodb.AttributeType.STRING },
  billingMode: dynamodb.BillingMode.PAY_PER_REQUEST,
  pointInTimeRecovery: true,
  encryption: dynamodb.TableEncryption.AWS_MANAGED,
  replicationRegions: ['us-west-2'] // Multi-region
})

// Add Step Functions for batch processing
const stateMachine = new sfn.StateMachine(this, 'BatchProcessor', {
  definition: /* ... */,
  timeout: cdk.Duration.hours(1)
})

// Add Amazon MQ for hybrid integration
const broker = new amazonmq.CfnBroker(this, 'MessageBroker', {
  brokerName: 'cobra-mq',
  engineType: 'ACTIVEMQ',
  engineVersion: '5.17',
  hostInstanceType: 'mq.t3.micro',
  deploymentMode: 'ACTIVE_STANDBY_MULTI_AZ',
  users: [/* ... */]
})
```

### Step 3: Deploy to Production

```bash
# Set production environment
export AWS_PROFILE=production
export CDK_DEFAULT_ACCOUNT=123456789012
export CDK_DEFAULT_REGION=us-east-1

# Review changes
cdk diff --profile production

# Deploy with approval
cdk deploy --profile production --require-approval broadening

# Monitor deployment
aws cloudformation describe-stacks \
  --stack-name InterestCalculationStack \
  --profile production
```

### Step 4: Configure Custom Domain

```bash
# Create custom domain in API Gateway
aws apigateway create-domain-name \
  --domain-name api.yourbank.com \
  --certificate-arn arn:aws:acm:us-east-1:123456789012:certificate/xxxxx \
  --endpoint-configuration types=REGIONAL

# Create base path mapping
aws apigateway create-base-path-mapping \
  --domain-name api.yourbank.com \
  --rest-api-id xxxxx \
  --stage prod

# Update DNS (Route 53 or your DNS provider)
# Create CNAME record: api.yourbank.com -> xxxxx.execute-api.us-east-1.amazonaws.com
```

### Step 5: Enable Advanced Monitoring

```bash
# Enable X-Ray tracing
aws lambda update-function-configuration \
  --function-name InterestCalculationStack-InterestCalculationFunction-xxxxx \
  --tracing-config Mode=Active

# Create CloudWatch dashboard
aws cloudwatch put-dashboard \
  --dashboard-name InterestCalculation-Production \
  --dashboard-body file://dashboard.json

# Set up alarms
aws cloudwatch put-metric-alarm \
  --alarm-name "Lambda-High-Error-Rate" \
  --alarm-description "Alert when error rate exceeds 5%" \
  --metric-name Errors \
  --namespace AWS/Lambda \
  --statistic Average \
  --period 300 \
  --evaluation-periods 2 \
  --threshold 0.05 \
  --comparison-operator GreaterThanThreshold \
  --dimensions Name=FunctionName,Value=InterestCalculationStack-InterestCalculationFunction-xxxxx
```

## Monitoring and Maintenance

### CloudWatch Dashboards

Access the dashboard:

```
https://console.aws.amazon.com/cloudwatch/home?region=us-east-1#dashboards:name=InterestCalculation
```

**Key Metrics**:

- Lambda invocations (count)
- Lambda errors (count, percentage)
- Lambda duration (p50, p95, p99)
- API Gateway requests (count)
- API Gateway 4XX errors (count)
- API Gateway 5XX errors (count)
- API Gateway latency (p50, p95, p99)

### Log Analysis

```bash
# View Lambda logs
aws logs tail /aws/lambda/InterestCalculationStack-InterestCalculationFunction-xxxxx --follow

# Search for errors
aws logs filter-log-events \
  --log-group-name /aws/lambda/InterestCalculationStack-InterestCalculationFunction-xxxxx \
  --filter-pattern "ERROR"

# Query with CloudWatch Logs Insights
aws logs start-query \
  --log-group-name /aws/lambda/InterestCalculationStack-InterestCalculationFunction-xxxxx \
  --start-time $(date -u -d '1 hour ago' +%s) \
  --end-time $(date -u +%s) \
  --query-string 'fields @timestamp, @message | filter @message like /ERROR/ | sort @timestamp desc | limit 20'
```

### Performance Optimization

**Lambda Cold Start Optimization**:

```typescript
// Use provisioned concurrency
const version = interestCalculationFunction.currentVersion
const alias = new lambda.Alias(this, 'ProdAlias', {
  aliasName: 'prod',
  version
})

alias
  .addAutoScaling({
    minCapacity: 5,
    maxCapacity: 100
  })
  .scaleOnUtilization({
    utilizationTarget: 0.7
  })
```

**API Gateway Caching**:

```typescript
const api = new apigateway.RestApi(this, 'API', {
  deployOptions: {
    cachingEnabled: true,
    cacheClusterEnabled: true,
    cacheClusterSize: '0.5', // 0.5 GB cache
    cacheTtl: cdk.Duration.minutes(5)
  }
})
```

### Backup and Recovery

**Lambda Function Backup**:

```bash
# Export Lambda function code
aws lambda get-function \
  --function-name InterestCalculationStack-InterestCalculationFunction-xxxxx \
  --query 'Code.Location' \
  --output text | xargs curl -o lambda-backup.zip
```

**DynamoDB Backup**:

```bash
# Enable point-in-time recovery
aws dynamodb update-continuous-backups \
  --table-name TransactionHistory \
  --point-in-time-recovery-specification PointInTimeRecoveryEnabled=true

# Create on-demand backup
aws dynamodb create-backup \
  --table-name TransactionHistory \
  --backup-name TransactionHistory-$(date +%Y%m%d)
```

## Cost Optimization

### Monitor Costs

```bash
# Get cost and usage
aws ce get-cost-and-usage \
  --time-period Start=2024-11-01,End=2024-11-08 \
  --granularity DAILY \
  --metrics BlendedCost \
  --group-by Type=SERVICE

# Get cost forecast
aws ce get-cost-forecast \
  --time-period Start=2024-11-08,End=2024-12-08 \
  --metric BLENDED_COST \
  --granularity MONTHLY
```

### Optimization Strategies

**1. Right-Size Lambda**:

```bash
# Analyze Lambda performance
aws lambda get-function-configuration \
  --function-name InterestCalculationStack-InterestCalculationFunction-xxxxx

# Adjust memory based on actual usage
# Lower memory = lower cost (if sufficient)
```

**2. Use Reserved Capacity** (for predictable workloads):

```bash
# Purchase Lambda reserved concurrency
# Savings: up to 40% vs on-demand
```

**3. Implement Caching**:

- API Gateway caching for repeated requests
- Lambda@Edge for global distribution
- DynamoDB DAX for database caching

**4. Optimize Data Transfer**:

- Use VPC endpoints to avoid NAT Gateway costs
- Compress API responses
- Use CloudFront for static content

### Cost Comparison

| Component       | Free Tier | Low Traffic | Medium Traffic | High Traffic |
| --------------- | --------- | ----------- | -------------- | ------------ |
| **Lambda**      | $0        | $5          | $20            | $100         |
| **API Gateway** | $0        | $3.50       | $35            | $350         |
| **CloudWatch**  | $0        | $2          | $10            | $50          |
| **DynamoDB**    | $0        | $5          | $25            | $200         |
| **WAF**         | N/A       | $5          | $5             | $5           |
| **Total**       | **$0**    | **$20.50**  | **$95**        | **$705**     |

## Troubleshooting

### Deployment Failures

**Issue**: CDK deploy fails with "Resource already exists"

**Solution**:

```bash
# Delete the stack and redeploy
cdk destroy
cdk deploy
```

**Issue**: Lambda function timeout

**Solution**:

```typescript
// Increase timeout in cdk-stack.ts
timeout: cdk.Duration.seconds(60)
```

**Issue**: API Gateway 403 Forbidden

**Solution**:

```bash
# Check API key
aws apigateway get-api-keys --include-values

# Verify usage plan
aws apigateway get-usage-plans
```

### Runtime Errors

**Issue**: Lambda function errors

**Solution**:

```bash
# Check CloudWatch Logs
aws logs tail /aws/lambda/your-function-name --follow

# Enable detailed logging
# Set LOG_LEVEL=debug in Lambda environment variables
```

**Issue**: High latency

**Solution**:

- Enable X-Ray tracing to identify bottlenecks
- Increase Lambda memory (more CPU)
- Use provisioned concurrency
- Implement caching

### Cost Overruns

**Issue**: Unexpected AWS charges

**Solution**:

```bash
# Check cost breakdown
aws ce get-cost-and-usage \
  --time-period Start=2024-11-01,End=2024-11-08 \
  --granularity DAILY \
  --metrics BlendedCost \
  --group-by Type=SERVICE

# Set up billing alerts
aws budgets create-budget \
  --account-id 123456789012 \
  --budget file://budget.json
```

## Next Steps

After successful deployment:

1. **Test Thoroughly**: Run integration tests against deployed API
2. **Monitor Closely**: Watch CloudWatch metrics for first 24 hours
3. **Optimize**: Adjust Lambda memory and timeout based on actual usage
4. **Document**: Update runbooks with deployment-specific details
5. **Plan DR**: Set up multi-region deployment for disaster recovery

---

**Deployment Complete!** 🚀

Your COBOL banking logic is now running on AWS with modern cloud-native infrastructure.
