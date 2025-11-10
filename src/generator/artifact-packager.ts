/**
 * Artifact Packager
 * Creates ZIP archives of generated code with deployment instructions
 */

import type { CodeArtifact } from './types.js'
import type { LogicAnalysis } from '../mcp-server/types.js'

export interface PackagedArtifact {
  filename: string
  content: string // Base64 encoded ZIP content
  size: number
  files: string[]
  downloadUrl?: string
}

export interface PackagingOptions {
  includeReadme?: boolean
  includeDeploymentScripts?: boolean
  includeDependencies?: boolean
  format?: 'zip' | 'tar.gz'
}

/**
 * Package generated artifacts into a downloadable archive
 */
export function packageArtifacts (
  artifacts: CodeArtifact[],
  analysis: LogicAnalysis,
  options: PackagingOptions = {}
): PackagedArtifact {
  const {
    includeReadme = true,
    includeDeploymentScripts = true,
    includeDependencies = true
  } = options

  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
  const archiveName = `${programName.toLowerCase()}-aws-${timestamp}.zip`

  // Organize artifacts by type
  const organizedArtifacts = organizeArtifacts(artifacts)

  // Add README if requested
  if (includeReadme) {
    organizedArtifacts.push({
      filename: 'README.md',
      content: generatePackageReadme(analysis, artifacts),
      language: 'markdown'
    })
  }

  // Add deployment scripts if requested
  if (includeDeploymentScripts) {
    organizedArtifacts.push(...generateDeploymentScripts(analysis))
  }

  // Add package.json files if not already present
  if (includeDependencies) {
    organizedArtifacts.push(...ensureDependencyFiles(artifacts))
  }

  // Create file list
  const fileList = organizedArtifacts.map(a => a.filename)

  // In a real implementation, this would create an actual ZIP file
  // For now, we'll create a manifest of what would be in the ZIP
  const manifest = createArchiveManifest(organizedArtifacts)

  return {
    filename: archiveName,
    content: Buffer.from(manifest).toString('base64'),
    size: manifest.length,
    files: fileList,
    downloadUrl: `/api/download/${programName.toLowerCase()}/${archiveName}`
  }
}

/**
 * Organize artifacts into proper directory structure
 */
function organizeArtifacts (artifacts: CodeArtifact[]): CodeArtifact[] {
  const organized: CodeArtifact[] = []

  for (const artifact of artifacts) {
    let targetPath = artifact.filename

    // Organize Lambda files
    if (
      artifact.filename.includes('handler') ||
      artifact.filename.includes('types') ||
      artifact.filename.includes('validation')
    ) {
      if (!targetPath.startsWith('lambda/')) {
        targetPath = `lambda/src/${artifact.filename}`
      }
    }

    // Organize CDK files
    if (
      artifact.filename.includes('stack') ||
      artifact.filename.includes('app')
    ) {
      if (!targetPath.startsWith('lib/') && !targetPath.startsWith('bin/')) {
        // Already has proper path from generator
      }
    }

    // Organize API Gateway files
    if (
      artifact.filename.includes('openapi') ||
      artifact.filename.includes('api-gateway')
    ) {
      if (!targetPath.startsWith('api/')) {
        targetPath = `api/${artifact.filename}`
      }
    }

    // Organize configuration files
    if (
      artifact.filename === 'package.json' ||
      artifact.filename === 'tsconfig.json' ||
      artifact.filename === 'cdk.json'
    ) {
      // Keep at root or in appropriate subdirectory
      if (artifact.content.includes('"handler"')) {
        targetPath = `lambda/${artifact.filename}`
      }
    }

    organized.push({
      ...artifact,
      filename: targetPath
    })
  }

  return organized
}

/**
 * Generate comprehensive README for the package
 */
function generatePackageReadme (
  analysis: LogicAnalysis,
  artifacts: CodeArtifact[]
): string {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  return `# ${programName} - AWS Modernization Package

## Overview

This package contains complete AWS infrastructure code generated from COBOL program **${programName}**.

**Generated**: ${new Date().toISOString()}
**Business Rules**: ${analysis.businessRules.length}
**Banking Patterns**: ${analysis.patterns.length}
**Entry Points**: ${analysis.entryPoints.length}

## Package Contents

### Lambda Functions (\`lambda/\`)
- \`src/handler.ts\` - Main Lambda handler with business logic
- \`src/types.ts\` - TypeScript type definitions
- \`src/validation.ts\` - Input validation logic
- \`package.json\` - Node.js dependencies
- \`tsconfig.json\` - TypeScript configuration

### CDK Infrastructure (\`lib/\`, \`bin/\`)
- \`lib/stack.ts\` - Main CDK stack with all AWS resources
- \`bin/app.ts\` - CDK application entry point
- \`cdk.json\` - CDK configuration
- \`package.json\` - CDK dependencies

### API Gateway (\`api/\`)
- \`openapi.json\` - OpenAPI 3.0 specification
- \`api-gateway-construct.ts\` - CDK construct for API Gateway

### Deployment Scripts (\`scripts/\`)
- \`deploy.sh\` - Automated deployment script
- \`test-local.sh\` - Local testing script
- \`cleanup.sh\` - Resource cleanup script

## Quick Start

### Prerequisites

\`\`\`bash
# Install Node.js 20.x
# Install AWS CLI and configure credentials
# Install AWS CDK
npm install -g aws-cdk
\`\`\`

### Deploy to AWS

\`\`\`bash
# Run automated deployment
chmod +x scripts/deploy.sh
./scripts/deploy.sh

# Or deploy manually
npm install
cd lambda && npm install && npm run build && cd ..
cdk bootstrap  # First time only
cdk deploy
\`\`\`

### Test Locally

\`\`\`bash
# Run local tests
chmod +x scripts/test-local.sh
./scripts/test-local.sh
\`\`\`

## Architecture

\`\`\`
┌─────────────┐
│   Client    │
└──────┬──────┘
       │
       ▼
┌─────────────────┐
│  API Gateway    │
│  (REST API)     │
└──────┬──────────┘
       │
       ▼
┌─────────────────┐
│ Lambda Function │
│ (${programName})  │
└──────┬──────────┘
       │
       ├──────────────────┐
       │                  │
       ▼                  ▼
┌──────────────┐   ┌──────────────┐
│  CloudWatch  │   │   DynamoDB   │
│    Logs      │   │  (Optional)  │
└──────────────┘   └──────────────┘
\`\`\`

## Business Logic

${analysis.businessRules
  .slice(0, 5)
  .map(
    (rule, idx) => `
### ${idx + 1}. ${rule.description}
- **Type**: ${rule.type}
- **Source**: Lines ${rule.cobolSource.startLine}-${rule.cobolSource.endLine}
- **Inputs**: ${rule.inputs.map(i => i.name).join(', ')}
- **Outputs**: ${rule.outputs.map(o => o.name).join(', ')}
`
  )
  .join('\n')}

${
  analysis.patterns.length > 0
    ? `
## Banking Patterns

${analysis.patterns
  .map(
    pattern => `
### ${pattern.type.replace(/_/g, ' ').toUpperCase()}
- **Confidence**: ${(pattern.confidence * 100).toFixed(0)}%
- **Description**: ${pattern.description}
- **Location**: Lines ${pattern.location.startLine}-${pattern.location.endLine}
`
  )
  .join('\n')}
`
    : ''
}

## API Endpoints

${analysis.entryPoints
  .map(
    ep => `
### POST /${ep.name.toLowerCase()}

Execute ${ep.name} operation

**Request Body**:
\`\`\`json
{
${ep.parameters.map(p => `  "${p.name}": "value"`).join(',\n')}
}
\`\`\`

**Response**:
\`\`\`json
{
  "success": true,
  "message": "Operation completed successfully",
  "data": {}
}
\`\`\`
`
  )
  .join('\n')}

## Configuration

### Environment Variables

Set these in Lambda function configuration:
- \`NODE_ENV\`: \`production\`
- \`LOG_LEVEL\`: \`info\` or \`debug\`

### IAM Permissions

The Lambda execution role requires:
- \`logs:CreateLogGroup\`
- \`logs:CreateLogStream\`
- \`logs:PutLogEvents\`
- \`dynamodb:GetItem\` (if using DynamoDB)
- \`dynamodb:PutItem\` (if using DynamoDB)

## Monitoring

### CloudWatch Logs

\`\`\`bash
# View Lambda logs
aws logs tail /aws/lambda/${programName} --follow
\`\`\`

### CloudWatch Metrics

Access the dashboard: **CobolModernizationDashboard**

Key metrics:
- Invocations
- Errors
- Duration
- Throttles

### Alarms

Set up alarms for:
- Error rate > 5%
- Duration > 3 seconds (p95)
- Throttles > 0

## Cost Estimation

### AWS Free Tier (First 12 Months)
- Lambda: 1M requests/month free
- API Gateway: 1M requests/month free
- CloudWatch: 10 custom metrics free

### Beyond Free Tier
- Lambda: $0.20 per 1M requests
- API Gateway: $3.50 per 1M requests
- CloudWatch: $0.30 per metric/month

**Estimated cost for 100K requests/month**: $5-10

## Security

### Authentication
- API Gateway uses AWS IAM authentication
- Configure Cognito for user authentication if needed

### Encryption
- TLS 1.2+ for data in transit
- KMS encryption for sensitive data at rest
- S3 server-side encryption enabled

### Audit Logging
- All requests logged with timestamp, user, input, output
- Logs retained for 7 days (configurable)

## Troubleshooting

### Common Issues

**Lambda Timeout**
- Increase timeout in \`lib/stack.ts\`: \`timeout: cdk.Duration.seconds(60)\`

**Permission Denied**
- Check IAM role permissions in CloudWatch Logs
- Verify API Gateway resource policies

**Cold Start Latency**
- Consider provisioned concurrency for critical functions
- Optimize Lambda package size

### Debug Mode

Enable debug logging:
\`\`\`bash
aws lambda update-function-configuration \\
  --function-name ${programName} \\
  --environment Variables={LOG_LEVEL=debug}
\`\`\`

## Cleanup

Remove all deployed resources:

\`\`\`bash
chmod +x scripts/cleanup.sh
./scripts/cleanup.sh

# Or manually
cdk destroy
\`\`\`

## Support

- **AWS Documentation**: https://docs.aws.amazon.com/
- **CDK Documentation**: https://docs.aws.amazon.com/cdk/
- **Lambda Documentation**: https://docs.aws.amazon.com/lambda/

## License

Generated by COBRA (COBOL Banking Resilience Agent)

---

**Note**: This code was automatically generated from COBOL source. Review and test thoroughly before deploying to production.
`
}

/**
 * Generate deployment scripts
 */
function generateDeploymentScripts (analysis: LogicAnalysis): CodeArtifact[] {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  return [
    {
      filename: 'scripts/deploy.sh',
      content: generateDeployScript(programName),
      language: 'bash'
    },
    {
      filename: 'scripts/test-local.sh',
      content: generateTestScript(programName),
      language: 'bash'
    },
    {
      filename: 'scripts/cleanup.sh',
      content: generateCleanupScript(),
      language: 'bash'
    }
  ]
}

/**
 * Generate deployment script
 */
function generateDeployScript (programName: string): string {
  return `#!/bin/bash
# Automated deployment script for ${programName}
# Generated by COBRA

set -e

echo "🚀 Starting deployment of ${programName}..."

# Check prerequisites
echo "📋 Checking prerequisites..."

if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed"
    exit 1
fi

if ! command -v aws &> /dev/null; then
    echo "❌ AWS CLI is not installed"
    exit 1
fi

if ! command -v cdk &> /dev/null; then
    echo "❌ AWS CDK is not installed. Install with: npm install -g aws-cdk"
    exit 1
fi

# Check AWS credentials
echo "🔐 Checking AWS credentials..."
if ! aws sts get-caller-identity &> /dev/null; then
    echo "❌ AWS credentials not configured"
    exit 1
fi

# Install root dependencies
echo "📦 Installing CDK dependencies..."
npm install

# Build and package Lambda function
echo "🔨 Building Lambda function..."
cd lambda
npm install
npm run build
cd ..

# Bootstrap CDK (if needed)
echo "🏗️  Bootstrapping CDK..."
cdk bootstrap || echo "CDK already bootstrapped"

# Show diff
echo "📊 Reviewing changes..."
cdk diff

# Confirm deployment
read -p "Deploy to AWS? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Deployment cancelled"
    exit 1
fi

# Deploy
echo "🚀 Deploying to AWS..."
cdk deploy --require-approval never

# Get API URL
echo "✅ Deployment complete!"
echo ""
echo "📝 API Gateway URL:"
aws cloudformation describe-stacks \\
  --stack-name CobolModernizationStack \\
  --query 'Stacks[0].Outputs[?OutputKey==\`APIUrl\`].OutputValue' \\
  --output text

echo ""
echo "🎉 Deployment successful!"
echo "📊 View logs: aws logs tail /aws/lambda/${programName} --follow"
`
}

/**
 * Generate test script
 */
function generateTestScript (programName: string): string {
  return `#!/bin/bash
# Local testing script for ${programName}
# Generated by COBRA

set -e

echo "🧪 Starting local tests..."

# Check if SAM CLI is installed
if ! command -v sam &> /dev/null; then
    echo "⚠️  AWS SAM CLI not installed. Install from:"
    echo "   https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/install-sam-cli.html"
    echo ""
    echo "Running unit tests instead..."
    cd lambda
    npm test
    exit 0
fi

# Build Lambda function
echo "🔨 Building Lambda function..."
cd lambda
npm install
npm run build

# Create test event
echo "📝 Creating test event..."
cat > test-event.json << EOF
{
  "body": "{\\"test\\": \\"data\\"}",
  "requestContext": {
    "requestId": "test-request-id",
    "identity": {
      "cognitoIdentityId": "test-user"
    }
  }
}
EOF

# Run Lambda locally
echo "🚀 Invoking Lambda function locally..."
sam local invoke \\
  --event test-event.json \\
  --template-file ../template.yaml

# Cleanup
rm test-event.json

echo "✅ Local tests complete!"
`
}

/**
 * Generate cleanup script
 */
function generateCleanupScript (): string {
  return `#!/bin/bash
# Cleanup script - removes all deployed AWS resources
# Generated by COBRA

set -e

echo "🧹 Starting cleanup..."

# Confirm deletion
read -p "⚠️  This will delete all deployed resources. Continue? (y/n) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Cleanup cancelled"
    exit 1
fi

# Destroy CDK stack
echo "🗑️  Destroying CDK stack..."
cdk destroy --force

# Clean local build artifacts
echo "🧹 Cleaning local artifacts..."
rm -rf lambda/dist
rm -rf lambda/node_modules
rm -rf node_modules
rm -rf cdk.out

echo "✅ Cleanup complete!"
echo "💡 Note: CloudWatch logs are retained. Delete manually if needed."
`
}

/**
 * Ensure dependency files are present
 */
function ensureDependencyFiles (artifacts: CodeArtifact[]): CodeArtifact[] {
  const additional: CodeArtifact[] = []

  // Check if root package.json exists
  const hasRootPackageJson = artifacts.some(
    a => a.filename === 'package.json' && !a.filename.includes('lambda/')
  )

  if (!hasRootPackageJson) {
    additional.push({
      filename: 'package.json',
      content: JSON.stringify(
        {
          name: 'cobol-modernization',
          version: '1.0.0',
          description: 'COBOL modernization infrastructure',
          scripts: {
            build: 'tsc',
            test: 'jest',
            deploy: 'bash scripts/deploy.sh',
            cleanup: 'bash scripts/cleanup.sh'
          },
          dependencies: {
            'aws-cdk-lib': '^2.100.0',
            constructs: '^10.0.0'
          },
          devDependencies: {
            '@types/node': '^20.0.0',
            typescript: '^5.0.0',
            'aws-cdk': '^2.100.0'
          }
        },
        null,
        2
      ),
      language: 'json'
    })
  }

  // Check if .gitignore exists
  const hasGitignore = artifacts.some(a => a.filename === '.gitignore')

  if (!hasGitignore) {
    additional.push({
      filename: '.gitignore',
      content: `# Dependencies
node_modules/
lambda/node_modules/

# Build output
dist/
lambda/dist/
cdk.out/

# Environment
.env
.env.local

# IDE
.vscode/
.idea/
*.swp
*.swo

# OS
.DS_Store
Thumbs.db

# AWS
*.zip
function.zip

# Logs
*.log
npm-debug.log*
`,
      language: 'text'
    })
  }

  return additional
}

/**
 * Create archive manifest (placeholder for actual ZIP creation)
 */
function createArchiveManifest (artifacts: CodeArtifact[]): string {
  let manifest = '# Archive Manifest\n\n'
  manifest += `Generated: ${new Date().toISOString()}\n`
  manifest += `Total Files: ${artifacts.length}\n\n`

  manifest += '## Files:\n\n'

  for (const artifact of artifacts) {
    manifest += `### ${artifact.filename}\n`
    manifest += `Language: ${artifact.language}\n`
    manifest += `Size: ${artifact.content.length} bytes\n\n`
  }

  manifest += '\n## File Contents:\n\n'

  for (const artifact of artifacts) {
    manifest += `\n${'='.repeat(80)}\n`
    manifest += `FILE: ${artifact.filename}\n`
    manifest += `${'='.repeat(80)}\n\n`
    manifest += artifact.content
    manifest += '\n\n'
  }

  return manifest
}

/**
 * Generate download URL for packaged artifacts
 */
export function generateDownloadUrl (
  programName: string,
  artifactType: 'lambda' | 'cdk' | 'complete'
): string {
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
  const filename = `${programName.toLowerCase()}-${artifactType}-${timestamp}.zip`
  return `/api/download/${programName.toLowerCase()}/${filename}`
}

/**
 * Create individual artifact packages
 */
export function createArtifactPackages (
  artifacts: CodeArtifact[],
  analysis: LogicAnalysis
): Record<string, PackagedArtifact> {
  const programName = analysis.entryPoints[0]?.name || 'COBOL-MODERNIZATION'

  // Separate artifacts by type
  const lambdaArtifacts = artifacts.filter(
    a =>
      a.filename.includes('handler') ||
      a.filename.includes('types') ||
      a.filename.includes('validation') ||
      (a.filename.includes('package.json') && a.content.includes('handler'))
  )

  const cdkArtifacts = artifacts.filter(
    a =>
      a.filename.includes('lib/') ||
      a.filename.includes('bin/') ||
      a.filename.includes('cdk.json')
  )

  const apiGatewayArtifacts = artifacts.filter(
    a => a.filename.includes('openapi') || a.filename.includes('api-gateway')
  )

  return {
    lambda: packageArtifacts(lambdaArtifacts, analysis, {
      includeReadme: true,
      includeDeploymentScripts: false
    }),
    cdk: packageArtifacts(cdkArtifacts, analysis, {
      includeReadme: true,
      includeDeploymentScripts: true
    }),
    apiGateway: packageArtifacts(apiGatewayArtifacts, analysis, {
      includeReadme: true,
      includeDeploymentScripts: false
    }),
    complete: packageArtifacts(artifacts, analysis, {
      includeReadme: true,
      includeDeploymentScripts: true,
      includeDependencies: true
    })
  }
}
