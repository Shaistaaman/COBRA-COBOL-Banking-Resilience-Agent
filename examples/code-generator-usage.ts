/**
 * Example: Using the generateAWSCode MCP tool
 * Demonstrates how to generate AWS infrastructure code from COBOL analysis
 */

import { generateAWSCode } from '../src/mcp-server/tools/index.js'
import type { LogicAnalysis } from '../src/mcp-server/types.js'

async function main () {
  console.log('🚀 COBRA Code Generator Example\n')

  // Sample COBOL analysis (would normally come from parseCobol + analyzeLogic)
  const sampleAnalysis: LogicAnalysis = {
    businessRules: [
      {
        id: 'BR-001',
        type: 'calculation',
        description: 'Calculate simple interest on account balance',
        cobolSource: {
          file: 'INTEREST-CALC.cbl',
          startLine: 100,
          endLine: 120,
          snippet: 'COMPUTE INTEREST = PRINCIPAL * RATE * TIME / 100'
        },
        inputs: [
          {
            name: 'principal',
            level: 5,
            picture: '9(10)V99',
            usage: 'input'
          },
          {
            name: 'rate',
            level: 5,
            picture: '9(3)V99',
            usage: 'input'
          },
          {
            name: 'time',
            level: 5,
            picture: '9(3)',
            usage: 'input'
          }
        ],
        outputs: [
          {
            name: 'interest',
            level: 5,
            picture: '9(10)V99',
            usage: 'output'
          }
        ],
        formula: 'principal * rate * time / 100'
      },
      {
        id: 'BR-002',
        type: 'validation',
        description: 'Validate account balance is non-negative',
        cobolSource: {
          file: 'INTEREST-CALC.cbl',
          startLine: 85,
          endLine: 95,
          snippet: 'IF BALANCE < 0 THEN MOVE "ERROR" TO STATUS'
        },
        inputs: [
          {
            name: 'balance',
            level: 5,
            picture: '9(10)V99',
            usage: 'input'
          }
        ],
        outputs: [
          {
            name: 'status',
            level: 5,
            picture: 'X(10)',
            usage: 'output'
          }
        ],
        conditions: [
          {
            expression: 'balance < 0',
            action: 'throw new ValidationError("Balance cannot be negative")'
          }
        ]
      }
    ],
    dataStructures: [
      {
        name: 'ACCOUNT-RECORD',
        type: 'record',
        fields: [
          {
            name: 'accountNumber',
            level: 5,
            picture: '9(10)',
            usage: 'input'
          },
          {
            name: 'balance',
            level: 5,
            picture: '9(10)V99',
            usage: 'input'
          }
        ]
      }
    ],
    dependencies: [],
    entryPoints: [
      {
        name: 'INTEREST-CALC',
        parameters: [
          {
            name: 'principal',
            level: 1,
            picture: '9(10)V99'
          },
          {
            name: 'rate',
            level: 1,
            picture: '9(3)V99'
          },
          {
            name: 'time',
            level: 1,
            picture: '9(3)'
          }
        ],
        returnType: 'number'
      }
    ],
    patterns: [
      {
        type: 'interest_calculation',
        confidence: 0.95,
        location: {
          startLine: 100,
          endLine: 120,
          startColumn: 1,
          endColumn: 80
        },
        description: 'Simple interest calculation pattern detected',
        parameters: {
          principal: {
            name: 'principal',
            level: 5,
            picture: '9(10)V99'
          },
          rate: {
            name: 'rate',
            level: 5,
            picture: '9(3)V99'
          },
          term: {
            name: 'time',
            level: 5,
            picture: '9(3)'
          }
        }
      }
    ]
  }

  console.log('📊 Sample Analysis:')
  console.log(`  - Program: ${sampleAnalysis.entryPoints[0].name}`)
  console.log(`  - Business Rules: ${sampleAnalysis.businessRules.length}`)
  console.log(`  - Banking Patterns: ${sampleAnalysis.patterns.length}`)
  console.log(`  - Data Structures: ${sampleAnalysis.dataStructures.length}`)
  console.log()

  console.log('🔧 Generating AWS infrastructure code...\n')

  try {
    // Generate AWS code
    const artifacts = await generateAWSCode(sampleAnalysis)

    console.log('✅ Code generation complete!\n')

    // Display results
    console.log('📦 Generated Artifacts:')
    console.log(`  - Lambda Functions: ${artifacts.lambdaFunctions.length}`)
    console.log(`  - API Gateway Config: ${artifacts.apiGateway.length} bytes`)
    console.log(`  - CDK Stack: ${artifacts.cdkStack.length} bytes`)
    console.log(`  - README: ${artifacts.readme.length} bytes`)

    if (artifacts.packages) {
      console.log('\n📥 Download Packages:')
      for (const [type, pkg] of Object.entries(artifacts.packages)) {
        console.log(
          `  - ${type}: ${pkg.filename} (${pkg.files.length} files, ${(
            pkg.size / 1024
          ).toFixed(2)} KB)`
        )
        if (pkg.downloadUrl) {
          console.log(`    URL: ${pkg.downloadUrl}`)
        }
      }
    }

    // Display Lambda function details
    if (artifacts.lambdaFunctions.length > 0) {
      console.log('\n🔍 Lambda Function Details:')
      for (const fn of artifacts.lambdaFunctions) {
        console.log(`  - Name: ${fn.name}`)
        console.log(`    Runtime: ${fn.runtime}`)
        console.log(`    Handler: ${fn.handler}`)
        console.log(`    Code Size: ${fn.code.length} bytes`)
        console.log(
          `    Environment: ${Object.keys(fn.environment).join(', ')}`
        )
      }
    }

    // Display README preview
    console.log('\n📄 README Preview (first 500 chars):')
    console.log('─'.repeat(80))
    console.log(artifacts.readme.substring(0, 500) + '...')
    console.log('─'.repeat(80))

    console.log('\n✨ Success! AWS infrastructure code has been generated.')
    console.log('💡 Next steps:')
    console.log('   1. Review the generated code')
    console.log('   2. Download the complete package')
    console.log('   3. Deploy to AWS using the deployment scripts')
    console.log('   4. Test the API endpoints')
  } catch (error) {
    console.error('❌ Error generating code:', error)
    process.exit(1)
  }
}

// Run the example
main().catch(console.error)
