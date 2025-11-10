#!/usr/bin/env node

/**
 * COBRA MCP Server
 * Exposes COBOL analysis and modernization capabilities through Model Context Protocol
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js'
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  Tool
} from '@modelcontextprotocol/sdk/types.js'
import {
  parseCobol,
  analyzeLogic,
  generateSpec,
  generateAWSCode,
  suggestModernization
} from './tools/index.js'

/**
 * Define available MCP tools
 */
const tools: Tool[] = [
  {
    name: 'parseCobol',
    description:
      'Parse COBOL source code and generate Abstract Syntax Tree (AST). Returns structured analysis including program metadata, errors, and warnings.',
    inputSchema: {
      type: 'object',
      properties: {
        source: {
          type: 'string',
          description: 'COBOL source code to parse'
        }
      },
      required: ['source']
    }
  },
  {
    name: 'analyzeLogic',
    description:
      'Analyze COBOL logic to extract business rules, banking patterns, data structures, and dependencies. Identifies interest calculations, transaction validations, and batch processing workflows.',
    inputSchema: {
      type: 'object',
      properties: {
        ast: {
          type: 'object',
          description: 'Abstract Syntax Tree from parseCobol'
        }
      },
      required: ['ast']
    }
  },
  {
    name: 'generateSpec',
    description:
      'Generate Kiro spec documents (requirements.md, design.md, tasks.md) from COBOL analysis. Creates EARS-compliant requirements and AWS architecture recommendations.',
    inputSchema: {
      type: 'object',
      properties: {
        analysis: {
          type: 'object',
          description: 'Logic analysis result from analyzeLogic'
        }
      },
      required: ['analysis']
    }
  },
  {
    name: 'generateAWSCode',
    description:
      'Generate AWS Lambda functions, API Gateway configurations, and CDK infrastructure code from spec or analysis. Produces production-ready TypeScript/Python code.',
    inputSchema: {
      type: 'object',
      properties: {
        input: {
          type: 'object',
          description: 'Spec document or logic analysis'
        }
      },
      required: ['input']
    }
  },
  {
    name: 'suggestModernization',
    description:
      'Suggest modernization strategies and prioritize COBOL modules for migration. Provides coupling analysis, AWS service recommendations, and de-risking approaches.',
    inputSchema: {
      type: 'object',
      properties: {
        analysis: {
          type: 'object',
          description: 'Logic analysis result from analyzeLogic'
        }
      },
      required: ['analysis']
    }
  }
]

/**
 * Initialize MCP server
 */
const server = new Server(
  {
    name: 'cobra-cobol-agent',
    version: '0.1.0'
  },
  {
    capabilities: {
      tools: {}
    }
  }
)

/**
 * Handle tool list requests
 */
server.setRequestHandler(ListToolsRequestSchema, async () => {
  return { tools }
})

/**
 * Handle tool execution requests
 */
server.setRequestHandler(CallToolRequestSchema, async request => {
  const { name, arguments: args } = request.params

  try {
    switch (name) {
      case 'parseCobol': {
        const { source } = args as { source: string }
        const result = await parseCobol(source)
        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(result, null, 2)
            }
          ]
        }
      }

      case 'analyzeLogic': {
        const { ast } = args as { ast: any }
        const result = await analyzeLogic(ast)
        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(result, null, 2)
            }
          ]
        }
      }

      case 'generateSpec': {
        const { analysis } = args as { analysis: any }
        const result = await generateSpec(analysis)
        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(result, null, 2)
            }
          ]
        }
      }

      case 'generateAWSCode': {
        const { input } = args as { input: any }
        const result = await generateAWSCode(input)
        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(result, null, 2)
            }
          ]
        }
      }

      case 'suggestModernization': {
        const { analysis } = args as { analysis: any }
        const result = await suggestModernization(analysis)
        return {
          content: [
            {
              type: 'text',
              text: JSON.stringify(result, null, 2)
            }
          ]
        }
      }

      default:
        throw new Error(`Unknown tool: ${name}`)
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    return {
      content: [
        {
          type: 'text',
          text: JSON.stringify({ error: errorMessage }, null, 2)
        }
      ],
      isError: true
    }
  }
})

/**
 * Start the server
 */
async function main () {
  // Initialize performance optimizations
  const { initializePerformanceOptimizations } = await import(
    '../utils/startup-optimizer.js'
  )
  await initializePerformanceOptimizations({
    preloadExamples: true,
    enableCache: true,
    verbose: true
  })

  const transport = new StdioServerTransport()
  await server.connect(transport)
  console.error('COBRA MCP Server running on stdio')
}

main().catch(error => {
  console.error('Server error:', error)
  process.exit(1)
})
