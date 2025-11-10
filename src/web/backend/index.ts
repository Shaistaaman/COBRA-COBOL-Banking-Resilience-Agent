import express, { Request, Response, NextFunction } from 'express'
import { v4 as uuidv4 } from 'uuid'
import { createOrchestrator } from '../../orchestrator.js'
import type { AnalysisProgress } from '../../orchestrator.js'
import {
  createError,
  ErrorCode,
  formatErrorForUser
} from '../../utils/error-handler.js'

const app = express()
const PORT = process.env.PORT || 3001

// Middleware
app.use(express.json({ limit: '10mb' }))
app.use(express.urlencoded({ extended: true }))

// CORS middleware
app.use((req: Request, res: Response, next: NextFunction) => {
  res.header('Access-Control-Allow-Origin', '*')
  res.header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
  res.header('Access-Control-Allow-Headers', 'Content-Type')
  if (req.method === 'OPTIONS') {
    return res.sendStatus(200)
  }
  next()
})

// Rate limiting
const requestCounts = new Map<string, { count: number; resetTime: number }>()
const RATE_LIMIT = 10
const RATE_WINDOW = 60000

function rateLimiter (req: Request, res: Response, next: NextFunction) {
  const ip = req.ip || 'unknown'
  const now = Date.now()
  const record = requestCounts.get(ip)

  if (!record || now > record.resetTime) {
    requestCounts.set(ip, { count: 1, resetTime: now + RATE_WINDOW })
    return next()
  }

  if (record.count >= RATE_LIMIT) {
    return res.status(429).json({
      error: 'Too many requests',
      message: 'Rate limit exceeded. Please try again later.'
    })
  }

  record.count++
  next()
}

app.use(rateLimiter)

// In-memory storage
interface AnalysisResult {
  id: string
  status: 'processing' | 'complete' | 'error'
  cobolSource?: string
  explanation?: string
  architecture?: string
  artifacts?: {
    lambda?: string
    apiGateway?: string
    cdk?: string
  }
  error?: string
  errorDetails?: {
    code?: string
    suggestions?: string[]
  }
  progress?: {
    stage: string
    percentage: number
    message: string
  }
  createdAt: Date
}

const analysisResults = new Map<string, AnalysisResult>()

// Input validation
function validateCobolInput (cobolSource: string): {
  valid: boolean
  error?: string
} {
  if (!cobolSource || typeof cobolSource !== 'string') {
    return { valid: false, error: 'COBOL source code is required' }
  }

  if (cobolSource.length === 0) {
    return { valid: false, error: 'COBOL source code cannot be empty' }
  }

  if (cobolSource.length > 5000000) {
    return {
      valid: false,
      error: 'COBOL source code exceeds maximum size (5MB)'
    }
  }

  const hasIdentificationDivision = /IDENTIFICATION\s+DIVISION/i.test(
    cobolSource
  )
  const hasProgramId = /PROGRAM-ID/i.test(cobolSource)

  if (!hasIdentificationDivision && !hasProgramId) {
    return {
      valid: false,
      error:
        'Invalid COBOL format: missing IDENTIFICATION DIVISION or PROGRAM-ID'
    }
  }

  return { valid: true }
}

// POST /api/analyze
app.post('/api/analyze', async (req: Request, res: Response) => {
  try {
    const { cobolSource } = req.body

    const validation = validateCobolInput(cobolSource)
    if (!validation.valid) {
      return res.status(400).json({ error: validation.error })
    }

    const analysisId = uuidv4()

    analysisResults.set(analysisId, {
      id: analysisId,
      status: 'processing',
      cobolSource,
      createdAt: new Date()
    })

    processAnalysis(analysisId, cobolSource).catch(error => {
      console.error(`Analysis ${analysisId} failed:`, error)
      const result = analysisResults.get(analysisId)
      if (result) {
        result.status = 'error'
        result.error = error.message || 'Analysis failed'
      }
    })

    res.json({ analysisId })
  } catch (error) {
    console.error('Error starting analysis:', error)
    res.status(500).json({ error: 'Failed to start analysis' })
  }
})

// Process analysis using orchestrator
async function processAnalysis (analysisId: string, cobolSource: string) {
  const result = analysisResults.get(analysisId)
  if (!result) return

  try {
    // Create orchestrator with LLM configuration
    const orchestrator = createOrchestrator({
      llmApiKey: process.env.OPENAI_API_KEY || process.env.ANTHROPIC_API_KEY,
      llmProvider: process.env.OPENAI_API_KEY ? 'openai' : 'anthropic',
      enableCaching: true,
      verbose: true
    })

    // Progress callback to track analysis stages and update result
    const onProgress = (progress: AnalysisProgress) => {
      console.log(
        `[${analysisId}] ${progress.stage}: ${progress.progress}% - ${progress.message}`
      )

      // Update progress in result
      if (result) {
        result.progress = {
          stage: progress.stage,
          percentage: progress.progress,
          message: progress.message
        }
      }
    }

    // Run complete analysis
    const analysisResult = await orchestrator.analyzeComplete(
      cobolSource,
      onProgress
    )

    // Generate architecture diagram
    const architecture = generateArchitectureDiagram(
      analysisResult.analysis,
      analysisResult.parseResult.metadata.programName
    )

    // Extract generated code from artifacts
    const lambdaCode =
      analysisResult.awsCode?.lambdaFunctions.map(f => f.code).join('\n\n') ||
      '// No Lambda code generated'

    const apiGatewayConfig =
      typeof analysisResult.awsCode?.apiGateway === 'string'
        ? analysisResult.awsCode.apiGateway
        : '# No API Gateway config generated'

    const cdkCode =
      typeof analysisResult.awsCode?.cdkStack === 'string'
        ? analysisResult.awsCode.cdkStack
        : '// No CDK code generated'

    // Update result
    result.status = 'complete'
    result.explanation =
      analysisResult.explanation || 'Analysis complete (no LLM explanation)'
    result.architecture = architecture
    result.artifacts = {
      lambda: lambdaCode,
      apiGateway: apiGatewayConfig,
      cdk: cdkCode
    }
  } catch (error: any) {
    console.error(`Analysis ${analysisId} failed:`, error)
    result.status = 'error'

    // Extract error details if it's a COBRA error
    const errorMessage = error.message || 'Analysis processing failed'

    // Check if it's a formatted COBRA error
    if (errorMessage.includes('❌')) {
      result.error = errorMessage
      // Try to extract suggestions
      const suggestionsMatch = errorMessage.match(/Suggestions:\n([\s\S]+)/)
      if (suggestionsMatch) {
        const suggestions = suggestionsMatch[1]
          .split('\n')
          .filter((s: string) => s.trim())
          .map((s: string) => s.replace(/^\s*\d+\.\s*/, ''))
        result.errorDetails = { suggestions }
      }
    } else {
      // Generic error
      const cobraError = createError(ErrorCode.SYSTEM_UNKNOWN, errorMessage, {
        analysisId
      })
      result.error = formatErrorForUser(cobraError)
      result.errorDetails = {
        code: cobraError.code,
        suggestions: cobraError.suggestions
      }
    }
  }
}

// Generate architecture diagram
function generateArchitectureDiagram (
  analysis: any,
  programName: string = 'COBOL-Program'
): string {
  // Determine if DynamoDB is needed based on patterns
  const needsDatabase = analysis.patterns?.some(
    (p: any) =>
      p.type === 'transaction_posting' || p.type === 'account_reconciliation'
  )

  // Determine if Step Functions is needed for batch processing
  const needsStepFunctions = analysis.patterns?.some(
    (p: any) => p.type === 'batch_processing'
  )

  let diagram = `graph TB
    subgraph "Legacy System"
        COBOL[${programName}]
    end
    
    subgraph "AWS Cloud"
        APIGW[API Gateway]
        LAMBDA[Lambda Function]
        CW[CloudWatch Logs]`

  if (needsDatabase) {
    diagram += `
        DDB[DynamoDB]`
  }

  if (needsStepFunctions) {
    diagram += `
        SF[Step Functions]`
  }

  diagram += `
    end
    
    CLIENT[Client Application] --> APIGW
    APIGW --> LAMBDA`

  if (needsDatabase) {
    diagram += `
    LAMBDA --> DDB`
  }

  if (needsStepFunctions) {
    diagram += `
    LAMBDA --> SF`
  }

  diagram += `
    LAMBDA --> CW
    
    style COBOL fill:#f9f,stroke:#333,stroke-width:2px
    style LAMBDA fill:#ff9,stroke:#333,stroke-width:2px
    style APIGW fill:#9f9,stroke:#333,stroke-width:2px`

  return diagram
}

// GET /api/analysis/:id
app.get('/api/analysis/:id', (req: Request, res: Response) => {
  const { id } = req.params
  const result = analysisResults.get(id)

  if (!result) {
    return res.status(404).json({
      error: 'Analysis not found',
      message:
        'The requested analysis ID does not exist or has expired. Please start a new analysis.'
    })
  }

  const response: any = {
    id: result.id,
    status: result.status,
    createdAt: result.createdAt
  }

  // Include progress information for processing status
  if (result.status === 'processing' && result.progress) {
    response.progress = result.progress
  }

  if (result.status === 'complete') {
    response.explanation = result.explanation
    response.architecture = result.architecture
    response.artifacts = {
      lambda: result.artifacts?.lambda ? true : false,
      apiGateway: result.artifacts?.apiGateway ? true : false,
      cdk: result.artifacts?.cdk ? true : false
    }
  } else if (result.status === 'error') {
    response.error = result.error
    if (result.errorDetails) {
      response.errorDetails = result.errorDetails
    }
  }

  res.json(response)
})

// GET /api/download/:id/:artifact
app.get('/api/download/:id/:artifact', (req: Request, res: Response) => {
  const { id, artifact } = req.params
  const result = analysisResults.get(id)

  if (!result) {
    return res.status(404).json({ error: 'Analysis not found' })
  }

  if (result.status !== 'complete') {
    return res.status(400).json({ error: 'Analysis not complete' })
  }

  let content: string | undefined
  let filename: string

  switch (artifact) {
    case 'lambda':
      content = result.artifacts?.lambda
      filename = 'lambda-handler.ts'
      break
    case 'apiGateway':
      content = result.artifacts?.apiGateway
      filename = 'api-gateway-config.yaml'
      break
    case 'cdk':
      content = result.artifacts?.cdk
      filename = 'cdk-stack.ts'
      break
    case 'all':
      content = `# Generated AWS Artifacts

## Lambda Function
\`\`\`typescript
${result.artifacts?.lambda || '// No Lambda code generated'}
\`\`\`

## API Gateway Configuration
\`\`\`yaml
${result.artifacts?.apiGateway || '# No API Gateway config generated'}
\`\`\`

## CDK Stack
\`\`\`typescript
${result.artifacts?.cdk || '// No CDK code generated'}
\`\`\`
`
      filename = 'aws-artifacts.md'
      break
    default:
      return res.status(400).json({ error: 'Invalid artifact type' })
  }

  if (!content) {
    return res.status(404).json({ error: 'Artifact not found' })
  }

  res.setHeader('Content-Type', 'text/plain')
  res.setHeader('Content-Disposition', `attachment; filename="${filename}"`)
  res.send(content)
})

// Health check
app.get('/health', (req: Request, res: Response) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() })
})

// Error handling
app.use((err: Error, req: Request, res: Response, next: NextFunction) => {
  console.error('Unhandled error:', err)
  res.status(500).json({ error: 'Internal server error' })
})

// Start server
async function startServer () {
  // Initialize performance optimizations
  const { initializePerformanceOptimizations } = await import(
    '../../utils/startup-optimizer.js'
  )
  await initializePerformanceOptimizations({
    preloadExamples: true,
    enableCache: true,
    verbose: true
  })

  app.listen(PORT, () => {
    console.log(`COBRA API server running on http://localhost:${PORT}`)
    console.log(`Health check: http://localhost:${PORT}/health`)
  })
}

startServer().catch(error => {
  console.error('Failed to start server:', error)
  process.exit(1)
})

export default app
