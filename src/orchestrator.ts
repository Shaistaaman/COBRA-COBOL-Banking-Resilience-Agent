/**
 * COBRA Orchestrator
 * Coordinates all components for end-to-end COBOL analysis and modernization
 */

import type {
  ParseResult,
  LogicAnalysis,
  SpecDocument,
  AWSArtifacts,
  ModernizationPlan
} from './mcp-server/types.js'
import { createParser } from './parser/cobol-parser.js'
import { createLogicAnalyzer } from './analyzer/index.js'
import { ExplanationGenerator } from './llm/explanation-generator.js'
import { generateLambdaFunction } from './generator/lambda-generator.js'
import { generateAPIGatewayConfig } from './generator/api-gateway-generator.js'
import { generateCDKStack } from './generator/cdk-generator.js'
import {
  parseCobol,
  analyzeLogic,
  generateSpec,
  generateAWSCode,
  suggestModernization
} from './mcp-server/tools/index.js'
import {
  createError,
  ErrorCode,
  formatErrorForUser,
  type CobraError
} from './utils/error-handler.js'
import {
  ProgressTracker,
  createConsoleProgressReporter
} from './utils/progress-tracker.js'
import { getGlobalCache, getGlobalPreloader } from './utils/index.js'
import { getGlobalCostTracker } from './llm/cost-tracker.js'

export interface OrchestratorOptions {
  llmApiKey?: string
  llmProvider?: 'openai' | 'anthropic'
  enableCaching?: boolean
  verbose?: boolean
}

export interface AnalysisProgress {
  stage:
    | 'parsing'
    | 'analyzing'
    | 'explaining'
    | 'generating-spec'
    | 'generating-code'
    | 'complete'
  progress: number
  message: string
}

export type ProgressCallback = (progress: AnalysisProgress) => void

/**
 * Main orchestrator class that coordinates all COBRA components
 */
export class CobraOrchestrator {
  private options: OrchestratorOptions
  private parser: any
  private analyzer: any
  private explanationGenerator: ExplanationGenerator | null = null

  constructor (options: OrchestratorOptions = {}) {
    this.options = {
      enableCaching: true,
      verbose: false,
      ...options
    }

    // Initialize parser
    this.parser = createParser({
      dialect: 'COBOL-85',
      preserveComments: true,
      strictMode: false
    })

    // Initialize analyzer
    this.analyzer = createLogicAnalyzer({
      patternRecognition: {
        enableAllPatterns: true,
        confidenceThreshold: 0.6
      }
    })

    // Initialize LLM explanation generator if API key provided
    if (this.options.llmApiKey) {
      this.explanationGenerator = new ExplanationGenerator({
        llmConfig: {
          provider: this.options.llmProvider || 'openai',
          apiKey: this.options.llmApiKey
        },
        cacheEnabled: this.options.enableCaching
      })
    }
  }

  /**
   * Complete end-to-end analysis workflow
   * Parses COBOL, analyzes logic, generates explanation, spec, and AWS code
   */
  async analyzeComplete (
    cobolSource: string,
    onProgress?: ProgressCallback
  ): Promise<{
    parseResult: ParseResult
    analysis: LogicAnalysis
    explanation?: string
    spec?: SpecDocument
    awsCode?: AWSArtifacts
    modernizationPlan?: ModernizationPlan
  }> {
    // Create progress tracker
    const tracker = new ProgressTracker(
      {
        parsing: 20,
        analyzing: 20,
        explaining: 20,
        'generating-spec': 20,
        'generating-code': 20
      },
      update => {
        if (onProgress) {
          onProgress({
            stage: update.stage as any,
            progress: update.progress,
            message: update.message
          })
        }
      }
    )

    try {
      // Validate input first
      const validation = this.validateCobolSource(cobolSource)
      if (!validation.valid) {
        const error = createError(
          ErrorCode.PARSE_EMPTY_SOURCE,
          validation.errors.join('; ')
        )
        throw new Error(formatErrorForUser(error))
      }

      // Stage 1: Parse COBOL
      tracker.startStage('parsing', 'Parsing COBOL source code...')
      const parseResult = await this.parse(cobolSource)
      tracker.updateProgress('parsing', 50, 'Validating parsed structure...')

      if (!parseResult.ast) {
        const error = createError(
          ErrorCode.PARSE_INVALID_SYNTAX,
          `Parse failed: ${parseResult.errors.map(e => e.message).join(', ')}`
        )
        throw new Error(formatErrorForUser(error))
      }

      tracker.completeStage('parsing', 'COBOL parsing complete ✓')

      // Stage 2: Analyze logic
      tracker.startStage('analyzing', 'Analyzing business logic patterns...')
      const analysis = await this.analyze(parseResult.ast)
      tracker.updateProgress(
        'analyzing',
        50,
        `Found ${analysis.patterns.length} banking patterns`
      )

      if (analysis.patterns.length === 0) {
        const error = createError(
          ErrorCode.ANALYZE_NO_PATTERNS,
          'No recognizable banking patterns found'
        )
        this.log(`⚠️  ${error.userMessage}`)
      }

      tracker.completeStage('analyzing', 'Logic analysis complete ✓')

      // Stage 3: Generate explanation (if LLM available)
      let explanation: string | undefined
      if (this.explanationGenerator) {
        tracker.startStage(
          'explaining',
          'Generating natural-language explanation...'
        )
        try {
          const explanationResult =
            await this.explanationGenerator.generateExplanation(
              parseResult.ast,
              analysis
            )
          explanation = explanationResult.summary
          tracker.completeStage('explaining', 'Explanation generated ✓')
        } catch (error) {
          const err = error instanceof Error ? error.message : String(error)
          this.log(`⚠️  LLM explanation failed: ${err}`)
          tracker.completeStage(
            'explaining',
            'Explanation skipped (LLM unavailable)'
          )
        }
      } else {
        tracker.completeStage('explaining', 'Explanation skipped (no LLM key)')
      }

      // Stage 4: Generate spec documents
      tracker.startStage(
        'generating-spec',
        'Generating specification documents...'
      )
      const spec = await this.generateSpecDocuments(analysis)
      tracker.completeStage('generating-spec', 'Spec documents generated ✓')

      // Stage 5: Generate AWS code
      tracker.startStage(
        'generating-code',
        'Generating AWS infrastructure code...'
      )
      const awsCode = await this.generateAWSInfrastructure(analysis)
      tracker.updateProgress(
        'generating-code',
        50,
        `Generated ${awsCode.lambdaFunctions.length} Lambda functions`
      )

      // Stage 6: Generate modernization plan
      const modernizationPlan = await this.suggestModernizationStrategy(
        analysis
      )

      tracker.completeStage(
        'generating-code',
        `Analysis complete in ${tracker.getFormattedElapsedTime()} ✓`
      )

      return {
        parseResult,
        analysis,
        explanation,
        spec,
        awsCode,
        modernizationPlan
      }
    } catch (error) {
      const errorMessage =
        error instanceof Error ? error.message : String(error)
      this.log(`❌ Error in complete analysis: ${errorMessage}`)

      // If it's already a formatted COBRA error, re-throw
      if (errorMessage.includes('❌')) {
        throw error
      }

      // Otherwise wrap it
      const cobraError = createError(ErrorCode.SYSTEM_UNKNOWN, errorMessage, {
        stage: 'complete-analysis'
      })
      throw new Error(formatErrorForUser(cobraError))
    }
  }

  /**
   * Parse COBOL source code
   */
  async parse (cobolSource: string): Promise<ParseResult> {
    this.log('Parsing COBOL source...')
    return await parseCobol(cobolSource)
  }

  /**
   * Analyze COBOL logic
   */
  async analyze (ast: any): Promise<LogicAnalysis> {
    this.log('Analyzing COBOL logic...')
    return await analyzeLogic(ast)
  }

  /**
   * Generate specification documents
   */
  async generateSpecDocuments (analysis: LogicAnalysis): Promise<SpecDocument> {
    this.log('Generating specification documents...')
    return await generateSpec(analysis)
  }

  /**
   * Generate AWS infrastructure code
   */
  async generateAWSInfrastructure (
    analysis: LogicAnalysis
  ): Promise<AWSArtifacts> {
    this.log('Generating AWS infrastructure code...')
    return await generateAWSCode(analysis)
  }

  /**
   * Suggest modernization strategy
   */
  async suggestModernizationStrategy (
    analysis: LogicAnalysis
  ): Promise<ModernizationPlan> {
    this.log('Generating modernization recommendations...')
    return await suggestModernization(analysis)
  }

  /**
   * Quick analysis (parse + analyze only, no LLM)
   */
  async analyzeQuick (
    cobolSource: string,
    onProgress?: ProgressCallback
  ): Promise<{
    parseResult: ParseResult
    analysis: LogicAnalysis
  }> {
    this.reportProgress(onProgress, 'parsing', 30, 'Parsing COBOL source...')
    const parseResult = await this.parse(cobolSource)

    if (!parseResult.ast) {
      throw new Error(
        `Parse failed: ${parseResult.errors.map(e => e.message).join(', ')}`
      )
    }

    this.reportProgress(
      onProgress,
      'analyzing',
      70,
      'Analyzing business logic...'
    )
    const analysis = await this.analyze(parseResult.ast)

    this.reportProgress(onProgress, 'complete', 100, 'Quick analysis complete!')

    return { parseResult, analysis }
  }

  /**
   * Generate only AWS code (skip spec generation)
   */
  async generateCodeOnly (
    analysis: LogicAnalysis,
    onProgress?: ProgressCallback
  ): Promise<AWSArtifacts> {
    this.reportProgress(
      onProgress,
      'generating-code',
      50,
      'Generating AWS code...'
    )
    const awsCode = await this.generateAWSInfrastructure(analysis)

    this.reportProgress(
      onProgress,
      'complete',
      100,
      'Code generation complete!'
    )

    return awsCode
  }

  /**
   * Validate COBOL source before processing
   */
  validateCobolSource (cobolSource: string): {
    valid: boolean
    errors: string[]
  } {
    const errors: string[] = []

    if (!cobolSource || typeof cobolSource !== 'string') {
      errors.push('COBOL source must be a non-empty string')
    }

    if (cobolSource.length === 0) {
      errors.push('COBOL source cannot be empty')
    }

    if (cobolSource.length > 10000000) {
      errors.push('COBOL source exceeds maximum size (10MB)')
    }

    const hasIdentificationDivision = /IDENTIFICATION\s+DIVISION/i.test(
      cobolSource
    )
    const hasProgramId = /PROGRAM-ID/i.test(cobolSource)

    if (!hasIdentificationDivision && !hasProgramId) {
      errors.push(
        'Invalid COBOL format: missing IDENTIFICATION DIVISION or PROGRAM-ID'
      )
    }

    return {
      valid: errors.length === 0,
      errors
    }
  }

  /**
   * Report progress to callback
   */
  private reportProgress (
    callback: ProgressCallback | undefined,
    stage: AnalysisProgress['stage'],
    progress: number,
    message: string
  ) {
    this.log(`[${progress}%] ${message}`)
    if (callback) {
      callback({ stage, progress, message })
    }
  }

  /**
   * Log message if verbose mode enabled
   */
  private log (message: string) {
    if (this.options.verbose) {
      console.log(`[COBRA] ${message}`)
    }
  }

  /**
   * Get performance statistics
   */
  getPerformanceStats (): {
    cache: string
    cost: string
  } {
    const cache = getGlobalCache()
    const costTracker = getGlobalCostTracker()

    return {
      cache: cache.getFormattedStats(),
      cost: costTracker.getFormattedReport()
    }
  }

  /**
   * Preload example files for instant demo
   */
  async preloadExamples (): Promise<void> {
    const preloader = getGlobalPreloader()
    await preloader.preloadExamples()
  }

  /**
   * Get preloaded example
   */
  getExample (name: string) {
    const preloader = getGlobalPreloader()
    return preloader.getExample(name)
  }

  /**
   * List available examples
   */
  listExamples (): string[] {
    const preloader = getGlobalPreloader()
    return preloader.getExampleNames()
  }
}

/**
 * Factory function to create orchestrator
 */
export function createOrchestrator (
  options?: OrchestratorOptions
): CobraOrchestrator {
  return new CobraOrchestrator(options)
}

/**
 * Convenience function for quick analysis
 */
export async function analyzeCobol (
  cobolSource: string,
  options?: OrchestratorOptions
): Promise<{
  parseResult: ParseResult
  analysis: LogicAnalysis
  explanation?: string
  spec?: SpecDocument
  awsCode?: AWSArtifacts
  modernizationPlan?: ModernizationPlan
}> {
  const orchestrator = createOrchestrator(options)
  return await orchestrator.analyzeComplete(cobolSource)
}
