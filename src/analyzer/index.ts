/**
 * COBOL Logic Analyzer Module
 * Provides comprehensive analysis of COBOL business logic
 */

import type { AST, LogicAnalysis } from '../mcp-server/types.js'
import {
  createPatternRecognizer,
  type PatternRecognizerOptions
} from './pattern-recognizer.js'
import {
  createBusinessRuleExtractor,
  type BusinessRuleExtractorOptions
} from './business-rule-extractor.js'
import {
  createDataFlowAnalyzer,
  type DataFlowAnalyzerOptions
} from './data-flow-analyzer.js'

export * from './types.js'
export * from './pattern-recognizer.js'
export * from './business-rule-extractor.js'
export * from './data-flow-analyzer.js'

export interface LogicAnalyzerOptions {
  patternRecognition?: PatternRecognizerOptions
  businessRules?: BusinessRuleExtractorOptions
  dataFlow?: DataFlowAnalyzerOptions
}

/**
 * Main Logic Analyzer class that coordinates all analysis components
 */
export class LogicAnalyzer {
  private patternRecognizer
  private businessRuleExtractor
  private dataFlowAnalyzer

  constructor (options: LogicAnalyzerOptions = {}) {
    this.patternRecognizer = createPatternRecognizer(options.patternRecognition)
    this.businessRuleExtractor = createBusinessRuleExtractor(
      options.businessRules
    )
    this.dataFlowAnalyzer = createDataFlowAnalyzer(options.dataFlow)
  }

  /**
   * Perform complete logic analysis on COBOL AST
   */
  analyze (ast: AST, fileName: string = 'UNKNOWN'): LogicAnalysis {
    // Check cache first
    const { getGlobalCache } = require('../utils/performance-cache.js')
    const cache = getGlobalCache()
    const cachedAnalysis = cache.getAnalysis(ast.programId)
    if (cachedAnalysis) {
      return cachedAnalysis
    }

    // Extract banking patterns
    const patterns = this.patternRecognizer.identifyPatterns(ast)

    // Extract business rules
    const businessRules = this.businessRuleExtractor.extractBusinessRules(
      ast,
      fileName
    )

    // Build data flow graph
    const dataFlowGraph = this.dataFlowAnalyzer.buildDataFlowGraph(ast)

    // Extract data structures from working storage
    const dataStructures = this.extractDataStructures(ast)

    // Identify dependencies
    const dependencies = this.identifyDependencies(ast)

    // Identify entry points
    const entryPoints = this.identifyEntryPoints(ast)

    const analysis = {
      businessRules,
      dataStructures,
      dependencies,
      entryPoints,
      patterns
    }

    // Cache the analysis
    cache.cacheAnalysis(ast.programId, analysis)

    return analysis
  }

  /**
   * Extract data structures from AST
   */
  private extractDataStructures (ast: AST) {
    const structures = []
    const workingStorage = ast.divisions.data?.workingStorage || []

    // Group by top-level (01) elements
    const topLevel = workingStorage.filter(e => e.level === 1)

    for (const top of topLevel) {
      const fields = workingStorage.filter(e => e.level > 1 && e.level < 50)

      structures.push({
        name: top.name,
        type: 'WORKING-STORAGE',
        fields
      })
    }

    return structures
  }

  /**
   * Identify program dependencies
   */
  private identifyDependencies (ast: AST) {
    const dependencies = []

    // Check for CALL statements (subprogram dependencies)
    const statements = ast.divisions.procedure?.statements || []
    for (const stmt of statements) {
      if (stmt.type === 'CALL' && stmt.raw) {
        const callMatch = stmt.raw.match(/CALL\s+['"]([^'"]+)['"]/i)
        if (callMatch) {
          dependencies.push({
            type: 'subprogram' as const,
            name: callMatch[1]
          })
        }
      }
    }

    // Check for file dependencies
    const files = ast.divisions.data?.fileSection || []
    for (const file of files) {
      dependencies.push({
        type: 'file' as const,
        name: file.name
      })
    }

    return dependencies
  }

  /**
   * Identify program entry points
   */
  private identifyEntryPoints (ast: AST) {
    const entryPoints = []

    // Main program entry point
    entryPoints.push({
      name: ast.programId,
      parameters: [],
      returnType: undefined
    })

    return entryPoints
  }
}

/**
 * Factory function to create logic analyzer
 */
export function createLogicAnalyzer (
  options?: LogicAnalyzerOptions
): LogicAnalyzer {
  return new LogicAnalyzer(options)
}
