/**
 * COBOL Explanation Generator
 * Converts AST and analysis to natural-language explanations using LLM
 */

import type { AST, LogicAnalysis } from '../mcp-server/types.js'
import type {
  LLMConfig,
  ExplanationRequest,
  ExplanationResponse
} from './types.js'
import { createLLMClient, type LLMClient } from './client.js'
import { createCache, type LLMCache } from './cache.js'
import {
  getSystemPrompt,
  getExplanationPrompt,
  getQuickSummaryPrompt
} from './prompts.js'

export interface ExplanationGeneratorOptions {
  llmConfig: LLMConfig
  cacheEnabled?: boolean
  cacheTTL?: number
}

/**
 * Main explanation generator class
 */
export class ExplanationGenerator {
  private client: LLMClient
  private cache: LLMCache

  constructor (options: ExplanationGeneratorOptions) {
    this.client = createLLMClient(options.llmConfig)
    this.cache = createCache({
      enabled: options.cacheEnabled ?? true,
      ttlSeconds: options.cacheTTL ?? 3600
    })
  }

  /**
   * Generate comprehensive explanation from AST and analysis
   */
  async generateExplanation (
    ast: AST,
    analysis: LogicAnalysis,
    fileName?: string
  ): Promise<ExplanationResponse> {
    // Check cache first (both local and global)
    const cached = this.cache.get(ast, analysis)
    if (cached) {
      return { ...cached, cached: true } as any
    }

    // Also check global performance cache
    const { getGlobalCache } = await import('../utils/performance-cache.js')
    const globalCache = getGlobalCache()
    const prompt = getExplanationPrompt(ast, analysis, fileName)
    const cachedResponse = globalCache.getLLMResponse(prompt)
    if (cachedResponse) {
      return { ...cachedResponse, cached: true } as any
    }

    // Generate prompts
    const systemPrompt = getSystemPrompt()
    const userPrompt = prompt

    try {
      // Call LLM
      const response = await this.client.chat(systemPrompt, userPrompt)

      // Parse JSON response
      const explanation = this.parseExplanation(response.content)

      // Cache the result in both caches
      this.cache.set(ast, analysis, explanation)
      globalCache.cacheLLMResponse(userPrompt, explanation)

      return explanation
    } catch (error) {
      // Fallback to basic explanation if LLM fails
      console.error('LLM explanation generation failed:', error)
      return this.generateFallbackExplanation(ast, analysis)
    }
  }

  /**
   * Generate quick summary (faster, cheaper)
   */
  async generateQuickSummary (ast: AST, fileName?: string): Promise<string> {
    const systemPrompt = getSystemPrompt()
    const userPrompt = getQuickSummaryPrompt(ast, fileName)

    try {
      const response = await this.client.chat(systemPrompt, userPrompt)
      return response.content.trim()
    } catch (error) {
      console.error('Quick summary generation failed:', error)
      return `COBOL program ${ast.programId} - Analysis unavailable`
    }
  }

  /**
   * Parse LLM response into structured format
   */
  private parseExplanation (content: string): ExplanationResponse {
    try {
      // Try to parse as JSON first
      const parsed = JSON.parse(content)

      return {
        summary: parsed.summary || '',
        businessLogic: parsed.businessLogic || '',
        inputs: Array.isArray(parsed.inputs) ? parsed.inputs : [],
        outputs: Array.isArray(parsed.outputs) ? parsed.outputs : [],
        rules: Array.isArray(parsed.rules) ? parsed.rules : [],
        complexity: parsed.complexity || 'Unknown',
        recommendations: Array.isArray(parsed.recommendations)
          ? parsed.recommendations
          : []
      }
    } catch (error) {
      // If JSON parsing fails, try to extract sections from text
      return this.parseTextExplanation(content)
    }
  }

  /**
   * Parse text-based explanation (fallback)
   */
  private parseTextExplanation (content: string): ExplanationResponse {
    const sections = {
      summary: '',
      businessLogic: '',
      inputs: [] as string[],
      outputs: [] as string[],
      rules: [] as string[],
      complexity: 'Unknown',
      recommendations: [] as string[]
    }

    // Extract sections using regex patterns
    const summaryMatch = content.match(/summary[:\s]+(.+?)(?=\n\n|\n[A-Z]|$)/is)
    if (summaryMatch) {
      sections.summary = summaryMatch[1].trim()
    }

    const businessLogicMatch = content.match(
      /business\s+logic[:\s]+(.+?)(?=\n\n|\n[A-Z]|$)/is
    )
    if (businessLogicMatch) {
      sections.businessLogic = businessLogicMatch[1].trim()
    }

    const complexityMatch = content.match(/complexity[:\s]+(\w+)/i)
    if (complexityMatch) {
      sections.complexity = complexityMatch[1]
    }

    // Extract lists
    const inputsMatch = content.match(
      /inputs?[:\s]+(.+?)(?=\n\n|\noutputs?|$)/is
    )
    if (inputsMatch) {
      sections.inputs = this.extractListItems(inputsMatch[1])
    }

    const outputsMatch = content.match(
      /outputs?[:\s]+(.+?)(?=\n\n|\nrules?|$)/is
    )
    if (outputsMatch) {
      sections.outputs = this.extractListItems(outputsMatch[1])
    }

    const rulesMatch = content.match(
      /rules?[:\s]+(.+?)(?=\n\n|\ncomplexity|$)/is
    )
    if (rulesMatch) {
      sections.rules = this.extractListItems(rulesMatch[1])
    }

    const recommendationsMatch = content.match(/recommendations?[:\s]+(.+?)$/is)
    if (recommendationsMatch) {
      sections.recommendations = this.extractListItems(recommendationsMatch[1])
    }

    return sections
  }

  /**
   * Extract list items from text
   */
  private extractListItems (text: string): string[] {
    const items: string[] = []
    const lines = text.split('\n')

    for (const line of lines) {
      const trimmed = line.trim()
      if (trimmed && (trimmed.match(/^[-*•\d.]+\s/) || !items.length)) {
        items.push(trimmed.replace(/^[-*•\d.]+\s*/, ''))
      }
    }

    return items.filter(item => item.length > 0)
  }

  /**
   * Generate fallback explanation when LLM is unavailable
   */
  private generateFallbackExplanation (
    ast: AST,
    analysis: LogicAnalysis
  ): ExplanationResponse {
    const summary = `COBOL program ${ast.programId} with ${analysis.businessRules.length} business rules and ${analysis.patterns.length} banking patterns identified.`

    const businessLogic = analysis.businessRules
      .slice(0, 5)
      .map(rule => rule.description)
      .join('. ')

    const inputs = analysis.dataStructures
      .filter(ds => ds.type === 'FILE' || ds.type === 'INPUT')
      .map(ds => ds.name)

    const outputs = analysis.dataStructures
      .filter(ds => ds.type === 'OUTPUT')
      .map(ds => ds.name)

    const rules = analysis.businessRules.map(
      (rule, idx) => `${idx + 1}. ${rule.description}`
    )

    const complexity =
      analysis.businessRules.length > 10
        ? 'High'
        : analysis.businessRules.length > 5
        ? 'Medium'
        : 'Low'

    const recommendations = [
      'Consider modernizing to cloud-native architecture',
      'Review business rules for optimization opportunities',
      'Implement comprehensive testing before migration'
    ]

    return {
      summary,
      businessLogic: businessLogic || 'No detailed business logic available',
      inputs,
      outputs,
      rules,
      complexity,
      recommendations
    }
  }

  /**
   * Get cache statistics
   */
  getCacheStats () {
    return this.cache.getStats()
  }

  /**
   * Clear cache
   */
  clearCache () {
    this.cache.clear()
  }
}

/**
 * Factory function to create explanation generator
 */
export function createExplanationGenerator (
  options: ExplanationGeneratorOptions
): ExplanationGenerator {
  return new ExplanationGenerator(options)
}
