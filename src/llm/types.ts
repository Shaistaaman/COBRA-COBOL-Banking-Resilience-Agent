/**
 * LLM Integration Type Definitions
 */

export type LLMProvider = 'openai' | 'anthropic'

export interface LLMConfig {
  provider: LLMProvider
  apiKey: string
  model?: string
  temperature?: number
  maxTokens?: number
  cacheEnabled?: boolean
}

export interface ExplanationRequest {
  ast: any
  analysis: any
  fileName?: string
}

export interface ExplanationResponse {
  summary: string
  businessLogic: string
  inputs: string[]
  outputs: string[]
  rules: string[]
  complexity: string
  recommendations: string[]
}

export interface LLMResponse {
  content: string
  usage?: {
    promptTokens: number
    completionTokens: number
    totalTokens: number
  }
  cached?: boolean
}

export interface CacheEntry {
  key: string
  response: ExplanationResponse
  timestamp: number
  expiresAt: number
}
