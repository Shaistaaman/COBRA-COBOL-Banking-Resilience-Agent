/**
 * LLM Client for OpenAI and Anthropic APIs
 * Provides unified interface for both providers
 */

import type { LLMConfig, LLMResponse } from './types.js'
import { createError, ErrorCode } from '../utils/error-handler.js'

/**
 * Abstract LLM client interface
 */
export interface LLMClient {
  chat(systemPrompt: string, userPrompt: string): Promise<LLMResponse>
}

/**
 * OpenAI API client
 */
export class OpenAIClient implements LLMClient {
  private apiKey: string
  private model: string
  private temperature: number
  private maxTokens: number

  constructor (config: LLMConfig) {
    this.apiKey = config.apiKey
    this.model = config.model || 'gpt-4o-mini'
    this.temperature = config.temperature ?? 0.3
    this.maxTokens = config.maxTokens ?? 2000
  }

  async chat (systemPrompt: string, userPrompt: string): Promise<LLMResponse> {
    try {
      const controller = new AbortController()
      const timeout = setTimeout(() => controller.abort(), 60000) // 60 second timeout

      const response = await fetch(
        'https://api.openai.com/v1/chat/completions',
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${this.apiKey}`
          },
          body: JSON.stringify({
            model: this.model,
            messages: [
              { role: 'system', content: systemPrompt },
              { role: 'user', content: userPrompt }
            ],
            temperature: this.temperature,
            max_tokens: this.maxTokens
          }),
          signal: controller.signal
        }
      )

      clearTimeout(timeout)

      if (!response.ok) {
        const errorText = await response.text()

        // Handle rate limiting
        if (response.status === 429) {
          const error = createError(
            ErrorCode.LLM_RATE_LIMIT,
            `OpenAI rate limit: ${errorText}`
          )
          throw new Error(error.userMessage)
        }

        // Handle other API errors
        const error = createError(
          ErrorCode.LLM_API_ERROR,
          `OpenAI API error: ${response.status} - ${errorText}`
        )
        throw new Error(error.userMessage)
      }

      const data = (await response.json()) as any

      // Validate response structure
      if (!data.choices || !data.choices[0]?.message?.content) {
        const error = createError(
          ErrorCode.LLM_INVALID_RESPONSE,
          'OpenAI returned invalid response structure'
        )
        throw new Error(error.userMessage)
      }

      const usage = {
        promptTokens: data.usage?.prompt_tokens || 0,
        completionTokens: data.usage?.completion_tokens || 0,
        totalTokens: data.usage?.total_tokens || 0
      }

      // Track cost
      const { getGlobalCostTracker } = await import('./cost-tracker.js')
      const costTracker = getGlobalCostTracker()
      costTracker.trackRequest(
        'openai',
        this.model,
        usage.promptTokens,
        usage.completionTokens
      )

      return {
        content: data.choices[0].message.content,
        usage
      }
    } catch (err) {
      // Handle timeout
      if (err instanceof Error && err.name === 'AbortError') {
        const error = createError(
          ErrorCode.LLM_TIMEOUT,
          'OpenAI request timed out'
        )
        throw new Error(error.userMessage)
      }

      // Re-throw if already a COBRA error
      if (err instanceof Error && err.message.includes('❌')) {
        throw err
      }

      // Wrap unknown errors
      const error = createError(
        ErrorCode.LLM_API_ERROR,
        err instanceof Error ? err.message : String(err)
      )
      throw new Error(error.userMessage)
    }
  }
}

/**
 * Anthropic API client
 */
export class AnthropicClient implements LLMClient {
  private apiKey: string
  private model: string
  private temperature: number
  private maxTokens: number

  constructor (config: LLMConfig) {
    this.apiKey = config.apiKey
    this.model = config.model || 'claude-3-5-sonnet-20241022'
    this.temperature = config.temperature ?? 0.3
    this.maxTokens = config.maxTokens ?? 2000
  }

  async chat (systemPrompt: string, userPrompt: string): Promise<LLMResponse> {
    try {
      const controller = new AbortController()
      const timeout = setTimeout(() => controller.abort(), 60000) // 60 second timeout

      const response = await fetch('https://api.anthropic.com/v1/messages', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-api-key': this.apiKey,
          'anthropic-version': '2023-06-01'
        },
        body: JSON.stringify({
          model: this.model,
          system: systemPrompt,
          messages: [{ role: 'user', content: userPrompt }],
          temperature: this.temperature,
          max_tokens: this.maxTokens
        }),
        signal: controller.signal
      })

      clearTimeout(timeout)

      if (!response.ok) {
        const errorText = await response.text()

        // Handle rate limiting
        if (response.status === 429) {
          const error = createError(
            ErrorCode.LLM_RATE_LIMIT,
            `Anthropic rate limit: ${errorText}`
          )
          throw new Error(error.userMessage)
        }

        // Handle other API errors
        const error = createError(
          ErrorCode.LLM_API_ERROR,
          `Anthropic API error: ${response.status} - ${errorText}`
        )
        throw new Error(error.userMessage)
      }

      const data = (await response.json()) as any

      // Validate response structure
      if (!data.content || !data.content[0]?.text) {
        const error = createError(
          ErrorCode.LLM_INVALID_RESPONSE,
          'Anthropic returned invalid response structure'
        )
        throw new Error(error.userMessage)
      }

      const usage = {
        promptTokens: data.usage?.input_tokens || 0,
        completionTokens: data.usage?.output_tokens || 0,
        totalTokens:
          (data.usage?.input_tokens || 0) + (data.usage?.output_tokens || 0)
      }

      // Track cost
      const { getGlobalCostTracker } = await import('./cost-tracker.js')
      const costTracker = getGlobalCostTracker()
      costTracker.trackRequest(
        'anthropic',
        this.model,
        usage.promptTokens,
        usage.completionTokens
      )

      return {
        content: data.content[0].text,
        usage
      }
    } catch (err) {
      // Handle timeout
      if (err instanceof Error && err.name === 'AbortError') {
        const error = createError(
          ErrorCode.LLM_TIMEOUT,
          'Anthropic request timed out'
        )
        throw new Error(error.userMessage)
      }

      // Re-throw if already a COBRA error
      if (err instanceof Error && err.message.includes('❌')) {
        throw err
      }

      // Wrap unknown errors
      const error = createError(
        ErrorCode.LLM_API_ERROR,
        err instanceof Error ? err.message : String(err)
      )
      throw new Error(error.userMessage)
    }
  }
}

/**
 * Factory function to create appropriate LLM client
 */
export function createLLMClient (config: LLMConfig): LLMClient {
  switch (config.provider) {
    case 'openai':
      return new OpenAIClient(config)
    case 'anthropic':
      return new AnthropicClient(config)
    default:
      throw new Error(`Unsupported LLM provider: ${config.provider}`)
  }
}
