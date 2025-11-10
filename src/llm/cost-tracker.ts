/**
 * LLM Cost Tracker
 * Tracks and reports LLM API usage costs
 */

export interface CostEntry {
  timestamp: number
  provider: 'openai' | 'anthropic'
  model: string
  promptTokens: number
  completionTokens: number
  totalTokens: number
  estimatedCost: number
}

export interface CostSummary {
  totalRequests: number
  totalTokens: number
  totalCost: number
  costByProvider: Record<string, number>
  costByModel: Record<string, number>
  averageCostPerRequest: number
  period: {
    start: number
    end: number
  }
}

/**
 * Pricing information for LLM providers (as of 2024)
 */
const PRICING = {
  openai: {
    'gpt-4o': {
      input: 0.0025 / 1000, // $2.50 per 1M tokens
      output: 0.01 / 1000 // $10 per 1M tokens
    },
    'gpt-4o-mini': {
      input: 0.00015 / 1000, // $0.15 per 1M tokens
      output: 0.0006 / 1000 // $0.60 per 1M tokens
    },
    'gpt-4-turbo': {
      input: 0.01 / 1000, // $10 per 1M tokens
      output: 0.03 / 1000 // $30 per 1M tokens
    },
    'gpt-3.5-turbo': {
      input: 0.0005 / 1000, // $0.50 per 1M tokens
      output: 0.0015 / 1000 // $1.50 per 1M tokens
    }
  },
  anthropic: {
    'claude-3-5-sonnet-20241022': {
      input: 0.003 / 1000, // $3 per 1M tokens
      output: 0.015 / 1000 // $15 per 1M tokens
    },
    'claude-3-opus-20240229': {
      input: 0.015 / 1000, // $15 per 1M tokens
      output: 0.075 / 1000 // $75 per 1M tokens
    },
    'claude-3-sonnet-20240229': {
      input: 0.003 / 1000, // $3 per 1M tokens
      output: 0.015 / 1000 // $15 per 1M tokens
    },
    'claude-3-haiku-20240307': {
      input: 0.00025 / 1000, // $0.25 per 1M tokens
      output: 0.00125 / 1000 // $1.25 per 1M tokens
    }
  }
}

/**
 * LLM cost tracker
 */
export class LLMCostTracker {
  private entries: CostEntry[] = []
  private startTime: number

  constructor () {
    this.startTime = Date.now()
  }

  /**
   * Track LLM API request
   */
  trackRequest (
    provider: 'openai' | 'anthropic',
    model: string,
    promptTokens: number,
    completionTokens: number
  ): number {
    const totalTokens = promptTokens + completionTokens
    const cost = this.calculateCost(
      provider,
      model,
      promptTokens,
      completionTokens
    )

    this.entries.push({
      timestamp: Date.now(),
      provider,
      model,
      promptTokens,
      completionTokens,
      totalTokens,
      estimatedCost: cost
    })

    return cost
  }

  /**
   * Calculate cost for request
   */
  private calculateCost (
    provider: 'openai' | 'anthropic',
    model: string,
    promptTokens: number,
    completionTokens: number
  ): number {
    const providerPricing = PRICING[provider]
    const modelPricing = providerPricing[
      model as keyof typeof providerPricing
    ] as { input: number; output: number } | undefined

    if (!modelPricing) {
      // Use default pricing if model not found
      console.warn(`Unknown model ${model}, using default pricing`)
      return (promptTokens + completionTokens) * 0.00001 // $0.01 per 1K tokens
    }

    const inputCost = promptTokens * modelPricing.input
    const outputCost = completionTokens * modelPricing.output

    return inputCost + outputCost
  }

  /**
   * Get cost summary
   */
  getSummary (): CostSummary {
    const totalRequests = this.entries.length
    const totalTokens = this.entries.reduce((sum, e) => sum + e.totalTokens, 0)
    const totalCost = this.entries.reduce((sum, e) => sum + e.estimatedCost, 0)

    // Cost by provider
    const costByProvider: Record<string, number> = {}
    for (const entry of this.entries) {
      costByProvider[entry.provider] =
        (costByProvider[entry.provider] || 0) + entry.estimatedCost
    }

    // Cost by model
    const costByModel: Record<string, number> = {}
    for (const entry of this.entries) {
      costByModel[entry.model] =
        (costByModel[entry.model] || 0) + entry.estimatedCost
    }

    return {
      totalRequests,
      totalTokens,
      totalCost,
      costByProvider,
      costByModel,
      averageCostPerRequest: totalRequests > 0 ? totalCost / totalRequests : 0,
      period: {
        start: this.startTime,
        end: Date.now()
      }
    }
  }

  /**
   * Get formatted cost report
   */
  getFormattedReport (): string {
    const summary = this.getSummary()
    const durationHours =
      (summary.period.end - summary.period.start) / 1000 / 3600

    let report = `
💰 LLM Cost Tracker Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Total Requests: ${summary.totalRequests}
Total Tokens: ${summary.totalTokens.toLocaleString()}
Total Cost: $${summary.totalCost.toFixed(4)}
Average Cost/Request: $${summary.averageCostPerRequest.toFixed(4)}
Tracking Period: ${durationHours.toFixed(1)} hours

Cost by Provider:
`

    for (const [provider, cost] of Object.entries(summary.costByProvider)) {
      const percentage = (cost / summary.totalCost) * 100
      report += `  ${provider}: $${cost.toFixed(4)} (${percentage.toFixed(
        1
      )}%)\n`
    }

    report += `\nCost by Model:\n`
    for (const [model, cost] of Object.entries(summary.costByModel)) {
      const percentage = (cost / summary.totalCost) * 100
      report += `  ${model}: $${cost.toFixed(4)} (${percentage.toFixed(1)}%)\n`
    }

    // Projected monthly cost
    const projectedMonthlyCost =
      durationHours > 0 ? (summary.totalCost / durationHours) * 24 * 30 : 0

    report += `\n📊 Projected Monthly Cost: $${projectedMonthlyCost.toFixed(2)}`
    report += `\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`

    return report.trim()
  }

  /**
   * Get recent entries
   */
  getRecentEntries (count: number = 10): CostEntry[] {
    return this.entries.slice(-count)
  }

  /**
   * Clear all entries
   */
  clear (): void {
    this.entries = []
    this.startTime = Date.now()
  }

  /**
   * Export entries to JSON
   */
  exportToJSON (): string {
    return JSON.stringify(
      {
        summary: this.getSummary(),
        entries: this.entries
      },
      null,
      2
    )
  }

  /**
   * Get cost alert if threshold exceeded
   */
  checkCostAlert (thresholdDollars: number): {
    alert: boolean
    message: string
  } {
    const summary = this.getSummary()

    if (summary.totalCost >= thresholdDollars) {
      return {
        alert: true,
        message: `⚠️  Cost threshold exceeded: $${summary.totalCost.toFixed(
          2
        )} >= $${thresholdDollars.toFixed(2)}`
      }
    }

    const remaining = thresholdDollars - summary.totalCost
    const percentUsed = (summary.totalCost / thresholdDollars) * 100

    return {
      alert: false,
      message: `✓ Cost within threshold: $${summary.totalCost.toFixed(
        2
      )} / $${thresholdDollars.toFixed(2)} (${percentUsed.toFixed(
        1
      )}% used, $${remaining.toFixed(2)} remaining)`
    }
  }
}

/**
 * Global cost tracker instance
 */
let globalTracker: LLMCostTracker | null = null

/**
 * Get or create global cost tracker
 */
export function getGlobalCostTracker (): LLMCostTracker {
  if (!globalTracker) {
    globalTracker = new LLMCostTracker()
  }
  return globalTracker
}

/**
 * Reset global cost tracker
 */
export function resetGlobalCostTracker (): void {
  globalTracker = null
}
