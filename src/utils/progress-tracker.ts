/**
 * Progress Tracking Utilities
 * Provides progress updates for long-running operations
 */

export interface ProgressUpdate {
  stage: string
  progress: number // 0-100
  message: string
  timestamp: Date
  details?: any
}

export type ProgressCallback = (update: ProgressUpdate) => void

/**
 * Progress tracker for multi-stage operations
 */
export class ProgressTracker {
  private stages: Map<string, { weight: number; progress: number }>
  private currentStage: string | null = null
  private callback: ProgressCallback | null = null
  private startTime: Date

  constructor (stages: Record<string, number>, callback?: ProgressCallback) {
    this.stages = new Map()
    this.callback = callback || null
    this.startTime = new Date()

    // Normalize weights to sum to 100
    const totalWeight = Object.values(stages).reduce((sum, w) => sum + w, 0)
    Object.entries(stages).forEach(([stage, weight]) => {
      this.stages.set(stage, {
        weight: (weight / totalWeight) * 100,
        progress: 0
      })
    })
  }

  /**
   * Start a new stage
   */
  startStage (stage: string, message: string): void {
    if (!this.stages.has(stage)) {
      throw new Error(`Unknown stage: ${stage}`)
    }

    this.currentStage = stage
    this.updateProgress(stage, 0, message)
  }

  /**
   * Update progress for current stage
   */
  updateProgress (
    stage: string,
    progress: number,
    message: string,
    details?: any
  ): void {
    const stageInfo = this.stages.get(stage)
    if (!stageInfo) {
      throw new Error(`Unknown stage: ${stage}`)
    }

    stageInfo.progress = Math.min(100, Math.max(0, progress))

    const totalProgress = this.calculateTotalProgress()

    if (this.callback) {
      this.callback({
        stage,
        progress: totalProgress,
        message,
        timestamp: new Date(),
        details
      })
    }
  }

  /**
   * Complete a stage
   */
  completeStage (stage: string, message: string): void {
    this.updateProgress(stage, 100, message)
  }

  /**
   * Calculate overall progress
   */
  private calculateTotalProgress (): number {
    let total = 0
    this.stages.forEach(stageInfo => {
      total += (stageInfo.progress / 100) * stageInfo.weight
    })
    return Math.round(total)
  }

  /**
   * Get elapsed time
   */
  getElapsedTime (): number {
    return Date.now() - this.startTime.getTime()
  }

  /**
   * Get formatted elapsed time
   */
  getFormattedElapsedTime (): string {
    const elapsed = this.getElapsedTime()
    const seconds = Math.floor(elapsed / 1000)
    const minutes = Math.floor(seconds / 60)
    const remainingSeconds = seconds % 60

    if (minutes > 0) {
      return `${minutes}m ${remainingSeconds}s`
    }
    return `${seconds}s`
  }
}

/**
 * Create a simple progress reporter for console output
 */
export function createConsoleProgressReporter (): ProgressCallback {
  return (update: ProgressUpdate) => {
    const bar = createProgressBar(update.progress)
    console.log(
      `[${update.stage}] ${bar} ${update.progress}% - ${update.message}`
    )
  }
}

/**
 * Create a progress bar string
 */
function createProgressBar (progress: number, width: number = 20): string {
  const filled = Math.round((progress / 100) * width)
  const empty = width - filled
  return '█'.repeat(filled) + '░'.repeat(empty)
}

/**
 * Create a no-op progress callback
 */
export function createNoOpProgressCallback (): ProgressCallback {
  return () => {
    // Do nothing
  }
}

/**
 * Estimate remaining time based on progress
 */
export function estimateRemainingTime (
  startTime: Date,
  currentProgress: number
): string {
  if (currentProgress === 0) {
    return 'Calculating...'
  }

  const elapsed = Date.now() - startTime.getTime()
  const estimatedTotal = (elapsed / currentProgress) * 100
  const remaining = estimatedTotal - elapsed

  const seconds = Math.floor(remaining / 1000)
  const minutes = Math.floor(seconds / 60)
  const remainingSeconds = seconds % 60

  if (minutes > 0) {
    return `~${minutes}m ${remainingSeconds}s remaining`
  }
  return `~${seconds}s remaining`
}
