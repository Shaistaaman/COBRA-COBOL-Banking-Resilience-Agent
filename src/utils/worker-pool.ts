/**
 * Worker Thread Pool for Parallel Processing
 * Enables concurrent parsing and analysis of multiple COBOL files
 */

import { Worker } from 'worker_threads'
import { cpus } from 'os'
import path from 'path'
import { fileURLToPath } from 'url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

export interface WorkerTask<T = any> {
  id: string
  type: 'parse' | 'analyze'
  data: any
  resolve: (result: T) => void
  reject: (error: Error) => void
}

export interface WorkerPoolOptions {
  maxWorkers?: number
  workerScript?: string
}

/**
 * Worker pool for parallel COBOL processing
 */
export class WorkerPool {
  private workers: Worker[] = []
  private taskQueue: WorkerTask[] = []
  private activeTasks: Map<string, WorkerTask> = new Map()
  private maxWorkers: number
  private workerScript: string
  private nextTaskId = 0

  constructor (options: WorkerPoolOptions = {}) {
    this.maxWorkers = options.maxWorkers ?? Math.max(2, cpus().length - 1)
    this.workerScript =
      options.workerScript ?? path.join(__dirname, 'worker.js')
  }

  /**
   * Initialize worker pool
   */
  async initialize (): Promise<void> {
    console.log(
      `🔧 Initializing worker pool with ${this.maxWorkers} workers...`
    )

    for (let i = 0; i < this.maxWorkers; i++) {
      try {
        const worker = new Worker(this.workerScript)

        worker.on('message', message => {
          this.handleWorkerMessage(worker, message)
        })

        worker.on('error', error => {
          console.error(`Worker ${i} error:`, error)
        })

        worker.on('exit', code => {
          if (code !== 0) {
            console.error(`Worker ${i} exited with code ${code}`)
          }
        })

        this.workers.push(worker)
      } catch (error) {
        console.warn(`Failed to create worker ${i}:`, error)
      }
    }

    console.log(`✓ Worker pool initialized with ${this.workers.length} workers`)
  }

  /**
   * Execute task in worker thread
   */
  async executeTask<T> (type: 'parse' | 'analyze', data: any): Promise<T> {
    return new Promise((resolve, reject) => {
      const task: WorkerTask<T> = {
        id: `task-${this.nextTaskId++}`,
        type,
        data,
        resolve,
        reject
      }

      this.taskQueue.push(task)
      this.processQueue()
    })
  }

  /**
   * Process task queue
   */
  private processQueue (): void {
    // Find available worker
    const availableWorker = this.workers.find(worker => {
      return ![...this.activeTasks.values()].some(
        task => (task as any).worker === worker
      )
    })

    if (!availableWorker || this.taskQueue.length === 0) {
      return
    }

    const task = this.taskQueue.shift()
    if (!task) return

    // Assign task to worker
    this.activeTasks.set(task.id, task)
    ;(task as any).worker = availableWorker

    availableWorker.postMessage({
      id: task.id,
      type: task.type,
      data: task.data
    })
  }

  /**
   * Handle message from worker
   */
  private handleWorkerMessage (worker: Worker, message: any): void {
    const task = this.activeTasks.get(message.id)
    if (!task) return

    this.activeTasks.delete(message.id)

    if (message.error) {
      task.reject(new Error(message.error))
    } else {
      task.resolve(message.result)
    }

    // Process next task in queue
    this.processQueue()
  }

  /**
   * Parse multiple COBOL files in parallel
   */
  async parseMultiple (sources: string[]): Promise<any[]> {
    if (this.workers.length === 0) {
      await this.initialize()
    }

    const tasks = sources.map(source => this.executeTask('parse', { source }))

    return await Promise.all(tasks)
  }

  /**
   * Analyze multiple ASTs in parallel
   */
  async analyzeMultiple (asts: any[]): Promise<any[]> {
    if (this.workers.length === 0) {
      await this.initialize()
    }

    const tasks = asts.map(ast => this.executeTask('analyze', { ast }))

    return await Promise.all(tasks)
  }

  /**
   * Get pool statistics
   */
  getStats (): {
    totalWorkers: number
    activeWorkers: number
    queuedTasks: number
    activeTasks: number
  } {
    return {
      totalWorkers: this.workers.length,
      activeWorkers: this.activeTasks.size,
      queuedTasks: this.taskQueue.length,
      activeTasks: this.activeTasks.size
    }
  }

  /**
   * Shutdown worker pool
   */
  async shutdown (): Promise<void> {
    console.log('🛑 Shutting down worker pool...')

    for (const worker of this.workers) {
      await worker.terminate()
    }

    this.workers = []
    this.taskQueue = []
    this.activeTasks.clear()

    console.log('✓ Worker pool shut down')
  }
}

/**
 * Global worker pool instance
 */
let globalPool: WorkerPool | null = null

/**
 * Get or create global worker pool
 */
export function getGlobalWorkerPool (): WorkerPool {
  if (!globalPool) {
    globalPool = new WorkerPool()
  }
  return globalPool
}

/**
 * Reset global worker pool
 */
export async function resetGlobalWorkerPool (): Promise<void> {
  if (globalPool) {
    await globalPool.shutdown()
    globalPool = null
  }
}
