/**
 * Example File Preloader
 * Pre-parses example COBOL files at startup for instant demo
 */

import fs from 'fs/promises'
import path from 'path'
import { fileURLToPath } from 'url'
import type { ParseResult, LogicAnalysis } from '../mcp-server/types.js'
import { getGlobalCache } from './performance-cache.js'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

export interface PreloadedExample {
  name: string
  source: string
  parseResult: ParseResult
  analysis: LogicAnalysis
  description: string
}

/**
 * Example file preloader
 */
export class ExamplePreloader {
  private examples: Map<string, PreloadedExample> = new Map()
  private examplesDir: string
  private isPreloaded = false

  constructor (examplesDir?: string) {
    // Default to examples directory at project root
    this.examplesDir = examplesDir ?? path.join(__dirname, '../../examples')
  }

  /**
   * Preload all example COBOL files
   */
  async preloadExamples (): Promise<void> {
    if (this.isPreloaded) {
      console.log('✓ Examples already preloaded')
      return
    }

    console.log('📚 Preloading example COBOL files...')
    const startTime = Date.now()

    try {
      // Check if examples directory exists
      try {
        await fs.access(this.examplesDir)
      } catch {
        console.warn(`⚠️  Examples directory not found: ${this.examplesDir}`)
        return
      }

      // Read all .cbl files
      const files = await fs.readdir(this.examplesDir)
      const cobolFiles = files.filter(
        f => f.endsWith('.cbl') || f.endsWith('.cob')
      )

      if (cobolFiles.length === 0) {
        console.warn('⚠️  No COBOL example files found')
        return
      }

      // Preload each file
      for (const file of cobolFiles) {
        await this.preloadFile(file)
      }

      const elapsed = Date.now() - startTime
      console.log(`✓ Preloaded ${this.examples.size} examples in ${elapsed}ms`)
      this.isPreloaded = true
    } catch (error) {
      console.error('❌ Failed to preload examples:', error)
    }
  }

  /**
   * Preload single example file
   */
  private async preloadFile (filename: string): Promise<void> {
    try {
      const filePath = path.join(this.examplesDir, filename)
      const source = await fs.readFile(filePath, 'utf-8')

      // Import parser and analyzer dynamically to avoid circular deps
      const { createParser } = await import('../parser/cobol-parser.js')
      const { createLogicAnalyzer } = await import('../analyzer/index.js')

      const parser = createParser()
      const analyzer = createLogicAnalyzer()

      // Parse and analyze
      const parseResult = await parser.parse(source)

      if (!parseResult.ast) {
        console.warn(`⚠️  Failed to parse ${filename}`)
        return
      }

      const analysis = analyzer.analyze(parseResult.ast, filename)

      // Cache results
      const cache = getGlobalCache()
      cache.cacheAST(source, parseResult)
      cache.cacheAnalysis(parseResult.ast.programId, analysis)

      // Store in examples map
      const name = path.basename(filename, path.extname(filename))
      this.examples.set(name, {
        name,
        source,
        parseResult,
        analysis,
        description: this.getDescription(name)
      })

      console.log(`  ✓ Preloaded ${filename}`)
    } catch (error) {
      console.error(`  ❌ Failed to preload ${filename}:`, error)
    }
  }

  /**
   * Get description for example file
   */
  private getDescription (name: string): string {
    const descriptions: Record<string, string> = {
      'interest-calculation':
        'Simple interest calculation using daily rate and balance',
      'transaction-posting':
        'Transaction validation and posting with balance checks',
      'batch-reconciliation':
        'File reconciliation with match/exception reporting'
    }

    return descriptions[name] ?? 'COBOL example program'
  }

  /**
   * Get preloaded example by name
   */
  getExample (name: string): PreloadedExample | undefined {
    return this.examples.get(name)
  }

  /**
   * Get all preloaded examples
   */
  getAllExamples (): PreloadedExample[] {
    return Array.from(this.examples.values())
  }

  /**
   * Check if examples are preloaded
   */
  isReady (): boolean {
    return this.isPreloaded
  }

  /**
   * Get example names
   */
  getExampleNames (): string[] {
    return Array.from(this.examples.keys())
  }

  /**
   * Clear preloaded examples
   */
  clear (): void {
    this.examples.clear()
    this.isPreloaded = false
  }
}

/**
 * Global preloader instance
 */
let globalPreloader: ExamplePreloader | null = null

/**
 * Get or create global preloader
 */
export function getGlobalPreloader (): ExamplePreloader {
  if (!globalPreloader) {
    globalPreloader = new ExamplePreloader()
  }
  return globalPreloader
}

/**
 * Preload examples at startup
 */
export async function preloadExamplesAtStartup (): Promise<void> {
  const preloader = getGlobalPreloader()
  await preloader.preloadExamples()
}
