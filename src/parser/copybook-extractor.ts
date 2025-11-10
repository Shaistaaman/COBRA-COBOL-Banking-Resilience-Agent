/**
 * COBOL Copybook Extraction Module
 * Extracts and processes COBOL copybooks from source code
 */

import type { Copybook } from './types.js'

/**
 * Extract copybook references from COBOL source
 */
export function extractCopybooks (source: string): Copybook[] {
  const copybooks: Copybook[] = []
  const lines = source.split('\n')

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim()

    // Match COPY statement patterns
    // COPY COPYBOOK-NAME
    // COPY "COPYBOOK-NAME"
    // COPY 'COPYBOOK-NAME'
    // COPY COPYBOOK-NAME IN LIBRARY
    const copyMatch = line.match(
      /COPY\s+(?:["'])?([A-Z0-9-]+)(?:["'])?\s*(?:IN\s+([A-Z0-9-]+))?/i
    )

    if (copyMatch) {
      const name = copyMatch[1]
      const location = copyMatch[2] || undefined

      copybooks.push({
        name,
        content: '', // Content would be loaded from external file
        location
      })
    }
  }

  return copybooks
}

/**
 * Extract copybook content from a file or string
 */
export function parseCopybookContent (content: string, name: string): Copybook {
  return {
    name,
    content,
    location: undefined
  }
}

/**
 * Resolve copybook references in source code
 * Replaces COPY statements with actual copybook content
 */
export function resolveCopybooks (
  source: string,
  copybookMap: Map<string, string>
): string {
  let resolvedSource = source
  const lines = source.split('\n')
  const result: string[] = []

  for (const line of lines) {
    const copyMatch = line.match(/COPY\s+(?:["'])?([A-Z0-9-]+)(?:["'])?/i)

    if (copyMatch) {
      const copybookName = copyMatch[1]
      const copybookContent = copybookMap.get(copybookName)

      if (copybookContent) {
        // Replace COPY statement with copybook content
        result.push(`      * BEGIN COPYBOOK ${copybookName}`)
        result.push(copybookContent)
        result.push(`      * END COPYBOOK ${copybookName}`)
      } else {
        // Keep original COPY statement if copybook not found
        result.push(line)
      }
    } else {
      result.push(line)
    }
  }

  return result.join('\n')
}

/**
 * Extract data structures from copybook content
 */
export interface CopybookDataStructure {
  name: string
  level: number
  picture?: string
  usage?: string
  value?: string
  children: CopybookDataStructure[]
}

export function extractDataStructures (
  copybookContent: string
): CopybookDataStructure[] {
  const structures: CopybookDataStructure[] = []
  const lines = copybookContent.split('\n')
  const stack: CopybookDataStructure[] = []

  for (const line of lines) {
    const trimmed = line.trim()
    if (!trimmed || trimmed.startsWith('*')) continue

    const element = parseDataStructureLine(trimmed)
    if (!element) continue

    // Build hierarchy based on level numbers
    while (stack.length > 0 && stack[stack.length - 1].level >= element.level) {
      stack.pop()
    }

    if (stack.length === 0) {
      structures.push(element)
    } else {
      stack[stack.length - 1].children.push(element)
    }

    stack.push(element)
  }

  return structures
}

/**
 * Parse a single data structure line
 */
function parseDataStructureLine (line: string): CopybookDataStructure | null {
  // Match: level-number name [PIC ...] [USAGE ...] [VALUE ...]
  const match = line.match(/^\s*(\d{2})\s+([A-Z0-9-]+)(?:\s+(.+))?/i)
  if (!match) return null

  const level = parseInt(match[1], 10)
  const name = match[2]
  const rest = match[3] || ''

  // Extract PIC clause
  const picMatch = rest.match(/PIC(?:TURE)?\s+(?:IS\s+)?([A-Z0-9()V]+)/i)
  const picture = picMatch ? picMatch[1] : undefined

  // Extract USAGE clause
  const usageMatch = rest.match(/USAGE\s+(?:IS\s+)?([A-Z-]+)/i)
  const usage = usageMatch ? usageMatch[1] : undefined

  // Extract VALUE clause
  const valueMatch = rest.match(/VALUE\s+(?:IS\s+)?([^.\s]+)/i)
  const value = valueMatch ? valueMatch[1].replace(/['"]/g, '') : undefined

  return {
    name,
    level,
    picture,
    usage,
    value,
    children: []
  }
}

/**
 * Validate copybook syntax
 */
export function validateCopybook (content: string): {
  valid: boolean
  errors: string[]
} {
  const errors: string[] = []
  const lines = content.split('\n')

  let hasDataElements = false

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim()
    if (!line || line.startsWith('*')) continue

    // Check for valid level numbers (01-49, 66, 77, 88)
    const levelMatch = line.match(/^\s*(\d{2})/)
    if (levelMatch) {
      const level = parseInt(levelMatch[1], 10)
      if (
        (level < 1 || level > 49) &&
        level !== 66 &&
        level !== 77 &&
        level !== 88
      ) {
        errors.push(`Line ${i + 1}: Invalid level number ${level}`)
      }
      hasDataElements = true
    }
  }

  if (!hasDataElements) {
    errors.push('Copybook contains no data elements')
  }

  return {
    valid: errors.length === 0,
    errors
  }
}
