/**
 * COBOL Data Structure Parser
 * Converts COBOL data structures to TypeScript type definitions
 */

import type { DataElement } from '../mcp-server/types.js'

/**
 * TypeScript type information derived from COBOL PIC clause
 */
export interface TypeScriptType {
  tsType: string
  isArray: boolean
  length?: number
  decimals?: number
  description: string
}

/**
 * Parse COBOL PIC clause and convert to TypeScript type
 */
export function picToTypeScript (picture: string): TypeScriptType {
  if (!picture) {
    return {
      tsType: 'unknown',
      isArray: false,
      description: 'No PIC clause specified'
    }
  }

  const pic = picture.toUpperCase().trim()

  // Numeric types
  if (pic.match(/^9+$/) || pic.match(/^9\(\d+\)$/)) {
    const length = extractLength(pic, '9')
    return {
      tsType: 'number',
      isArray: false,
      length,
      description: `Numeric field, ${length} digits`
    }
  }

  // Numeric with decimal (V indicates decimal point)
  if (pic.includes('V')) {
    const parts = pic.split('V')
    const integerLength = extractLength(parts[0], '9')
    const decimalLength = extractLength(parts[1], '9')
    return {
      tsType: 'number',
      isArray: false,
      length: integerLength + decimalLength,
      decimals: decimalLength,
      description: `Decimal number, ${integerLength} digits, ${decimalLength} decimal places`
    }
  }

  // Signed numeric (S prefix)
  if (pic.match(/^S9+/) || pic.match(/^S9\(\d+\)/)) {
    const length = extractLength(pic.replace('S', ''), '9')
    return {
      tsType: 'number',
      isArray: false,
      length,
      description: `Signed numeric field, ${length} digits`
    }
  }

  // Alphanumeric (X)
  if (pic.match(/^X+$/) || pic.match(/^X\(\d+\)$/)) {
    const length = extractLength(pic, 'X')
    return {
      tsType: 'string',
      isArray: false,
      length,
      description: `Alphanumeric field, ${length} characters`
    }
  }

  // Alphabetic (A)
  if (pic.match(/^A+$/) || pic.match(/^A\(\d+\)$/)) {
    const length = extractLength(pic, 'A')
    return {
      tsType: 'string',
      isArray: false,
      length,
      description: `Alphabetic field, ${length} characters`
    }
  }

  // Packed decimal (COMP-3)
  if (pic.includes('COMP-3') || pic.includes('COMPUTATIONAL-3')) {
    return {
      tsType: 'number',
      isArray: false,
      description: 'Packed decimal (COMP-3)'
    }
  }

  // Binary (COMP)
  if (pic.includes('COMP') && !pic.includes('COMP-3')) {
    return {
      tsType: 'number',
      isArray: false,
      description: 'Binary (COMP)'
    }
  }

  // Default to string for unknown patterns
  return {
    tsType: 'string',
    isArray: false,
    description: `Unknown PIC pattern: ${picture}`
  }
}

/**
 * Extract length from PIC clause
 */
function extractLength (pic: string, char: string): number {
  // Extract from parentheses first (e.g., "X(10)" = 10, "9(13)" = 13)
  const parenMatch = pic.match(new RegExp(`${char}\\((\\d+)\\)`, 'i'))
  if (parenMatch) {
    return parseInt(parenMatch[1], 10)
  }

  // Count repeated characters (e.g., "XXX" = 3, "999" = 3)
  const repeatMatch = pic.match(new RegExp(`${char}+`, 'gi'))
  if (repeatMatch) {
    return repeatMatch.join('').length
  }

  return 0
}

/**
 * Generate TypeScript interface from COBOL data structure
 */
export interface TypeScriptInterface {
  name: string
  code: string
  fields: TypeScriptField[]
}

export interface TypeScriptField {
  name: string
  type: string
  optional: boolean
  description: string
}

/**
 * Convert COBOL data elements to TypeScript interface
 */
export function generateTypeScriptInterface (
  elements: DataElement[],
  interfaceName: string
): TypeScriptInterface {
  const fields: TypeScriptField[] = []
  const hierarchy = buildHierarchy(elements)

  for (const element of hierarchy) {
    const field = convertElementToField(element, elements)
    if (field) {
      fields.push(field)
    }
  }

  const code = generateInterfaceCode(interfaceName, fields)

  return {
    name: interfaceName,
    code,
    fields
  }
}

/**
 * Build hierarchical structure from flat data elements
 */
function buildHierarchy (elements: DataElement[]): DataElement[] {
  const topLevel: DataElement[] = []

  for (const element of elements) {
    if (element.level === 1 || element.level === 77) {
      topLevel.push(element)
    }
  }

  return topLevel
}

/**
 * Convert COBOL data element to TypeScript field
 */
function convertElementToField (
  element: DataElement,
  allElements: DataElement[]
): TypeScriptField | null {
  // Skip FILLER fields
  if (element.name === 'FILLER') {
    return null
  }

  // Convert name to camelCase
  const fieldName = toCamelCase(element.name)

  // Determine if this is a group item (has children)
  const hasChildren = hasChildElements(element, allElements)

  let fieldType: string
  let description: string

  if (hasChildren) {
    // Group item - create nested interface
    const childInterfaceName = toPascalCase(element.name)
    fieldType = childInterfaceName
    description = `Group item: ${element.name}`
  } else if (element.picture) {
    // Elementary item with PIC clause
    const tsType = picToTypeScript(element.picture)
    fieldType = tsType.tsType
    description = tsType.description
  } else {
    // No PIC clause, default to unknown
    fieldType = 'unknown'
    description = 'No type information available'
  }

  return {
    name: fieldName,
    type: fieldType,
    optional: false,
    description
  }
}

/**
 * Check if element has child elements
 */
function hasChildElements (
  element: DataElement,
  allElements: DataElement[]
): boolean {
  const elementIndex = allElements.indexOf(element)
  if (elementIndex === -1 || elementIndex === allElements.length - 1) {
    return false
  }

  const nextElement = allElements[elementIndex + 1]
  return nextElement.level > element.level
}

/**
 * Generate TypeScript interface code
 */
function generateInterfaceCode (
  name: string,
  fields: TypeScriptField[]
): string {
  const lines: string[] = []

  lines.push(`export interface ${name} {`)

  for (const field of fields) {
    if (field.description) {
      lines.push(`  /** ${field.description} */`)
    }
    const optional = field.optional ? '?' : ''
    lines.push(`  ${field.name}${optional}: ${field.type}`)
  }

  lines.push('}')

  return lines.join('\n')
}

/**
 * Convert COBOL name to camelCase
 */
function toCamelCase (name: string): string {
  const parts = name.toLowerCase().split('-')
  return parts
    .map((part, index) => {
      if (index === 0) return part
      return part.charAt(0).toUpperCase() + part.slice(1)
    })
    .join('')
}

/**
 * Convert COBOL name to PascalCase
 */
function toPascalCase (name: string): string {
  const parts = name.toLowerCase().split('-')
  return parts
    .map(part => part.charAt(0).toUpperCase() + part.slice(1))
    .join('')
}

/**
 * Extract nested data structures (01-49 level numbers)
 */
export interface NestedDataStructure {
  parent: DataElement
  children: DataElement[]
  depth: number
}

export function extractNestedStructures (
  elements: DataElement[]
): NestedDataStructure[] {
  const structures: NestedDataStructure[] = []
  const stack: { element: DataElement; depth: number }[] = []

  for (let i = 0; i < elements.length; i++) {
    const element = elements[i]

    // Pop stack until we find the parent
    while (
      stack.length > 0 &&
      stack[stack.length - 1].element.level >= element.level
    ) {
      stack.pop()
    }

    const depth = stack.length

    // If this is a group item (level 01-49, not 77 or 88)
    if (
      element.level >= 1 &&
      element.level <= 49 &&
      element.level !== 77 &&
      element.level !== 88
    ) {
      // Check if it has children
      const children: DataElement[] = []
      for (let j = i + 1; j < elements.length; j++) {
        if (elements[j].level <= element.level) break
        if (stack.length === 0 || elements[j].level > element.level) {
          children.push(elements[j])
        }
      }

      if (children.length > 0) {
        structures.push({
          parent: element,
          children,
          depth
        })
      }

      stack.push({ element, depth })
    }
  }

  return structures
}

/**
 * Extract file definitions from DATA DIVISION
 */
export interface FileStructure {
  name: string
  organization: 'SEQUENTIAL' | 'INDEXED' | 'RELATIVE'
  accessMode: 'SEQUENTIAL' | 'RANDOM' | 'DYNAMIC'
  recordFormat?: string
  keyFields?: string[]
}

export function parseFileDefinition (lines: string[]): FileStructure | null {
  let name = ''
  let organization: FileStructure['organization'] = 'SEQUENTIAL'
  let accessMode: FileStructure['accessMode'] = 'SEQUENTIAL'
  const keyFields: string[] = []

  for (const line of lines) {
    const trimmed = line.trim()

    // FD file-name
    const fdMatch = trimmed.match(/^FD\s+([A-Z0-9-]+)/i)
    if (fdMatch) {
      name = fdMatch[1]
    }

    // ORGANIZATION IS ...
    const orgMatch = trimmed.match(/ORGANIZATION\s+(?:IS\s+)?(\w+)/i)
    if (orgMatch) {
      const org = orgMatch[1].toUpperCase()
      if (org === 'SEQUENTIAL' || org === 'INDEXED' || org === 'RELATIVE') {
        organization = org
      }
    }

    // ACCESS MODE IS ...
    const accessMatch = trimmed.match(/ACCESS\s+(?:MODE\s+)?(?:IS\s+)?(\w+)/i)
    if (accessMatch) {
      const access = accessMatch[1].toUpperCase()
      if (
        access === 'SEQUENTIAL' ||
        access === 'RANDOM' ||
        access === 'DYNAMIC'
      ) {
        accessMode = access
      }
    }

    // RECORD KEY IS ...
    const keyMatch = trimmed.match(/RECORD\s+KEY\s+(?:IS\s+)?([A-Z0-9-]+)/i)
    if (keyMatch) {
      keyFields.push(keyMatch[1])
    }
  }

  if (!name) return null

  return {
    name,
    organization,
    accessMode,
    keyFields: keyFields.length > 0 ? keyFields : undefined
  }
}

/**
 * Extract database schema information from COBOL
 */
export interface DatabaseSchema {
  tables: TableDefinition[]
  relationships: Relationship[]
}

export interface TableDefinition {
  name: string
  columns: ColumnDefinition[]
}

export interface ColumnDefinition {
  name: string
  type: string
  length?: number
  nullable: boolean
  primaryKey: boolean
}

export interface Relationship {
  fromTable: string
  toTable: string
  type: 'one-to-one' | 'one-to-many' | 'many-to-many'
  foreignKey: string
}

/**
 * Extract database schema from EXEC SQL statements
 */
export function extractDatabaseSchema (source: string): DatabaseSchema {
  const tables: TableDefinition[] = []
  const relationships: Relationship[] = []

  // Look for EXEC SQL statements
  const sqlBlocks = source.match(/EXEC\s+SQL([\s\S]*?)END-EXEC/gi)

  if (sqlBlocks) {
    for (const block of sqlBlocks) {
      // Extract table references from SELECT, INSERT, UPDATE, DELETE
      const tableMatches = block.match(/(?:FROM|INTO|UPDATE)\s+([A-Z0-9_]+)/gi)
      if (tableMatches) {
        for (const match of tableMatches) {
          const tableName = match.split(/\s+/)[1]
          if (!tables.find(t => t.name === tableName)) {
            tables.push({
              name: tableName,
              columns: []
            })
          }
        }
      }
    }
  }

  return {
    tables,
    relationships
  }
}
