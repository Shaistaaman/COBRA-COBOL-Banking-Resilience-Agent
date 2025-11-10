/**
 * Data Flow Analysis Module
 * Tracks data movement and transformations through COBOL programs
 */

import type {
  AST,
  Statement,
  DataElement,
  SourceLocation
} from '../mcp-server/types.js'
import type { DataFlowGraph, DataFlowNode, DataFlowEdge } from './types.js'

export interface DataFlowAnalyzerOptions {
  trackWorkingStorage?: boolean
  includeFileIO?: boolean
}

/**
 * Data Flow Analyzer
 */
export class DataFlowAnalyzer {
  private options: DataFlowAnalyzerOptions
  private nodeIdCounter: number = 0

  constructor (options: DataFlowAnalyzerOptions = {}) {
    this.options = {
      trackWorkingStorage: options.trackWorkingStorage ?? true,
      includeFileIO: options.includeFileIO ?? true
    }
  }

  /**
   * Build complete data flow graph from AST
   */
  buildDataFlowGraph (ast: AST): DataFlowGraph {
    const nodes: DataFlowNode[] = []
    const edges: DataFlowEdge[] = []

    if (!ast.divisions.procedure) {
      return { nodes, edges }
    }

    const statements = ast.divisions.procedure.statements
    const dataElements = ast.divisions.data?.workingStorage || []

    // Track variable assignments
    const assignments = this.trackVariableAssignments(statements, dataElements)

    // Identify input sources
    const inputs = this.identifyInputSources(statements, dataElements)
    nodes.push(...inputs)

    // Identify output destinations
    const outputs = this.identifyOutputDestinations(statements, dataElements)
    nodes.push(...outputs)

    // Identify transformations
    const transformations = this.identifyTransformations(
      statements,
      dataElements
    )
    nodes.push(...transformations)

    // Add working storage nodes if enabled
    if (this.options.trackWorkingStorage) {
      const storageNodes = this.createStorageNodes(dataElements)
      nodes.push(...storageNodes)
    }

    // Build edges from assignments
    edges.push(...this.buildEdgesFromAssignments(assignments, nodes))

    // Build edges from data flow
    edges.push(...this.buildDataFlowEdges(statements, nodes, dataElements))

    return { nodes, edges }
  }

  /**
   * Track variable assignments through working storage
   */
  private trackVariableAssignments (
    statements: Statement[],
    dataElements: DataElement[]
  ): Map<string, Set<string>> {
    const assignments = new Map<string, Set<string>>()

    for (const stmt of statements) {
      if (!stmt.raw) continue

      const raw = stmt.raw.toUpperCase()

      // Track MOVE statements
      if (stmt.type === 'MOVE') {
        const moveMatch = raw.match(/MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i)
        if (moveMatch) {
          const source = moveMatch[1]
          const target = moveMatch[2]
          this.addAssignment(assignments, target, source)
        }
      }

      // Track COMPUTE statements
      if (stmt.type === 'COMPUTE') {
        const computeMatch = raw.match(
          /COMPUTE\s+([A-Z0-9-]+)\s*=\s*(.+?)(?:\.|$)/i
        )
        if (computeMatch) {
          const target = computeMatch[1]
          const formula = computeMatch[2]

          // Extract variables from formula
          const variables = this.extractVariablesFromFormula(
            formula,
            dataElements
          )
          for (const variable of variables) {
            this.addAssignment(assignments, target, variable)
          }
        }
      }

      // Track arithmetic operations
      if (['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE'].includes(stmt.type)) {
        const arithmeticAssignments = this.extractArithmeticAssignments(
          raw,
          dataElements
        )
        for (const [target, sources] of arithmeticAssignments) {
          for (const source of sources) {
            this.addAssignment(assignments, target, source)
          }
        }
      }
    }

    return assignments
  }

  /**
   * Identify input sources (ACCEPT, READ statements)
   */
  private identifyInputSources (
    statements: Statement[],
    dataElements: DataElement[]
  ): DataFlowNode[] {
    const nodes: DataFlowNode[] = []

    for (const stmt of statements) {
      if (!stmt.raw) continue

      // ACCEPT statements (user input)
      if (stmt.type === 'ACCEPT') {
        const acceptMatch = stmt.raw.match(/ACCEPT\s+([A-Z0-9-]+)/i)
        if (acceptMatch) {
          const variable = acceptMatch[1]
          nodes.push({
            id: this.generateNodeId(),
            type: 'input',
            name: `User Input: ${variable}`,
            location: stmt.location
          })
        }
      }

      // READ statements (file input)
      if (stmt.type === 'READ') {
        const readMatch = stmt.raw.match(
          /READ\s+([A-Z0-9-]+)(?:\s+INTO\s+([A-Z0-9-]+))?/i
        )
        if (readMatch) {
          const fileName = readMatch[1]
          const recordName = readMatch[2] || fileName
          nodes.push({
            id: this.generateNodeId(),
            type: 'input',
            name: `File Input: ${fileName}`,
            location: stmt.location
          })
        }
      }
    }

    return nodes
  }

  /**
   * Map output destinations (DISPLAY, WRITE statements)
   */
  private identifyOutputDestinations (
    statements: Statement[],
    dataElements: DataElement[]
  ): DataFlowNode[] {
    const nodes: DataFlowNode[] = []

    for (const stmt of statements) {
      if (!stmt.raw) continue

      // DISPLAY statements (screen output)
      if (stmt.type === 'DISPLAY') {
        const displayMatch = stmt.raw.match(/DISPLAY\s+(.+?)(?:\.|$)/i)
        if (displayMatch) {
          const content = displayMatch[1]
          nodes.push({
            id: this.generateNodeId(),
            type: 'output',
            name: `Display Output: ${content.substring(0, 30)}`,
            location: stmt.location
          })
        }
      }

      // WRITE statements (file output)
      if (stmt.type === 'WRITE') {
        const writeMatch = stmt.raw.match(
          /WRITE\s+([A-Z0-9-]+)(?:\s+FROM\s+([A-Z0-9-]+))?/i
        )
        if (writeMatch) {
          const recordName = writeMatch[1]
          const sourceName = writeMatch[2] || recordName
          nodes.push({
            id: this.generateNodeId(),
            type: 'output',
            name: `File Output: ${recordName}`,
            location: stmt.location
          })
        }
      }
    }

    return nodes
  }

  /**
   * Identify transformations (COMPUTE, arithmetic operations)
   */
  private identifyTransformations (
    statements: Statement[],
    dataElements: DataElement[]
  ): DataFlowNode[] {
    const nodes: DataFlowNode[] = []

    for (const stmt of statements) {
      if (!stmt.raw) continue

      // COMPUTE transformations
      if (stmt.type === 'COMPUTE') {
        const computeMatch = stmt.raw.match(
          /COMPUTE\s+([A-Z0-9-]+)\s*=\s*(.+?)(?:\.|$)/i
        )
        if (computeMatch) {
          const target = computeMatch[1]
          const formula = computeMatch[2]
          nodes.push({
            id: this.generateNodeId(),
            type: 'transformation',
            name: `Calculate: ${target}`,
            location: stmt.location
          })
        }
      }

      // Arithmetic transformations
      if (['ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE'].includes(stmt.type)) {
        nodes.push({
          id: this.generateNodeId(),
          type: 'transformation',
          name: `${stmt.type} Operation`,
          location: stmt.location
        })
      }

      // MOVE transformations (data copying)
      if (stmt.type === 'MOVE') {
        const moveMatch = stmt.raw.match(
          /MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i
        )
        if (moveMatch) {
          const target = moveMatch[2]
          nodes.push({
            id: this.generateNodeId(),
            type: 'transformation',
            name: `Copy to: ${target}`,
            location: stmt.location
          })
        }
      }
    }

    return nodes
  }

  /**
   * Create storage nodes for working storage variables
   */
  private createStorageNodes (dataElements: DataElement[]): DataFlowNode[] {
    const nodes: DataFlowNode[] = []

    for (const element of dataElements) {
      // Only create nodes for top-level variables (01 level)
      if (element.level === 1 || element.level === 77) {
        nodes.push({
          id: this.generateNodeId(),
          type: 'storage',
          name: `Storage: ${element.name}`,
          location: {
            startLine: 0,
            endLine: 0,
            startColumn: 0,
            endColumn: 0
          }
        })
      }
    }

    return nodes
  }

  /**
   * Build dependency graph for data transformations
   */
  private buildEdgesFromAssignments (
    assignments: Map<string, Set<string>>,
    nodes: DataFlowNode[]
  ): DataFlowEdge[] {
    const edges: DataFlowEdge[] = []

    for (const [target, sources] of assignments) {
      const targetNode = nodes.find(n => n.name.includes(target))
      if (!targetNode) continue

      for (const source of sources) {
        const sourceNode = nodes.find(n => n.name.includes(source))
        if (sourceNode) {
          edges.push({
            from: sourceNode.id,
            to: targetNode.id,
            dataElement: source
          })
        }
      }
    }

    return edges
  }

  /**
   * Build edges from data flow analysis
   */
  private buildDataFlowEdges (
    statements: Statement[],
    nodes: DataFlowNode[],
    dataElements: DataElement[]
  ): DataFlowEdge[] {
    const edges: DataFlowEdge[] = []

    for (const stmt of statements) {
      if (!stmt.raw) continue

      // Connect inputs to transformations
      if (stmt.type === 'ACCEPT' || stmt.type === 'READ') {
        const inputNode = nodes.find(
          n =>
            n.type === 'input' &&
            n.location.startLine === stmt.location.startLine
        )

        if (inputNode) {
          // Find next transformation
          const nextTransform = nodes.find(
            n =>
              n.type === 'transformation' &&
              n.location.startLine > stmt.location.startLine
          )

          if (nextTransform) {
            edges.push({
              from: inputNode.id,
              to: nextTransform.id,
              dataElement: 'data'
            })
          }
        }
      }

      // Connect transformations to outputs
      if (stmt.type === 'DISPLAY' || stmt.type === 'WRITE') {
        const outputNode = nodes.find(
          n =>
            n.type === 'output' &&
            n.location.startLine === stmt.location.startLine
        )

        if (outputNode) {
          // Find previous transformation
          const prevTransform = nodes
            .filter(
              n =>
                n.type === 'transformation' &&
                n.location.startLine < stmt.location.startLine
            )
            .sort((a, b) => b.location.startLine - a.location.startLine)[0]

          if (prevTransform) {
            edges.push({
              from: prevTransform.id,
              to: outputNode.id,
              dataElement: 'result'
            })
          }
        }
      }
    }

    return edges
  }

  /**
   * Helper: Add assignment to tracking map
   */
  private addAssignment (
    assignments: Map<string, Set<string>>,
    target: string,
    source: string
  ): void {
    if (!assignments.has(target)) {
      assignments.set(target, new Set())
    }
    assignments.get(target)!.add(source)
  }

  /**
   * Helper: Extract variables from formula
   */
  private extractVariablesFromFormula (
    formula: string,
    dataElements: DataElement[]
  ): string[] {
    const variables: string[] = []

    for (const element of dataElements) {
      const pattern = new RegExp(`\\b${element.name}\\b`, 'i')
      if (pattern.test(formula)) {
        variables.push(element.name)
      }
    }

    return variables
  }

  /**
   * Helper: Extract assignments from arithmetic operations
   */
  private extractArithmeticAssignments (
    raw: string,
    dataElements: DataElement[]
  ): Map<string, string[]> {
    const assignments = new Map<string, string[]>()

    // ADD A TO B
    const addToMatch = raw.match(/ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)/i)
    if (addToMatch) {
      assignments.set(addToMatch[2], [addToMatch[1], addToMatch[2]])
    }

    // ADD A B GIVING C
    const addGivingMatch = raw.match(
      /ADD\s+([A-Z0-9-]+)\s+([A-Z0-9-]+)\s+GIVING\s+([A-Z0-9-]+)/i
    )
    if (addGivingMatch) {
      assignments.set(addGivingMatch[3], [addGivingMatch[1], addGivingMatch[2]])
    }

    // SUBTRACT A FROM B
    const subtractMatch = raw.match(
      /SUBTRACT\s+([A-Z0-9-]+)\s+FROM\s+([A-Z0-9-]+)/i
    )
    if (subtractMatch) {
      assignments.set(subtractMatch[2], [subtractMatch[1], subtractMatch[2]])
    }

    // MULTIPLY A BY B
    const multiplyMatch = raw.match(
      /MULTIPLY\s+([A-Z0-9-]+)\s+BY\s+([A-Z0-9-]+)/i
    )
    if (multiplyMatch) {
      assignments.set(multiplyMatch[2], [multiplyMatch[1], multiplyMatch[2]])
    }

    // DIVIDE A BY B
    const divideMatch = raw.match(/DIVIDE\s+([A-Z0-9-]+)\s+BY\s+([A-Z0-9-]+)/i)
    if (divideMatch) {
      assignments.set(divideMatch[2], [divideMatch[1], divideMatch[2]])
    }

    return assignments
  }

  /**
   * Generate unique node ID
   */
  private generateNodeId (): string {
    return `NODE-${String(++this.nodeIdCounter).padStart(4, '0')}`
  }
}

/**
 * Factory function to create data flow analyzer
 */
export function createDataFlowAnalyzer (
  options?: DataFlowAnalyzerOptions
): DataFlowAnalyzer {
  return new DataFlowAnalyzer(options)
}
