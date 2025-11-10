/**
 * Logic Analyzer Type Definitions
 */

export interface AnalyzerOptions {
  enablePatternRecognition?: boolean
  confidenceThreshold?: number
}

export interface DataFlowGraph {
  nodes: DataFlowNode[]
  edges: DataFlowEdge[]
}

export interface DataFlowNode {
  id: string
  type: 'input' | 'output' | 'transformation' | 'storage'
  name: string
  location: SourceLocation
}

export interface DataFlowEdge {
  from: string
  to: string
  dataElement: string
}

export interface SourceLocation {
  startLine: number
  endLine: number
  startColumn: number
  endColumn: number
}
