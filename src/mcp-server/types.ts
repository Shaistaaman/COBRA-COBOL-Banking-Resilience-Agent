/**
 * Type definitions for COBRA MCP Server
 */

export interface ParseResult {
  ast: AST | null
  errors: ParseError[]
  warnings: string[]
  metadata: {
    programName: string
    lineCount: number
    complexity: number
  }
}

export interface AST {
  programId: string
  divisions: {
    identification?: IdentificationDivision
    environment?: EnvironmentDivision
    data?: DataDivision
    procedure?: ProcedureDivision
  }
  sourceMap: SourceMap
}

export interface IdentificationDivision {
  programId: string
  author?: string
  dateWritten?: string
}

export interface EnvironmentDivision {
  configuration?: any
  inputOutput?: any
}

export interface DataDivision {
  workingStorage?: DataElement[]
  fileSection?: FileDefinition[]
}

export interface ProcedureDivision {
  statements: Statement[]
}

export interface DataElement {
  name: string
  level: number
  picture?: string
  value?: string
  usage?: string
}

export interface FileDefinition {
  name: string
  organization: string
  accessMode: string
}

export interface Statement {
  type: string
  location: SourceLocation
  [key: string]: any
}

export interface SourceMap {
  lines: Map<number, string>
}

export interface SourceLocation {
  startLine: number
  endLine: number
  startColumn: number
  endColumn: number
}

export interface ParseError {
  message: string
  line: number
  column: number
  severity: 'error' | 'warning'
}

export interface LogicAnalysis {
  businessRules: BusinessRule[]
  dataStructures: DataStructure[]
  dependencies: Dependency[]
  entryPoints: EntryPoint[]
  patterns: BankingPattern[]
}

export interface BusinessRule {
  id: string
  type: 'calculation' | 'validation' | 'transformation' | 'decision'
  description: string
  cobolSource: {
    file: string
    startLine: number
    endLine: number
    snippet: string
  }
  inputs: DataElement[]
  outputs: DataElement[]
  formula?: string
  conditions?: Condition[]
}

export interface Condition {
  expression: string
  action: string
}

export interface DataStructure {
  name: string
  type: string
  fields: DataElement[]
}

export interface Dependency {
  type: 'copybook' | 'subprogram' | 'file' | 'database'
  name: string
  location?: string
}

export interface EntryPoint {
  name: string
  parameters: DataElement[]
  returnType?: string
}

export interface BankingPattern {
  type:
    | 'interest_calculation'
    | 'transaction_posting'
    | 'batch_processing'
    | 'validation'
    | 'loan_amortization'
    | 'account_reconciliation'
  confidence: number
  location: SourceLocation
  description: string
  parameters: Record<string, DataElement>
}

export interface SpecDocument {
  requirements: string
  design: string
  tasks: string
}

export interface AWSArtifacts {
  lambdaFunctions: LambdaFunction[]
  apiGateway: string
  cdkStack: string
  readme: string
  packages?: Record<string, PackagedArtifact>
}

export interface PackagedArtifact {
  filename: string
  content: string
  size: number
  files: string[]
  downloadUrl?: string
}

export interface LambdaFunction {
  name: string
  runtime: 'nodejs20.x' | 'python3.12'
  handler: string
  code: string
  environment: Record<string, string>
}

export interface ModernizationPlan {
  recommendations: Recommendation[]
  prioritizedModules: PrioritizedModule[]
  deRiskingStrategies: Strategy[]
}

export interface Recommendation {
  module: string
  awsService: string
  rationale: string
  estimatedEffort: string
}

export interface PrioritizedModule {
  name: string
  priority: number
  coupling: number
  complexity: number
  businessValue: string
}

export interface Strategy {
  approach: string
  description: string
  risks: string[]
  mitigations: string[]
}
