/**
 * Code Generator Type Definitions
 */

export interface GeneratorOptions {
  language?: 'typescript' | 'python'
  includeComments?: boolean
  includeTests?: boolean
}

export interface CodeArtifact {
  filename: string
  content: string
  language: string
}

export interface DeploymentArtifact {
  type: 'lambda' | 'cdk' | 'api-gateway' | 'step-functions'
  files: CodeArtifact[]
  readme: string
}
