# COBRA MCP Tools

This directory contains the MCP (Model Context Protocol) tool handlers that expose COBRA's COBOL analysis and modernization capabilities to Kiro.

## Available Tools

### 1. parseCobol

Parses COBOL source code and generates an Abstract Syntax Tree (AST).

**Input:**

```typescript
{
  source: string // COBOL source code
}
```

**Output:**

```typescript
{
  ast: AST | null,
  errors: ParseError[],
  warnings: string[],
  metadata: {
    programName: string,
    lineCount: number,
    complexity: number
  }
}
```

**Features:**

- Timeout handling (30 second limit for large files)
- Support for multiple COBOL dialects (COBOL-85, COBOL-2002)
- Comprehensive error reporting with line numbers
- Program complexity metrics

**Example:**

```typescript
const result = await parseCobol(cobolSource)
console.log(`Program: ${result.metadata.programName}`)
console.log(`Complexity: ${result.metadata.complexity}`)
```

### 2. analyzeLogic

Analyzes COBOL AST to extract business rules, banking patterns, and dependencies.

**Input:**

```typescript
{
  ast: AST // Abstract Syntax Tree from parseCobol
}
```

**Output:**

```typescript
{
  businessRules: BusinessRule[],
  dataStructures: DataStructure[],
  dependencies: Dependency[],
  entryPoints: EntryPoint[],
  patterns: BankingPattern[]
}
```

**Features:**

- Banking pattern recognition (interest calculation, loan amortization, etc.)
- Confidence scores for pattern detection (0.0 - 1.0)
- Business rule extraction from COBOL logic
- Data flow analysis
- Dependency mapping

**Example:**

```typescript
const analysis = await analyzeLogic(ast)
console.log(`Found ${analysis.patterns.length} banking patterns`)
analysis.patterns.forEach(p => {
  console.log(`${p.type}: ${(p.confidence * 100).toFixed(0)}% confidence`)
})
```

### 3. generateSpec

Generates Kiro spec documents (requirements.md, design.md, tasks.md) from COBOL analysis.

**Input:**

```typescript
{
  analysis: LogicAnalysis // Result from analyzeLogic
}
```

**Output:**

```typescript
{
  requirements: string,  // EARS-compliant requirements document
  design: string,        // AWS architecture design document
  tasks: string          // Implementation task list
}
```

**Features:**

- EARS-compliant acceptance criteria
- AWS architecture recommendations
- Mermaid diagrams for visualization
- Implementation task breakdown
- References to original COBOL source lines

**Example:**

```typescript
const spec = await generateSpec(analysis)

// Save to files
fs.writeFileSync('.kiro/specs/my-cobol-app/requirements.md', spec.requirements)
fs.writeFileSync('.kiro/specs/my-cobol-app/design.md', spec.design)
fs.writeFileSync('.kiro/specs/my-cobol-app/tasks.md', spec.tasks)
```

### 4. suggestModernization

Provides modernization strategies and AWS service recommendations.

**Input:**

```typescript
{
  analysis: LogicAnalysis // Result from analyzeLogic
}
```

**Output:**

```typescript
{
  recommendations: Recommendation[],
  prioritizedModules: PrioritizedModule[],
  deRiskingStrategies: Strategy[]
}
```

**Features:**

- Coupling analysis to identify low-risk modules
- Prioritized modernization roadmap
- AWS service selection (Lambda, Step Functions, ECS, etc.)
- De-risking strategies with risk/mitigation analysis
- Effort estimation

**Example:**

```typescript
const plan = await suggestModernization(analysis)

console.log('Prioritized Modules:')
plan.prioritizedModules.forEach(m => {
  console.log(`${m.name}: Priority ${m.priority}, Coupling ${m.coupling}`)
})

console.log('\nRecommendations:')
plan.recommendations.forEach(r => {
  console.log(`${r.module} → ${r.awsService}`)
  console.log(`Rationale: ${r.rationale}`)
})
```

## Usage in Kiro

These tools are exposed through the COBRA MCP server and can be invoked from Kiro:

1. **Configure MCP Server** in `.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server/index.js"],
      "disabled": false
    }
  }
}
```

2. **Use in Kiro Chat**:

```
Parse this COBOL program and analyze its business logic:
[paste COBOL code]
```

Kiro will automatically invoke the appropriate MCP tools to:

- Parse the COBOL source
- Analyze business logic
- Generate explanations
- Suggest modernization approaches

## Complete Workflow Example

```typescript
import {
  parseCobol,
  analyzeLogic,
  generateSpec,
  suggestModernization
} from './tools/index.js'

async function modernizeCobol(cobolSource: string) {
  // Step 1: Parse
  const parseResult = await parseCobol(cobolSource)
  if (parseResult.errors.length > 0) {
    throw new Error('Parse errors found')
  }

  // Step 2: Analyze
  const analysis = await analyzeLogic(parseResult.ast!)

  // Step 3: Generate specs
  const spec = await generateSpec(analysis)

  // Step 4: Get recommendations
  const plan = await suggestModernization(analysis)

  return {
    parseResult,
    analysis,
    spec,
    plan
  }
}
```

## Performance Characteristics

- **parseCobol**: < 30 seconds for files up to 10,000 lines
- **analyzeLogic**: < 5 seconds for typical programs
- **generateSpec**: < 2 seconds (no LLM calls)
- **suggestModernization**: < 1 second (algorithmic analysis)

## Error Handling

All tools implement graceful error handling:

- **parseCobol**: Returns partial results with error details
- **analyzeLogic**: Returns minimal analysis on failure
- **generateSpec**: Returns fallback spec documents
- **suggestModernization**: Returns basic recommendations

Errors are logged to console.error and returned in structured format.

## Testing

Run unit tests:

```bash
npm test tests/unit/mcp-tools.test.ts
```

Run example demonstration:

```bash
npx tsx examples/mcp-tools-usage.ts
```

## Architecture

```
MCP Server (index.ts)
  ↓
Tool Handlers (tools/index.ts)
  ↓
Core Modules:
  - Parser (src/parser/)
  - Analyzer (src/analyzer/)
  - Generator (src/generator/)
  - LLM (src/llm/)
```

## Requirements Mapping

- **Requirement 1.1**: parseCobol with 30-second timeout
- **Requirement 1.2**: analyzeLogic with pattern recognition
- **Requirement 5.1**: MCP server integration
- **Requirement 5.2**: Logic analysis with confidence scores
- **Requirement 5.3**: De-risking strategies
- **Requirement 6.1-6.4**: Spec document generation

## Future Enhancements

- [ ] LLM integration for enhanced explanations
- [ ] Caching layer for repeated analyses
- [ ] Streaming support for large files
- [ ] Multi-file analysis (copybook resolution)
- [ ] Visual architecture diagram generation
- [ ] Cost estimation for AWS deployment
