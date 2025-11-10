# End-to-End Integration Guide

This document describes how all COBRA components are wired together for local zero-cost operation.

## Architecture Overview

COBRA consists of five main layers that work together:

```
┌─────────────────────────────────────────────────────────────────┐
│                        User Interfaces                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │  Web UI      │  │  Backend API │  │  MCP Server (Kiro)   │  │
│  │  React/Vite  │  │  Express.js  │  │  Model Context Proto │  │
│  │  Port 3000   │  │  Port 3001   │  │  stdio transport     │  │
│  └──────┬───────┘  └──────┬───────┘  └──────────┬───────────┘  │
│         │                 │                      │              │
│         └─────────────────┴──────────────────────┘              │
│                           ↓                                     │
│                  ┌────────────────────┐                         │
│                  │   Orchestrator     │                         │
│                  │  (Coordinator)     │                         │
│                  └────────┬───────────┘                         │
│                           │                                     │
│         ┌─────────────────┼─────────────────┐                  │
│         ↓                 ↓                 ↓                   │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐         │
│  │   Parser     │  │   Analyzer   │  │  Generator   │         │
│  │  COBOL→AST   │  │  Logic→Rules │  │  Rules→AWS   │         │
│  └──────────────┘  └──────────────┘  └──────────────┘         │
│                                                                 │
│  Optional: LLM Integration (OpenAI/Anthropic)                  │
│  └─→ Natural-language explanations (~$0.01-0.10 per analysis)  │
└─────────────────────────────────────────────────────────────────┘
```

## Component Integration

### 1. Orchestrator (Core Coordinator)

**File**: `src/orchestrator.ts`

**Purpose**: Coordinates all components and provides high-level workflows.

**Key Methods**:

- `analyzeComplete()` - Full end-to-end analysis
- `analyzeQuick()` - Parse + analyze only (no LLM)
- `parse()` - COBOL parsing
- `analyze()` - Logic analysis
- `generateSpecDocuments()` - Spec generation
- `generateAWSInfrastructure()` - Code generation
- `suggestModernizationStrategy()` - Recommendations

**Integration Points**:

```typescript
// Used by Backend API
import { createOrchestrator } from './orchestrator.js'
const orchestrator = createOrchestrator({ llmApiKey, verbose: true })
const result = await orchestrator.analyzeComplete(cobolSource)

// Used by MCP Server
import { parseCobol, analyzeLogic } from './mcp-server/tools/index.js'
const parseResult = await parseCobol(source)
const analysis = await analyzeLogic(parseResult.ast)
```

### 2. Backend API Server

**File**: `src/web/backend/index.ts`

**Purpose**: REST API for web interface and external integrations.

**Endpoints**:

- `POST /api/analyze` - Submit COBOL for analysis
- `GET /api/analysis/:id` - Check analysis status
- `GET /api/download/:id/:artifact` - Download generated code
- `GET /health` - Health check

**Integration with Orchestrator**:

```typescript
// Backend creates orchestrator instance
const orchestrator = createOrchestrator({
  llmApiKey: process.env.OPENAI_API_KEY || process.env.ANTHROPIC_API_KEY,
  llmProvider: process.env.OPENAI_API_KEY ? 'openai' : 'anthropic',
  enableCaching: true,
  verbose: true
})

// Processes analysis asynchronously
async function processAnalysis(analysisId: string, cobolSource: string) {
  const result = await orchestrator.analyzeComplete(cobolSource, onProgress)
  // Store results in memory
  analysisResults.set(analysisId, { status: 'complete', ...result })
}
```

**Features**:

- Rate limiting (10 requests per minute per IP)
- Input validation (max 5MB COBOL files)
- CORS enabled for local development
- In-memory result storage (no database needed)
- Progress tracking via callbacks

### 3. MCP Server (Kiro Integration)

**File**: `src/mcp-server/index.ts`

**Purpose**: Expose COBRA capabilities to Kiro via Model Context Protocol.

**Tools Exposed**:

1. `parseCobol` - Parse COBOL source
2. `analyzeLogic` - Extract business logic
3. `generateSpec` - Create spec documents
4. `generateAWSCode` - Generate AWS infrastructure
5. `suggestModernization` - Modernization recommendations

**Integration with Kiro**:

```json
// .kiro/settings/mcp.json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server/index.js"],
      "disabled": false,
      "autoApprove": ["parseCobol", "analyzeLogic", "generateSpec"]
    }
  }
}
```

**Usage in Kiro**:

```
User: Analyze this COBOL program
Kiro: [Calls parseCobol tool] → [Calls analyzeLogic tool] → [Presents results]
```

### 4. Parser Layer

**Files**: `src/parser/*.ts`

**Purpose**: Convert COBOL source to Abstract Syntax Tree (AST).

**Components**:

- `cobol-parser.ts` - Main parser wrapper
- `copybook-extractor.ts` - Extract data structures
- `data-structure-parser.ts` - Parse PIC clauses

**Integration**:

```typescript
// Used by orchestrator
import { createParser } from './parser/cobol-parser.js'
const parser = createParser({ dialect: 'COBOL-85' })
const ast = parser.parse(cobolSource)
```

**Output**: Structured AST with:

- Program metadata (ID, line count, complexity)
- Data division structures
- Procedure division logic
- Source maps for error reporting

### 5. Analyzer Layer

**Files**: `src/analyzer/*.ts`

**Purpose**: Extract business logic, patterns, and dependencies.

**Components**:

- `pattern-recognizer.ts` - Identify banking patterns
- `business-rule-extractor.ts` - Extract IF-THEN logic
- `data-flow-analyzer.ts` - Track data transformations

**Integration**:

```typescript
// Used by orchestrator
import { createLogicAnalyzer } from './analyzer/index.js'
const analyzer = createLogicAnalyzer({ confidenceThreshold: 0.6 })
const analysis = analyzer.analyze(ast)
```

**Output**: LogicAnalysis with:

- Business rules (conditions, actions)
- Banking patterns (interest, transactions, batch)
- Data structures (inputs, outputs, working storage)
- Dependencies (called programs, files, databases)

### 6. Generator Layer

**Files**: `src/generator/*.ts`

**Purpose**: Generate AWS infrastructure code from analysis.

**Components**:

- `lambda-generator.ts` - Generate Lambda functions
- `api-gateway-generator.ts` - Generate API configs
- `cdk-generator.ts` - Generate CDK stacks
- `hybrid-infrastructure-generator.ts` - Hybrid architectures

**Integration**:

```typescript
// Used by orchestrator
import { generateLambdaFunction } from './generator/lambda-generator.js'
import { generateCDKStack } from './generator/cdk-generator.js'

const lambdaCode = generateLambdaFunction(businessRule, 'typescript')
const cdkStack = generateCDKStack(analysis)
```

**Output**: AWSArtifacts with:

- Lambda function code (TypeScript/Python)
- API Gateway configurations (OpenAPI)
- CDK infrastructure code
- Deployment documentation

### 7. LLM Integration (Optional)

**Files**: `src/llm/*.ts`

**Purpose**: Generate natural-language explanations.

**Components**:

- `client.ts` - OpenAI/Anthropic API client
- `explanation-generator.ts` - Generate explanations
- `prompts.ts` - Prompt templates
- `cache.ts` - Response caching

**Integration**:

```typescript
// Used by orchestrator (optional)
import { ExplanationGenerator } from './llm/explanation-generator.js'

if (llmApiKey) {
  const generator = new ExplanationGenerator({ llmConfig: { apiKey } })
  const explanation = await generator.generateExplanation(ast, analysis)
}
```

**Cost**: ~$0.01-0.10 per analysis (only if API key provided)

## Data Flow

### Complete Analysis Workflow

```
1. User Input (COBOL source)
   ↓
2. Orchestrator.analyzeComplete()
   ↓
3. Parser.parse() → AST
   ↓
4. Analyzer.analyze(AST) → LogicAnalysis
   ↓
5. LLM.generateExplanation() → Natural language (optional)
   ↓
6. SpecGenerator.generate() → requirements.md, design.md, tasks.md
   ↓
7. CodeGenerator.generate() → Lambda, API Gateway, CDK
   ↓
8. ModernizationPlanner.suggest() → Recommendations
   ↓
9. Return complete results to user
```

### Backend API Workflow

```
1. POST /api/analyze
   ↓
2. Validate input
   ↓
3. Generate analysis ID
   ↓
4. Start async processing
   ↓
5. Return analysis ID immediately
   ↓
6. Background: orchestrator.analyzeComplete()
   ↓
7. Store results in memory
   ↓
8. GET /api/analysis/:id → Return results
```

### MCP Server Workflow

```
1. Kiro sends tool request
   ↓
2. MCP Server receives via stdio
   ↓
3. Route to appropriate tool handler
   ↓
4. Execute tool (parseCobol, analyzeLogic, etc.)
   ↓
5. Return JSON response to Kiro
   ↓
6. Kiro presents results to user
```

## Testing Integration

### Integration Test Suite

**File**: `src/integration-test.ts`

**Tests**:

1. COBOL parsing
2. Logic analysis
3. Spec generation
4. AWS code generation
5. Modernization planning
6. Complete end-to-end workflow

**Run**: `npm run test:e2e`

### End-to-End Verification

**File**: `scripts/verify-e2e.sh`

**Checks**:

1. Prerequisites (Node.js, npm, build)
2. Component integration tests
3. MCP server startup
4. Backend API endpoints
5. Orchestrator functionality
6. Generated artifacts
7. Zero-cost architecture
8. MCP configuration

**Run**: `./scripts/verify-e2e.sh`

## Configuration

### Environment Variables

```bash
# Optional: LLM API key for explanations
export OPENAI_API_KEY="sk-..."
# or
export ANTHROPIC_API_KEY="sk-ant-..."

# Optional: Backend port (default: 3001)
export PORT=3001

# Optional: Node environment
export NODE_ENV="development"
```

### MCP Configuration

**File**: `.kiro/settings/mcp.json`

```json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server/index.js"],
      "env": {
        "NODE_ENV": "production"
      },
      "disabled": false,
      "autoApprove": [
        "parseCobol",
        "analyzeLogic",
        "generateSpec",
        "generateAWSCode",
        "suggestModernization"
      ]
    }
  }
}
```

## Running All Components

### Option 1: All-in-One Script

```bash
./scripts/start-cobra.sh
```

This starts:

- Backend API on port 3001
- Frontend on port 3000 (if available)
- Logs to `logs/` directory

### Option 2: Individual Components

```bash
# Terminal 1: Backend API
npm run api:dev

# Terminal 2: Frontend (optional)
npm run web:dev

# Terminal 3: MCP Server (for Kiro)
npm run mcp:start
```

### Option 3: Programmatic Usage

```typescript
import { createOrchestrator } from './dist/orchestrator.js'

const orchestrator = createOrchestrator({
  llmApiKey: process.env.OPENAI_API_KEY,
  verbose: true
})

const result = await orchestrator.analyzeComplete(cobolSource)
console.log(result.explanation)
console.log(result.awsCode)
```

## Verification Checklist

- [ ] TypeScript compiles: `npm run build`
- [ ] Integration tests pass: `npm run test:e2e`
- [ ] Backend API responds: `curl http://localhost:3001/health`
- [ ] MCP server starts: `npm run mcp:start`
- [ ] Analysis completes: POST to `/api/analyze`
- [ ] Artifacts generated: Check `generated/` directory
- [ ] Zero AWS costs: No AWS SDK in core components
- [ ] Kiro integration: MCP tools available in Kiro

## Troubleshooting

### Components Not Connecting

**Symptom**: Backend API can't reach orchestrator

**Solution**:

```bash
# Rebuild TypeScript
npm run build

# Check for import errors
node dist/orchestrator.js
```

### MCP Server Not Working

**Symptom**: Kiro can't find COBRA tools

**Solution**:

1. Ensure build is complete: `npm run build`
2. Check MCP config: `.kiro/settings/mcp.json`
3. Restart Kiro
4. Check Kiro output panel for errors

### Backend API Errors

**Symptom**: Analysis fails with 500 error

**Solution**:

```bash
# Check API logs
tail -f logs/api.log

# Test orchestrator directly
npm run test:e2e

# Verify COBOL input format
curl -X POST http://localhost:3001/api/analyze \
  -H "Content-Type: application/json" \
  -d '{"cobolSource":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n       PROCEDURE DIVISION.\n           STOP RUN."}'
```

### Missing Generated Code

**Symptom**: No Lambda/CDK code in results

**Solution**:

- Check that analysis completed successfully
- Verify generator components are built
- Check for errors in orchestrator logs
- Run integration test: `npm run test:e2e`

## Performance Metrics

### Local Execution Times

- **Parse**: <1 second for 10,000 line COBOL file
- **Analyze**: 1-3 seconds for typical banking program
- **Generate Spec**: 1-2 seconds
- **Generate Code**: 2-5 seconds
- **LLM Explanation**: 3-10 seconds (if enabled)
- **Total**: 5-20 seconds end-to-end

### Resource Usage

- **Memory**: ~200MB for orchestrator + components
- **CPU**: Minimal (single-threaded parsing/analysis)
- **Disk**: Generated artifacts ~1-5MB per analysis
- **Network**: Only LLM API calls (if enabled)

## Zero-Cost Verification

All components run locally:

✅ **Parser**: Local Node.js processing
✅ **Analyzer**: Local pattern matching
✅ **Generator**: Local template rendering
✅ **Backend API**: localhost:3001
✅ **Frontend**: localhost:3000
✅ **MCP Server**: stdio transport (no network)
✅ **Storage**: In-memory (no database)
✅ **Generated Code**: Local files (not deployed)

❌ **No AWS Services Used**:

- No Lambda invocations
- No API Gateway calls
- No DynamoDB reads/writes
- No S3 storage
- No CloudWatch logs
- No VPC networking

**Total Monthly Cost**: $0 (excluding optional LLM API: $2-5)

## Next Steps

1. **Test Integration**: Run `./scripts/verify-e2e.sh`
2. **Start Components**: Run `./scripts/start-cobra.sh`
3. **Analyze COBOL**: Upload files via web UI or API
4. **Review Generated Code**: Check `generated/` directory
5. **Deploy to AWS**: Follow [Deployment Guide](DEPLOYMENT-GUIDE.md) when ready

## Related Documentation

- [Quick Start Guide](../QUICKSTART.md)
- [Integration Guide](INTEGRATION-GUIDE.md)
- [Deployment Guide](DEPLOYMENT-GUIDE.md)
- [Troubleshooting](TROUBLESHOOTING.md)
- [API Reference](QUICK-REFERENCE.md)
