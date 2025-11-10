# COBRA Integration Guide

This guide explains how all COBRA components are wired together and how to verify the integration.

## Architecture Overview

COBRA consists of several interconnected components that work together to analyze COBOL code and generate AWS infrastructure:

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interfaces                         │
├─────────────────────────────────────────────────────────────┤
│  Kiro IDE (MCP)  │  Web Interface  │  CLI Tools             │
└────────┬──────────┴────────┬────────┴────────┬──────────────┘
         │                   │                 │
         ▼                   ▼                 ▼
┌─────────────────────────────────────────────────────────────┐
│                      Orchestrator                            │
│  (Coordinates all components and manages workflow)           │
└────────┬────────────────────────────────────────────────────┘
         │
         ├──────────────────────────────────────────────┐
         │                                              │
         ▼                                              ▼
┌─────────────────┐  ┌──────────────┐  ┌──────────────────────┐
│  COBOL Parser   │  │   Analyzer   │  │   LLM Integration    │
│  - AST Gen      │  │  - Patterns  │  │  - Explanations      │
│  - Copybooks    │  │  - Rules     │  │  - Caching           │
│  - Validation   │  │  - Data Flow │  │                      │
└────────┬────────┘  └──────┬───────┘  └──────────┬───────────┘
         │                  │                      │
         └──────────────────┴──────────────────────┘
                            │
                            ▼
         ┌──────────────────────────────────────────┐
         │          Code Generators                  │
         ├──────────────────────────────────────────┤
         │  Lambda  │  API GW  │  CDK  │  Specs    │
         └──────────┴──────────┴───────┴────────────┘
                            │
                            ▼
         ┌──────────────────────────────────────────┐
         │         Generated Artifacts               │
         │  - Lambda Functions (TypeScript/Python)   │
         │  - API Gateway Configurations             │
         │  - CDK Infrastructure Code                │
         │  - Specification Documents                │
         └──────────────────────────────────────────┘
```

## Component Integration

### 1. Orchestrator (`src/orchestrator.ts`)

The orchestrator is the central coordinator that wires all components together:

- **Manages workflow**: Coordinates parsing → analysis → generation
- **Progress tracking**: Reports progress through callback functions
- **Error handling**: Provides graceful degradation when components fail
- **Caching**: Enables LLM response caching to reduce costs

**Key Methods**:

- `analyzeComplete()`: Full end-to-end analysis
- `analyzeQuick()`: Parse and analyze only (no LLM)
- `generateCodeOnly()`: Generate AWS code from existing analysis

### 2. MCP Server (`src/mcp-server/index.ts`)

Exposes COBRA capabilities to Kiro through Model Context Protocol:

- **Tool Registration**: Registers 5 MCP tools (parseCobol, analyzeLogic, etc.)
- **Request Handling**: Routes tool calls to appropriate handlers
- **Error Handling**: Returns structured error responses
- **Stdio Transport**: Communicates with Kiro via stdin/stdout

**Integration Points**:

- Uses `src/mcp-server/tools/index.ts` for tool implementations
- Each tool calls orchestrator methods or component functions directly

### 3. Web Backend (`src/web/backend/index.ts`)

Provides REST API for the demo web interface:

- **Uses Orchestrator**: Calls `orchestrator.analyzeComplete()` for analysis
- **Progress Tracking**: Logs analysis progress to console
- **Result Storage**: Stores results in-memory for retrieval
- **File Downloads**: Serves generated artifacts as downloadable files

**API Endpoints**:

- `POST /api/analyze`: Start COBOL analysis
- `GET /api/analysis/:id`: Get analysis status and results
- `GET /api/download/:id/:artifact`: Download generated code

### 4. Parser → Analyzer → Generator Flow

**Parsing Flow**:

```typescript
createParser() → parse(source) → ParseResult { ast, errors, metadata }
```

**Analysis Flow**:

```typescript
createLogicAnalyzer() → analyze(ast) → LogicAnalysis {
  businessRules,
  patterns,
  dataStructures,
  dependencies
}
```

**Generation Flow**:

```typescript
generateLambdaFunction(analysis) → Artifact[]
generateAPIGatewayConfig(analysis) → Artifact[]
generateCDKStack(analysis) → Artifact[]
```

## Verification

### Quick Verification

Run the integration verification script:

```bash
npm run verify
```

This will:

1. Check Node.js and npm installation
2. Install dependencies if needed
3. Build TypeScript code
4. Verify all component files exist
5. Run integration tests

### Manual Verification

#### 1. Build the Project

```bash
npm run build
```

Verify that `dist/` directory contains all compiled files.

#### 2. Run Integration Tests

```bash
npm run test:e2e
```

This runs the comprehensive integration test suite that verifies:

- COBOL parsing works
- Logic analysis extracts patterns
- Spec generation creates documents
- AWS code generation produces artifacts
- Modernization planning suggests strategies
- Complete end-to-end workflow succeeds

#### 3. Test MCP Server

Start the MCP server:

```bash
npm run mcp:start
```

The server should output:

```
COBRA MCP Server running on stdio
```

Test with a simple input (in another terminal):

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | node dist/mcp-server/index.js
```

#### 4. Test Web Backend

Start the API server:

```bash
npm run api:dev
```

Test the health endpoint:

```bash
curl http://localhost:3001/health
```

Should return:

```json
{ "status": "ok", "timestamp": "..." }
```

Test analysis endpoint:

```bash
curl -X POST http://localhost:3001/api/analyze \
  -H "Content-Type: application/json" \
  -d '{"cobolSource":"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n       PROCEDURE DIVISION.\n           STOP RUN."}'
```

#### 5. Test Web Frontend

Start the frontend (in a separate terminal):

```bash
npm run web:dev
```

Open browser to `http://localhost:3000` and verify:

- Code editor loads
- Example snippets work
- File upload works
- Analysis runs and displays results

## Component Dependencies

### Runtime Dependencies

```
Orchestrator
├── Parser (cobol-parser)
├── Analyzer
│   ├── Pattern Recognizer
│   ├── Business Rule Extractor
│   └── Data Flow Analyzer
├── LLM Integration (optional)
│   ├── OpenAI/Anthropic Client
│   ├── Cache
│   └── Explanation Generator
└── Generators
    ├── Lambda Generator
    ├── API Gateway Generator
    ├── CDK Generator
    └── Artifact Packager
```

### MCP Server Dependencies

```
MCP Server
├── @modelcontextprotocol/sdk
├── MCP Tools
│   ├── parseCobol → Parser
│   ├── analyzeLogic → Analyzer
│   ├── generateSpec → Spec Generator
│   ├── generateAWSCode → Code Generators
│   └── suggestModernization → Modernization Planner
└── Stdio Transport
```

### Web Backend Dependencies

```
Web Backend (Express)
├── Orchestrator
├── CORS Middleware
├── Rate Limiting
├── Input Validation
└── In-Memory Storage
```

## Configuration

### Environment Variables

```bash
# LLM Configuration (optional)
OPENAI_API_KEY=sk-...          # For OpenAI GPT models
ANTHROPIC_API_KEY=sk-ant-...   # For Anthropic Claude models

# Web Backend Configuration
PORT=3001                       # API server port (default: 3001)

# MCP Server Configuration
# Configured in .kiro/settings/mcp.json
```

### MCP Configuration

Create or update `.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server/index.js"],
      "disabled": false,
      "autoApprove": []
    }
  }
}
```

## Troubleshooting

### Build Errors

**Problem**: TypeScript compilation fails

**Solution**:

```bash
# Clean and rebuild
rm -rf dist/
npm run build
```

### MCP Server Not Starting

**Problem**: MCP server fails to start

**Solution**:

1. Check that build completed: `ls dist/mcp-server/index.js`
2. Check for syntax errors: `node dist/mcp-server/index.js`
3. Verify dependencies: `npm install`

### Web Backend Errors

**Problem**: API server returns 500 errors

**Solution**:

1. Check console logs for error details
2. Verify example COBOL file exists: `ls examples/interest-calculation.cbl`
3. Test with minimal COBOL input
4. Check LLM API key if using explanation generation

### Integration Test Failures

**Problem**: Integration tests fail

**Solution**:

1. Run tests with verbose output: `node dist/integration-test.js`
2. Check which specific test failed
3. Verify component files exist: `ls dist/`
4. Check for missing dependencies: `npm install`

### Parser Errors

**Problem**: COBOL parsing fails

**Solution**:

1. Verify COBOL syntax is valid
2. Check for required divisions (IDENTIFICATION, PROGRAM-ID)
3. Try with a simpler COBOL program
4. Check parser logs for specific error messages

### Generator Errors

**Problem**: Code generation produces empty or invalid output

**Solution**:

1. Verify analysis completed successfully
2. Check that patterns were identified
3. Review business rules extraction
4. Test with example COBOL file

## Performance Considerations

### Local Execution (Zero Cost)

All components run locally with zero AWS costs:

- **Parser**: Processes COBOL in-memory
- **Analyzer**: Pattern recognition runs locally
- **Generators**: Template-based code generation
- **Web Interface**: Runs on localhost

**Only External Cost**: LLM API calls (~$0.01-0.10 per analysis)

### Optimization Tips

1. **Enable Caching**: Set `enableCaching: true` in orchestrator options
2. **Skip LLM**: Don't provide API key for faster analysis without explanations
3. **Use Quick Analysis**: Call `analyzeQuick()` instead of `analyzeComplete()`
4. **Pre-parse Examples**: Parse example files at startup for instant demo

### Performance Targets

- Parse 10,000 line COBOL file: < 30 seconds
- Complete analysis: < 60 seconds (without LLM)
- Complete analysis: < 90 seconds (with LLM)
- Code generation: < 10 seconds

## Next Steps

After verifying integration:

1. **Configure Kiro**: Set up MCP server in Kiro settings
2. **Test with Real COBOL**: Try analyzing actual COBOL programs
3. **Customize Generators**: Modify templates for your specific needs
4. **Deploy Web Interface**: Deploy to GitHub Pages or Vercel (optional)
5. **Add Custom Patterns**: Extend pattern recognizer for domain-specific logic

## Support

For issues or questions:

1. Check logs in console output
2. Run integration tests: `npm run test:e2e`
3. Review component documentation in `docs/`
4. Check troubleshooting section above
