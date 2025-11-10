# COBRA Quick Start Guide

Get COBRA running locally in 5 minutes with **zero AWS costs**.

## Prerequisites

- Node.js 18+ installed
- npm or yarn
- (Optional) OpenAI or Anthropic API key for natural-language explanations

## Quick Start

### 1. Install Dependencies

```bash
npm install
```

### 2. Build the Project

```bash
npm run build
```

### 3. Start COBRA

**Option A: All-in-One Startup (Recommended)**

```bash
./scripts/start-cobra.sh
```

This starts:

- Backend API on `http://localhost:3001`
- Frontend UI on `http://localhost:3000` (if available)

**Option B: Individual Components**

```bash
# Terminal 1: Backend API
npm run api:dev

# Terminal 2: Frontend (optional)
npm run web:dev

# Terminal 3: MCP Server (for Kiro integration)
npm run mcp:start
```

### 4. Verify Installation

```bash
# Run integration tests
npm run test:e2e

# Or run comprehensive verification
./scripts/verify-e2e.sh
```

### 5. Test the API

```bash
# Health check
curl http://localhost:3001/health

# Analyze COBOL code
curl -X POST http://localhost:3001/api/analyze \
  -H "Content-Type: application/json" \
  -d '{
    "cobolSource": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n       PROCEDURE DIVISION.\n           DISPLAY \"HELLO\".\n           STOP RUN."
  }'
```

## Using COBRA

### Web Interface

1. Open `http://localhost:3000` in your browser
2. Paste COBOL code or upload a `.cbl` file
3. Click "Analyze"
4. View explanation, architecture diagram, and generated AWS code
5. Download artifacts (Lambda, API Gateway, CDK)

### MCP Integration with Kiro

1. COBRA is already configured in `.kiro/settings/mcp.json`
2. Restart Kiro to load the MCP server
3. Use COBRA tools in Kiro:
   - `parseCobol` - Parse COBOL source
   - `analyzeLogic` - Extract business logic
   - `generateSpec` - Create spec documents
   - `generateAWSCode` - Generate AWS infrastructure
   - `suggestModernization` - Get modernization recommendations

### Command Line

```bash
# Run integration test with example COBOL
npm run test:e2e

# Generate Lambda code
npm run generate:lambda -- --input examples/interest-calculation.cbl

# Generate CDK stack
npm run generate:cdk -- --input examples/interest-calculation.cbl

# Generate spec documents
npm run generate:spec -- --input examples/interest-calculation.cbl
```

## Optional: Enable LLM Explanations

Set your API key to enable natural-language explanations:

```bash
# OpenAI
export OPENAI_API_KEY="sk-..."

# Or Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."

# Then restart COBRA
./scripts/start-cobra.sh
```

**Cost**: ~$0.01-0.10 per COBOL analysis

## Architecture

All components run **locally** with **zero AWS costs**:

```
┌─────────────────────────────────────────────────────────┐
│                    Your Computer                        │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │   Frontend   │  │  Backend API │  │  MCP Server  │ │
│  │ localhost:   │→ │ localhost:   │→ │   (stdio)    │ │
│  │    3000      │  │    3001      │  │              │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│                           ↓                             │
│                    ┌──────────────┐                     │
│                    │ Orchestrator │                     │
│                    └──────────────┘                     │
│                           ↓                             │
│         ┌─────────────────┼─────────────────┐          │
│         ↓                 ↓                 ↓           │
│  ┌──────────┐      ┌──────────┐     ┌──────────┐      │
│  │  Parser  │      │ Analyzer │     │Generator │      │
│  └──────────┘      └──────────┘     └──────────┘      │
│                                                         │
│  Generated AWS code saved to: generated/               │
└─────────────────────────────────────────────────────────┘
```

## What Gets Generated (Not Deployed)

COBRA generates production-ready AWS code that you can deploy later:

- ✅ Lambda function code (TypeScript/Python)
- ✅ API Gateway configurations (OpenAPI)
- ✅ AWS CDK infrastructure code
- ✅ Step Functions workflows
- ✅ IAM policies and security configs
- ✅ Deployment documentation

**No AWS services are provisioned during development.**

## Troubleshooting

### Backend API won't start

```bash
# Check if port 3001 is in use
lsof -i :3001

# Kill existing process
kill -9 $(lsof -t -i:3001)

# Restart
npm run api:dev
```

### Frontend won't start

```bash
# Install frontend dependencies
cd src/web/frontend
npm install
npm run dev
```

### MCP Server not working in Kiro

1. Ensure TypeScript is compiled: `npm run build`
2. Check `.kiro/settings/mcp.json` exists
3. Restart Kiro
4. Check MCP server logs in Kiro's output panel

### Integration tests failing

```bash
# Rebuild everything
npm run build

# Run tests with verbose output
npm run test:e2e
```

## Next Steps

- **Analyze your COBOL**: Upload your own COBOL files
- **Review generated code**: Check `generated/` directory
- **Customize templates**: Edit files in `templates/`
- **Deploy to AWS**: Follow deployment guides in `docs/`
- **Integrate with CI/CD**: Use COBRA in your pipeline

## Documentation

- [Full Documentation](docs/README.md)
- [Integration Guide](docs/INTEGRATION-GUIDE.md)
- [Deployment Guide](docs/DEPLOYMENT-GUIDE.md)
- [Troubleshooting](docs/TROUBLESHOOTING.md)

## Cost Summary

| Component               | Monthly Cost |
| ----------------------- | ------------ |
| Local Development       | **$0**       |
| Backend API (localhost) | **$0**       |
| Frontend (localhost)    | **$0**       |
| MCP Server (local)      | **$0**       |
| LLM API (optional)      | $2-5         |
| **Total**               | **$0-5**     |

## Support

- Issues: [GitHub Issues](https://github.com/your-org/cobra/issues)
- Documentation: `docs/` directory
- Examples: `examples/` directory

---

**Ready to modernize your COBOL?** Start analyzing! 🐍
