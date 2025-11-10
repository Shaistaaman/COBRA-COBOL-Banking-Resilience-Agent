# COBRA Setup Guide

Complete guide for setting up COBRA (COBOL Banking Resilience Agent) for zero-cost local development.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [MCP Server Configuration](#mcp-server-configuration)
4. [Verification](#verification)
5. [Troubleshooting](#troubleshooting)
6. [Optional: Free Deployment](#optional-free-deployment)

## Prerequisites

### Required Software

| Software     | Version       | Purpose           | Installation                      |
| ------------ | ------------- | ----------------- | --------------------------------- |
| **Node.js**  | 20.x or later | Runtime for COBRA | [nodejs.org](https://nodejs.org/) |
| **npm**      | 10.x or later | Package manager   | Included with Node.js             |
| **Kiro IDE** | Latest        | MCP integration   | [kiro.ai](https://kiro.ai/)       |

### Optional Software

| Software    | Version      | Purpose                | Installation                                   |
| ----------- | ------------ | ---------------------- | ---------------------------------------------- |
| **Git**     | 2.x or later | Version control        | [git-scm.com](https://git-scm.com/)            |
| **AWS CLI** | 2.x or later | AWS deployment         | [AWS CLI Install](https://aws.amazon.com/cli/) |
| **AWS CDK** | 2.x or later | Infrastructure as code | `npm install -g aws-cdk`                       |
| **Docker**  | Latest       | Containerization       | [docker.com](https://www.docker.com/)          |

### System Requirements

- **OS**: macOS, Linux, or Windows (WSL2 recommended)
- **RAM**: 4GB minimum, 8GB recommended
- **Disk Space**: 2GB for dependencies and generated code
- **Network**: Internet connection for npm packages and optional LLM API calls

## Installation

### Step 1: Clone the Repository

```bash
# Clone from GitHub
git clone https://github.com/your-org/cobra.git
cd cobra

# Or download and extract ZIP
# https://github.com/your-org/cobra/archive/main.zip
```

### Step 2: Install Dependencies

```bash
# Install all dependencies
npm install

# This will install:
# - @modelcontextprotocol/sdk (MCP integration)
# - TypeScript and build tools
# - COBOL parser libraries
# - Testing frameworks
# - Web interface dependencies
```

**Expected Output**:

```
added 847 packages, and audited 848 packages in 45s
found 0 vulnerabilities
```

### Step 3: Build the Project

```bash
# Build TypeScript to JavaScript
npm run build

# This compiles:
# - MCP server (src/mcp-server/)
# - Parser (src/parser/)
# - Analyzer (src/analyzer/)
# - Generator (src/generator/)
# - LLM integration (src/llm/)
```

**Expected Output**:

```
> cobra@0.1.0 build
> tsc

✓ Build complete
```

### Step 4: Verify Installation

```bash
# Manually check build
node dist/mcp-server/index.js

# Expected: MCP server starts (press Ctrl+C to exit)
```

**Expected Output**:

```
✓ Node.js version: 20.10.0
✓ npm version: 10.2.3
✓ TypeScript compiled successfully
✓ MCP server built
✓ All dependencies installed
✓ COBRA is ready to use!
```

## MCP Server Configuration

### Automatic Configuration (Recommended)

COBRA includes a pre-configured MCP server setup in `.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server/index.js"],
      "disabled": false,
      "autoApprove": [],
      "env": {
        "NODE_ENV": "production",
        "LOG_LEVEL": "info"
      }
    }
  }
}
```

### Kiro Integration Steps

1. **Ensure COBRA is built**:

   ```bash
   npm run build
   ```

2. **Open Kiro IDE**

3. **Reconnect MCP Servers**:

   - Open Command Palette (Cmd/Ctrl + Shift + P)
   - Search for "MCP: Reconnect Servers"
   - Or use the MCP Server view in the sidebar

4. **Verify COBRA is connected**:
   - Open MCP Server view
   - Look for "cobra" server with status "Connected"
   - Expand to see available tools:
     - parseCobol
     - analyzeLogic
     - generateSpec
     - generateAWSCode
     - suggestModernization

### Manual Configuration (If Needed)

If the automatic configuration doesn't work:

1. **Create MCP configuration directory**:

   ```bash
   mkdir -p .kiro/settings
   ```

2. **Create or edit `.kiro/settings/mcp.json`**:

   ```json
   {
     "mcpServers": {
       "cobra": {
         "command": "node",
         "args": ["/absolute/path/to/cobra/dist/mcp-server/index.js"],
         "disabled": false
       }
     }
   }
   ```

3. **Replace `/absolute/path/to/cobra/` with your actual path**:

   ```bash
   # Get absolute path
   pwd
   # Example: /Users/yourname/projects/cobra
   ```

4. **Restart Kiro**

### Testing MCP Connection

Test the MCP server from Kiro:

1. **Open Kiro chat**

2. **Try parsing a simple COBOL program**:

   ```
   Use the parseCobol tool to parse this COBOL code:

   IDENTIFICATION DIVISION.
   PROGRAM-ID. HELLO.
   PROCEDURE DIVISION.
       DISPLAY "Hello, World!".
       STOP RUN.
   ```

3. **Expected Response**:
   - AST with program structure
   - Metadata (program name, line count, complexity)
   - No errors

## Verification

### Verify MCP Server

```bash
# Test MCP server directly
node dist/mcp-server/index.js

# Expected: Server starts and waits for input
# Press Ctrl+C to exit
```

### Verify Code Generation

```bash
# Generate spec from example
npm run generate:spec -- examples/interest-calculation.cbl

# Generate Lambda code
npm run generate:lambda -- examples/interest-calculation.cbl

# Generate CDK infrastructure
npm run generate:cdk -- examples/interest-calculation.cbl

# Expected: Generated files in generated/ directory
```

### Verify Web Interface

```bash
# Terminal 1: Start backend
npm run api:dev

# Terminal 2: Start frontend
npm run web:dev

# Open browser to http://localhost:3000
# Upload examples/interest-calculation.cbl
# Click "Analyze"
# Expected: Results displayed with explanation and code
```

### Run Tests

```bash
# Run all tests
npm test

# Expected: All tests pass
```

## Troubleshooting

### Issue: MCP Server Not Connecting

**Symptoms**:

- COBRA tools not visible in Kiro
- "Server disconnected" status in MCP view

**Solutions**:

1. **Rebuild the project**:

   ```bash
   npm run build
   ```

2. **Check MCP configuration**:

   ```bash
   cat .kiro/settings/mcp.json
   ```

3. **Verify absolute path**:

   ```bash
   ls -la dist/mcp-server/index.js
   ```

4. **Check Node.js version**:

   ```bash
   node --version
   # Should be 20.x or later
   ```

5. **Restart Kiro**

6. **Check Kiro logs**:
   - Open Kiro Developer Tools
   - Look for MCP connection errors

### Issue: Build Errors

**Symptoms**:

- `npm run build` fails
- TypeScript compilation errors

**Solutions**:

1. **Clean and rebuild**:

   ```bash
   rm -rf dist node_modules package-lock.json
   npm install
   npm run build
   ```

2. **Check TypeScript version**:

   ```bash
   npx tsc --version
   # Should be 5.x or later
   ```

3. **Check for syntax errors**:
   ```bash
   npm run lint
   ```

### Issue: Parse Errors

**Symptoms**:

- COBOL parsing fails
- "Unsupported dialect" errors

**Solutions**:

1. **Check COBOL dialect**:

   - COBRA supports COBOL-85, COBOL-2002
   - IBM Enterprise COBOL extensions
   - Micro Focus COBOL

2. **Try with example files first**:

   ```bash
   npm run parse -- examples/interest-calculation.cbl
   ```

3. **Check for syntax errors in COBOL**:
   - Ensure proper column formatting (columns 7-72)
   - Check for missing periods
   - Verify division structure

### Issue: Web Interface Not Starting

**Symptoms**:

- `npm run web:start` fails
- Port already in use errors

**Solutions**:

1. **Check if ports are available**:

   ```bash
   # Check port 3000 (frontend)
   lsof -i :3000

   # Check port 3001 (backend)
   lsof -i :3001
   ```

2. **Kill processes using ports**:

   ```bash
   # Kill process on port 3000
   kill -9 $(lsof -t -i:3000)

   # Kill process on port 3001
   kill -9 $(lsof -t -i:3001)
   ```

3. **Use different ports**:

   ```bash
   # Frontend on port 3002
   PORT=3002 npm run web:start

   # Backend on port 3003
   PORT=3003 npm run api:start
   ```

4. **Reinstall frontend dependencies**:
   ```bash
   cd src/web/frontend
   rm -rf node_modules package-lock.json
   npm install
   cd ../../..
   ```

### Issue: Generated Code Has Errors

**Symptoms**:

- TypeScript compilation errors in generated code
- CDK synth fails

**Solutions**:

1. **Install dependencies in generated project**:

   ```bash
   cd generated/your-project
   npm install
   ```

2. **Check TypeScript configuration**:

   ```bash
   npx tsc --noEmit
   ```

3. **Validate CDK stack**:

   ```bash
   npx cdk synth
   ```

4. **Review generated code**:
   - Check for TODO comments
   - Verify all imports are correct
   - Ensure types are properly defined

### Issue: LLM API Errors

**Symptoms**:

- "API key not found" errors
- Explanation generation fails

**Solutions**:

1. **Set API key environment variable**:

   ```bash
   # For OpenAI
   export OPENAI_API_KEY="your-api-key"

   # For Anthropic
   export ANTHROPIC_API_KEY="your-api-key"
   ```

2. **Use local templates instead**:

   - COBRA can generate specs without LLM
   - Uses template-based generation
   - Set `USE_LLM=false` environment variable

3. **Check API key validity**:
   ```bash
   curl https://api.openai.com/v1/models \
     -H "Authorization: Bearer $OPENAI_API_KEY"
   ```

## Optional: Free Deployment

### Deploy Demo to GitHub Pages

```bash
# Build frontend for production
npm run build:web

# Deploy to GitHub Pages
npm run deploy:github-pages

# Your demo will be available at:
# https://your-username.github.io/cobra/
```

### Deploy Demo to Vercel

```bash
# Install Vercel CLI
npm install -g vercel

# Build frontend
npm run build:web

# Deploy
vercel deploy

# Follow prompts to configure deployment
```

### Deploy to AWS Free Tier

```bash
# Navigate to generated example
cd generated/interest-calculation-example

# Install dependencies
npm install

# Bootstrap CDK (first time only)
npx cdk bootstrap

# Deploy
npx cdk deploy

# Note the API Gateway URL from output
```

**Free Tier Limits**:

- Lambda: 1M requests/month
- API Gateway: 1M requests/month (12 months)
- CloudWatch: 5GB logs

**Cost Monitoring**:

```bash
# Set up billing alert
aws budgets create-budget \
  --account-id YOUR_ACCOUNT_ID \
  --budget file://budget.json
```

## Next Steps

After successful setup:

1. **Try the Example**:

   ```bash
   cd generated/interest-calculation-example
   cat README.md
   ```

2. **Analyze Your COBOL**:

   - Upload your COBOL files to the web interface
   - Or use MCP tools in Kiro

3. **Generate AWS Code**:

   - Review generated Lambda functions
   - Customize CDK stack as needed
   - Deploy when ready

4. **Read Documentation**:
   - [Banking Patterns](.kiro/steering/banking-patterns.md)
   - [Modernization Strategy](.kiro/steering/modernization-strategy.md)
   - [COBOL-to-Cloud Template](.kiro/steering/cobol-to-cloud-template.md)

## Support

For additional help:

- **Documentation**: `/docs` directory
- **Examples**: `generated/interest-calculation-example/`
- **Issues**: GitHub Issues
- **Community**: Discord/Slack (if available)

---

**Setup Complete!** 🎉

You're now ready to modernize COBOL banking systems with COBRA at zero cost.
