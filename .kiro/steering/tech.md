# Technology Stack

## Core Technologies

- **Language**: TypeScript (Node.js runtime)
- **MCP Integration**: `@modelcontextprotocol/sdk` for Model Context Protocol
- **COBOL Parsing**: `cobol-parser` npm package (primary), `tree-sitter-cobol` (fallback)
- **AI/LLM**: OpenAI or Anthropic API for natural-language generation
- **AWS Services**: Lambda, API Gateway, CDK, Step Functions, MQ, S3, CloudWatch

## Frontend (Demo Web Interface)

- **Framework**: React with TypeScript
- **Build Tool**: Vite
- **Code Editor**: Monaco Editor (COBOL syntax highlighting)
- **Diagram Rendering**: mermaid.js
- **Deployment**: AWS Amplify or S3 + CloudFront

## Backend

- **API Server**: Express.js with TypeScript
- **Containerization**: Docker for local development
- **Production**: ECS containers or Lambda/Step Functions

## Generated Code Targets

- **Lambda Functions**: TypeScript or Python 3.12
- **Infrastructure**: AWS CDK (TypeScript)
- **API Definitions**: OpenAPI/Swagger for API Gateway

## COBOL Dialect Support

- COBOL-85 and COBOL-2002 standards
- IBM Enterprise COBOL extensions
- Micro Focus COBOL
- Common banking extensions: CICS, DB2, IMS

## Common Commands

### Development

```bash
# Install dependencies
npm install

# Start MCP server locally
npm run mcp:start

# Run unit tests
npm test

# Run integration tests
npm run test:integration

# Start demo web interface
npm run dev
```

### MCP Configuration

Configure in `.kiro/settings/mcp.json`:

```json
{
  "mcpServers": {
    "cobra": {
      "command": "node",
      "args": ["dist/mcp-server.js"],
      "disabled": false
    }
  }
}
```

### Code Generation

```bash
# Generate Lambda code from COBOL
npm run generate:lambda -- --input sample.cbl

# Generate CDK infrastructure
npm run generate:cdk -- --input sample.cbl

# Generate complete spec documents
npm run generate:spec -- --input sample.cbl
```

### AWS Deployment

```bash
# Deploy generated CDK stack
cd generated/cdk
npm install
cdk bootstrap  # First time only
cdk deploy

# Run generated Lambda locally
sam local invoke -e test-event.json
```

## Performance Targets

- Parse COBOL files up to 10,000 lines within 30 seconds
- Complete analysis and explanation within 60 seconds for demo interface
- Support concurrent analysis of multiple files

## Security Requirements

- Input validation and sanitization for all uploaded COBOL code
- Least-privilege IAM roles in generated infrastructure
- TLS 1.2+ for all API communications
- AWS WAF rules in generated API Gateway configurations
- Audit logging for all API invocations
- Encryption at rest and in transit
