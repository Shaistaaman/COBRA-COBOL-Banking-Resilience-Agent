# Project Structure

## Directory Organization

```
.
├── .kiro/
│   ├── specs/                    # Kiro spec documents
│   │   └── cobra-cobol-agent/
│   │       ├── requirements.md
│   │       ├── design.md
│   │       └── tasks.md
│   ├── steering/                 # AI assistant guidance documents
│   └── settings/
│       └── mcp.json             # MCP server configuration
├── src/
│   ├── mcp-server/              # MCP server implementation
│   │   ├── index.ts             # Server entry point
│   │   └── tools/               # MCP tool handlers
│   ├── parser/                  # COBOL parser wrapper
│   │   ├── cobol-parser.ts
│   │   ├── ast-types.ts
│   │   └── copybook-extractor.ts
│   ├── analyzer/                # Logic analysis engine
│   │   ├── pattern-recognizer.ts
│   │   ├── business-rule-extractor.ts
│   │   └── data-flow-analyzer.ts
│   ├── generator/               # Code generation components
│   │   ├── spec-generator.ts
│   │   ├── lambda-generator.ts
│   │   ├── api-gateway-generator.ts
│   │   └── cdk-generator.ts
│   ├── llm/                     # LLM integration
│   │   ├── explanation-generator.ts
│   │   └── prompts/
│   └── web/                     # Demo web interface
│       ├── frontend/            # React application
│       └── backend/             # Express API server
├── templates/                   # Code generation templates
│   ├── lambda/
│   ├── cdk/
│   └── api-gateway/
├── examples/                    # Sample COBOL programs
│   ├── interest-calculation.cbl
│   ├── transaction-posting.cbl
│   └── batch-reconciliation.cbl
├── tests/
│   ├── unit/
│   ├── integration/
│   └── fixtures/                # Test COBOL samples
├── generated/                   # Output directory (gitignored)
│   ├── specs/
│   ├── lambda/
│   └── cdk/
└── docs/                        # User documentation
```

## Key Directories

### `/src/mcp-server/`

Contains the MCP server implementation that exposes COBRA capabilities to Kiro. Tool handlers implement the MCP protocol interface.

### `/src/parser/`

COBOL parsing logic that generates ASTs from source code. Handles multiple COBOL dialects and extracts data structures.

### `/src/analyzer/`

Business logic analysis components that identify banking patterns, extract rules, and map data flows.

### `/src/generator/`

Code generation modules that produce Kiro specs, Lambda functions, API Gateway configs, and CDK infrastructure.

### `/src/web/`

Demo web application with React frontend and Express backend for uploading and analyzing COBOL code.

### `/templates/`

Reusable code templates for generating Lambda functions, CDK constructs, and API configurations.

### `/examples/`

Sample COBOL programs demonstrating common banking operations for testing and demonstration.

### `/generated/`

Output directory for all generated artifacts (specs, Lambda code, CDK stacks). This directory is gitignored.

## File Naming Conventions

- TypeScript source files: `kebab-case.ts`
- Test files: `*.test.ts` or `*.spec.ts`
- COBOL files: `kebab-case.cbl` or `.cob`
- Generated specs: Follow Kiro convention (`requirements.md`, `design.md`, `tasks.md`)
- Configuration files: `kebab-case.json` or `.yaml`

## Module Organization

Each major component (parser, analyzer, generator) follows this pattern:

```
component/
├── index.ts              # Public API exports
├── types.ts              # TypeScript interfaces
├── implementation.ts     # Core logic
└── utils.ts              # Helper functions
```

## Generated Code Structure

Generated AWS projects follow this structure:

```
generated/project-name/
├── lambda/
│   ├── src/
│   │   └── handlers/
│   ├── package.json
│   └── tsconfig.json
├── cdk/
│   ├── lib/
│   │   └── stacks/
│   ├── bin/
│   ├── package.json
│   └── cdk.json
└── README.md             # Deployment instructions
```

## Configuration Files

- `.kiro/settings/mcp.json`: MCP server configuration for Kiro integration
- `package.json`: Node.js dependencies and scripts
- `tsconfig.json`: TypeScript compiler configuration
- `docker-compose.yml`: Local development environment setup
