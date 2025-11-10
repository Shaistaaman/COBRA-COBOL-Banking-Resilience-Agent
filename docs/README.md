# COBRA Documentation

Complete documentation for COBRA (COBOL Banking Resilience Agent).

## Quick Start

New to COBRA? Start here:

1. **[Setup Guide](SETUP-GUIDE.md)** - Complete installation and configuration instructions
2. **[Quick Reference](QUICK-REFERENCE.md)** - Fast reference for common commands and operations
3. **[Example Project](../generated/interest-calculation-example/)** - Complete working example

## Documentation Index

### Getting Started

- **[Setup Guide](SETUP-GUIDE.md)** - Installation, MCP configuration, and verification

  - Prerequisites and system requirements
  - Step-by-step installation
  - MCP server configuration for Kiro
  - Troubleshooting common setup issues

- **[Quick Reference](QUICK-REFERENCE.md)** - Fast reference guide
  - MCP tools reference
  - Command line usage
  - Common file locations
  - Useful commands and shortcuts

### Deployment

- **[Deployment Guide](DEPLOYMENT-GUIDE.md)** - Complete deployment instructions
  - Free tier deployment (zero cost)
  - Production deployment
  - Monitoring and maintenance
  - Cost optimization strategies

### Troubleshooting

- **[Troubleshooting Guide](TROUBLESHOOTING.md)** - Solutions to common issues
  - Installation issues
  - MCP server issues
  - Build and compilation issues
  - Web interface issues
  - Code generation issues
  - Deployment issues
  - Performance issues

### Implementation Summaries

- **[Task 5 Summary](task-5-implementation-summary.md)** - MCP tools implementation
- **[Task 6 Summary](task-6-implementation-summary.md)** - AWS code generator
- **[Task 8 Summary](task-8-implementation-summary.md)** - Demo web interface
- **[Task 9 Summary](task-9-implementation-summary.md)** - generateAWSCode MCP tool
- **[Task 10 Summary](task-10-implementation-summary.md)** - Complete example and documentation

## Steering Documents

Advanced guides for COBOL modernization (located in `.kiro/steering/`):

- **[Banking Patterns](../.kiro/steering/banking-patterns.md)** - Common COBOL patterns in banking

  - Interest accrual
  - Loan amortization
  - Transaction posting
  - Batch reconciliation
  - Pattern recognition guidelines

- **[Modernization Strategy](../.kiro/steering/modernization-strategy.md)** - Modernization best practices

  - Prioritization criteria
  - AWS service selection
  - Security and compliance
  - Successful approaches
  - Anti-patterns to avoid

- **[COBOL-to-Cloud Template](../.kiro/steering/cobol-to-cloud-template.md)** - Spec templates

  - Requirements template
  - Design template
  - Tasks template
  - Usage instructions

- **[Technology Stack](../.kiro/steering/tech.md)** - Technical details

  - Core technologies
  - Frontend and backend stack
  - Generated code targets
  - Common commands

- **[Project Structure](../.kiro/steering/structure.md)** - Directory organization

  - Directory layout
  - File naming conventions
  - Module organization

- **[Product Overview](../.kiro/steering/product.md)** - Product information
  - Core capabilities
  - Target users
  - Key principles

## Examples

### Complete Working Example

**[Interest Calculation Example](../generated/interest-calculation-example/)**

A complete end-to-end example showing:

- Original COBOL program (50 lines)
- Generated Lambda function (TypeScript)
- Generated CDK infrastructure
- API Gateway configuration
- Architecture diagram
- Validation summary (100% accuracy)
- Deployment instructions

### Sample COBOL Programs

Located in `examples/`:

- `interest-calculation.cbl` - Simple interest calculation
- `code-generator-usage.ts` - Code generator usage example
- `llm-usage-example.ts` - LLM integration example
- `mcp-tools-usage.ts` - MCP tools usage example

## Architecture

### System Components

```
COBRA Architecture:
├── MCP Server (Kiro integration)
├── COBOL Parser (AST generation)
├── Logic Analyzer (pattern recognition)
├── Spec Generator (requirements, design, tasks)
├── Code Generator (Lambda, CDK, API Gateway)
├── LLM Integration (explanations)
└── Web Interface (demo application)
```

### Zero-Cost Design

All components run locally during development:

- **Cost**: $0/month (excluding minimal LLM API usage)
- **No AWS deployment required** for development
- **Generated code saved locally** for review
- **Optional free deployment** to GitHub Pages or Vercel

## Usage Patterns

### Using COBRA with Kiro

1. **Parse COBOL**: Use `parseCobol` MCP tool
2. **Analyze Logic**: Use `analyzeLogic` MCP tool
3. **Generate Specs**: Use `generateSpec` MCP tool
4. **Generate AWS Code**: Use `generateAWSCode` MCP tool
5. **Get Recommendations**: Use `suggestModernization` MCP tool

### Using the Demo Web Interface

1. Start backend: `npm run api:dev`
2. Start frontend: `npm run web:dev`
3. Open http://localhost:3000
4. Upload COBOL file or paste code
5. View results and download artifacts

### Command Line Usage

```bash
# Generate spec documents
npm run generate:spec -- examples/interest-calculation.cbl

# Generate Lambda code
npm run generate:lambda -- examples/interest-calculation.cbl

# Generate CDK infrastructure
npm run generate:cdk -- examples/interest-calculation.cbl
```

## Cost Information

### Development Phase (Zero Cost)

| Component          | Deployment | Cost        |
| ------------------ | ---------- | ----------- |
| MCP Server         | Local      | $0          |
| COBOL Parser       | Local      | $0          |
| Code Generator     | Local      | $0          |
| Web Interface      | localhost  | $0          |
| LLM API (optional) | API calls  | ~$2-5/month |

### Production Phase (Optional)

| Deployment    | Monthly Cost | Use Case             |
| ------------- | ------------ | -------------------- |
| Local Only    | $0           | Development, demos   |
| Static Site   | $0           | Public demo          |
| AWS Free Tier | $0           | Low-traffic POC      |
| Production    | $50-150      | Enterprise customers |

## Support

### Getting Help

1. **Check Documentation**: Start with this index
2. **Review Examples**: See `generated/interest-calculation-example/`
3. **Read Troubleshooting**: Check `TROUBLESHOOTING.md`
4. **Enable Debug Logging**: Set `LOG_LEVEL=debug`
5. **Open GitHub Issue**: Report bugs or request features

### Common Issues

- **MCP not connecting**: Rebuild and restart Kiro
- **Parse errors**: Check COBOL dialect compatibility
- **Port in use**: Kill process or use different port
- **Build errors**: Clean and reinstall dependencies
- **Deployment fails**: Check AWS credentials

See [Troubleshooting Guide](TROUBLESHOOTING.md) for detailed solutions.

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## Additional Resources

### External Links

- [AWS Lambda Documentation](https://docs.aws.amazon.com/lambda/)
- [AWS CDK Documentation](https://docs.aws.amazon.com/cdk/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
- [Kiro IDE](https://kiro.ai/)
- [Mermaid Diagrams](https://mermaid.live)

### Project Links

- [Main README](../README.md)
- [Package.json](../package.json)
- [TypeScript Config](../tsconfig.json)
- [MCP Configuration](../.kiro/settings/mcp.json)

---

**Documentation v1.0**  
**Last Updated**: November 8, 2024  
**COBRA Version**: 0.1.0
