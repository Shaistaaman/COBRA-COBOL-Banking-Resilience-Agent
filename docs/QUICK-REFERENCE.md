# COBRA Quick Reference Guide

Fast reference for common COBRA operations and commands.

## Installation

```bash
# Clone and install
git clone https://github.com/your-org/cobra.git
cd cobra
npm install
npm run build
```

## MCP Tools (Use in Kiro)

### parseCobol

Parse COBOL source code and generate AST.

```typescript
const result = await parseCobol(cobolSource)
// Returns: { ast, errors, warnings, metadata }
```

### analyzeLogic

Analyze COBOL logic and extract business rules.

```typescript
const analysis = await analyzeLogic(ast)
// Returns: { businessRules, patterns, dataStructures, dependencies }
```

### generateSpec

Generate Kiro spec documents.

```typescript
const spec = await generateSpec(analysis)
// Returns: { requirements, design, tasks }
```

### generateAWSCode

Generate AWS Lambda and infrastructure code.

```typescript
const code = await generateAWSCode(analysis)
// Returns: { lambdaFunctions, apiGateway, cdkStack, readme }
```

### suggestModernization

Get modernization recommendations.

```typescript
const plan = await suggestModernization(analysis)
// Returns: { recommendations, prioritizedModules, deRiskingStrategies }
```

## Command Line

### Generate Spec

```bash
npm run generate:spec -- examples/interest-calculation.cbl
```

### Generate Lambda Code

```bash
npm run generate:lambda -- examples/interest-calculation.cbl
```

### Generate CDK Infrastructure

```bash
npm run generate:cdk -- examples/interest-calculation.cbl
```

## Web Interface

### Start Demo

```bash
# Terminal 1: Backend
npm run api:dev

# Terminal 2: Frontend
npm run web:dev

# Open http://localhost:3000
```

## Deployment

### AWS Free Tier

```bash
cd generated/your-project
npm install
npm run build
npx cdk bootstrap  # First time only
npx cdk deploy
```

### GitHub Pages

```bash
npm run build:web
npm run deploy:github-pages
```

### Vercel

```bash
npm run build:web
vercel deploy
```

## Testing

### Run All Tests

```bash
npm test
```

### Run Specific Test

```bash
npm test -- mcp-tools.test.ts
```

### Run Integration Tests

```bash
npm run test:integration
```

## Development

### Watch Mode

```bash
npm run dev
```

### Lint Code

```bash
npm run lint
```

### Format Code

```bash
npm run format
```

### Build

```bash
npm run build
```

## Troubleshooting

### MCP Server Not Connecting

```bash
npm run build
# Restart Kiro
```

### Parse Errors

```bash
npm run parse -- your-file.cbl --verbose
```

### Port Already in Use

```bash
# Kill process on port 3000
kill -9 $(lsof -t -i:3000)
```

### Clean Build

```bash
rm -rf dist node_modules package-lock.json
npm install
npm run build
```

## Common File Locations

| File               | Location                  |
| ------------------ | ------------------------- |
| **MCP Config**     | `.kiro/settings/mcp.json` |
| **Steering Docs**  | `.kiro/steering/*.md`     |
| **Examples**       | `examples/*.cbl`          |
| **Generated Code** | `generated/*/`            |
| **Templates**      | `templates/*/`            |
| **Tests**          | `tests/*/`                |
| **Documentation**  | `docs/*.md`               |

## Environment Variables

```bash
# LLM API Keys (optional)
export OPENAI_API_KEY="your-key"
export ANTHROPIC_API_KEY="your-key"

# Disable LLM (use templates only)
export USE_LLM=false

# Log level
export LOG_LEVEL=debug

# Node environment
export NODE_ENV=production
```

## AWS CLI Commands

### View Lambda Logs

```bash
aws logs tail /aws/lambda/your-function-name --follow
```

### Get API Key

```bash
aws apigateway get-api-keys --include-values
```

### Check Costs

```bash
aws ce get-cost-and-usage \
  --time-period Start=2024-11-01,End=2024-11-08 \
  --granularity DAILY \
  --metrics BlendedCost
```

### Delete Stack

```bash
cdk destroy
```

## CDK Commands

### Synthesize CloudFormation

```bash
cdk synth
```

### Show Differences

```bash
cdk diff
```

### Deploy

```bash
cdk deploy
```

### Destroy

```bash
cdk destroy
```

### Bootstrap

```bash
cdk bootstrap
```

## Git Commands

### Clone

```bash
git clone https://github.com/your-org/cobra.git
```

### Update

```bash
git pull origin main
```

### Create Branch

```bash
git checkout -b feature/your-feature
```

### Commit

```bash
git add .
git commit -m "Your message"
git push origin feature/your-feature
```

## Docker Commands (Optional)

### Build Image

```bash
docker build -t cobra .
```

### Run Container

```bash
docker run -p 3000:3000 -p 3001:3001 cobra
```

### Docker Compose

```bash
docker-compose up
```

## Useful Links

| Resource       | URL                                       |
| -------------- | ----------------------------------------- |
| **COBRA Docs** | `/docs` directory                         |
| **Example**    | `generated/interest-calculation-example/` |
| **AWS Lambda** | https://docs.aws.amazon.com/lambda/       |
| **AWS CDK**    | https://docs.aws.amazon.com/cdk/          |
| **MCP**        | https://modelcontextprotocol.io/          |
| **Kiro**       | https://kiro.ai/                          |
| **Mermaid**    | https://mermaid.live                      |

## Cost Reference

| Deployment        | Monthly Cost   |
| ----------------- | -------------- |
| **Local Only**    | $0             |
| **Static Site**   | $0             |
| **AWS Free Tier** | $0 (12 months) |
| **Production**    | $50-150        |

## Support

| Issue                  | Solution                           |
| ---------------------- | ---------------------------------- |
| **MCP not connecting** | Rebuild and restart Kiro           |
| **Parse errors**       | Check COBOL dialect                |
| **Port in use**        | Kill process or use different port |
| **Build errors**       | Clean and reinstall                |
| **Deployment fails**   | Check AWS credentials              |

## Quick Start Checklist

- [ ] Install Node.js 20.x
- [ ] Clone repository
- [ ] Run `npm install`
- [ ] Run `npm run build`
- [ ] Configure MCP in Kiro
- [ ] Try example: `cd generated/interest-calculation-example`
- [ ] Read README.md
- [ ] Test with your COBOL

## Next Steps

1. **Review Example**: `cd generated/interest-calculation-example`
2. **Read Docs**: `docs/SETUP-GUIDE.md`
3. **Try Demo**: `npm run web:start`
4. **Deploy**: Follow `docs/DEPLOYMENT-GUIDE.md`

---

**Quick Reference v1.0**  
**Last Updated**: November 10, 2025
