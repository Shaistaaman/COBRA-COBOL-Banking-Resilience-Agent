# COBRA Troubleshooting Guide

Common issues and solutions for COBRA (COBOL Banking Resilience Agent).

## Table of Contents

1. [Installation Issues](#installation-issues)
2. [MCP Server Issues](#mcp-server-issues)
3. [Build and Compilation Issues](#build-and-compilation-issues)
4. [Web Interface Issues](#web-interface-issues)
5. [Code Generation Issues](#code-generation-issues)
6. [Deployment Issues](#deployment-issues)
7. [Performance Issues](#performance-issues)
8. [Getting Help](#getting-help)

## Installation Issues

### Issue: npm install fails

**Symptoms**:

- Dependency resolution errors
- Network timeout errors
- Permission denied errors

**Solutions**:

1. **Clear npm cache**:

   ```bash
   npm cache clean --force
   rm -rf node_modules package-lock.json
   npm install
   ```

2. **Check Node.js version**:

   ```bash
   node --version
   # Should be 20.x or later
   ```

3. **Use different registry** (if behind firewall):

   ```bash
   npm config set registry https://registry.npmjs.org/
   npm install
   ```

4. **Fix permissions** (macOS/Linux):
   ```bash
   sudo chown -R $(whoami) ~/.npm
   sudo chown -R $(whoami) /usr/local/lib/node_modules
   ```

### Issue: TypeScript not found

**Symptoms**:

- `tsc: command not found`
- Build fails with TypeScript errors

**Solutions**:

1. **Install TypeScript globally**:

   ```bash
   npm install -g typescript
   ```

2. **Use npx**:

   ```bash
   npx tsc --version
   ```

3. **Reinstall dependencies**:
   ```bash
   rm -rf node_modules
   npm install
   ```

## MCP Server Issues

### Issue: MCP server not connecting to Kiro

**Symptoms**:

- COBRA tools not visible in Kiro
- "Server disconnected" status in MCP view
- No response when invoking tools

**Solutions**:

1. **Rebuild the project**:

   ```bash
   npm run build
   ```

2. **Check MCP configuration**:

   ```bash
   cat .kiro/settings/mcp.json
   ```

   Ensure it contains:

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

3. **Use absolute path**:

   ```bash
   # Get absolute path
   pwd
   # Example: /Users/yourname/projects/cobra

   # Update mcp.json with absolute path
   "args": ["/Users/yourname/projects/cobra/dist/mcp-server/index.js"]
   ```

4. **Test MCP server directly**:

   ```bash
   node dist/mcp-server/index.js
   # Should start without errors
   # Press Ctrl+C to exit
   ```

5. **Check Kiro logs**:

   - Open Kiro Developer Tools (Help > Toggle Developer Tools)
   - Look for MCP connection errors in Console

6. **Restart Kiro**:
   - Completely quit and restart Kiro
   - Or use Command Palette: "MCP: Reconnect Servers"

### Issue: MCP tools return errors

**Symptoms**:

- Tools execute but return error messages
- "Tool execution failed" errors

**Solutions**:

1. **Check input format**:

   - Ensure COBOL source is valid
   - Check for special characters or encoding issues

2. **Enable debug logging**:

   ```bash
   export LOG_LEVEL=debug
   node dist/mcp-server/index.js
   ```

3. **Test with example COBOL**:

   ```
   Use parseCobol tool with:

   IDENTIFICATION DIVISION.
   PROGRAM-ID. TEST.
   PROCEDURE DIVISION.
       DISPLAY "TEST".
       STOP RUN.
   ```

## Build and Compilation Issues

### Issue: TypeScript compilation errors

**Symptoms**:

- `npm run build` fails
- Type errors in source code

**Solutions**:

1. **Clean build**:

   ```bash
   rm -rf dist
   npm run build
   ```

2. **Check TypeScript version**:

   ```bash
   npx tsc --version
   # Should be 5.3.x or later
   ```

3. **Fix type errors**:

   ```bash
   # Show all errors
   npx tsc --noEmit
   ```

4. **Update dependencies**:
   ```bash
   npm update
   npm run build
   ```

### Issue: Module not found errors

**Symptoms**:

- `Cannot find module` errors
- Import errors

**Solutions**:

1. **Reinstall dependencies**:

   ```bash
   rm -rf node_modules package-lock.json
   npm install
   ```

2. **Check import paths**:

   - Ensure relative paths are correct
   - Check for typos in module names

3. **Verify package.json**:
   - Ensure all dependencies are listed
   - Check for version conflicts

## Web Interface Issues

### Issue: Web interface won't start

**Symptoms**:

- `npm run web:dev` fails
- Port already in use errors
- Blank page in browser

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
   PORT=3002 npm run web:dev

   # Backend on port 3003
   PORT=3003 npm run api:dev
   ```

4. **Reinstall frontend dependencies**:

   ```bash
   cd src/web/frontend
   rm -rf node_modules package-lock.json
   npm install
   cd ../../..
   ```

5. **Check browser console**:
   - Open Developer Tools (F12)
   - Look for JavaScript errors
   - Check Network tab for failed requests

### Issue: File upload not working

**Symptoms**:

- Upload button doesn't respond
- Files don't appear after selection
- Upload fails with error

**Solutions**:

1. **Check file size**:

   - Maximum file size is typically 5MB
   - Try with smaller COBOL file

2. **Check file format**:

   - Ensure file has .cbl or .cob extension
   - Verify file is plain text (not binary)

3. **Check backend is running**:

   ```bash
   curl http://localhost:3001/health
   # Should return: {"status":"ok"}
   ```

4. **Check CORS settings**:
   - Backend should allow requests from localhost:3000
   - Check browser console for CORS errors

## Code Generation Issues

### Issue: Generated code has syntax errors

**Symptoms**:

- TypeScript compilation errors in generated code
- Missing imports or undefined types

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

3. **Review generated code**:

   - Look for TODO comments
   - Check for placeholder values
   - Verify all imports are correct

4. **Regenerate with verbose logging**:
   ```bash
   export LOG_LEVEL=debug
   npm run generate:lambda -- examples/interest-calculation.cbl
   ```

### Issue: CDK synth fails

**Symptoms**:

- `cdk synth` returns errors
- CloudFormation template invalid

**Solutions**:

1. **Install CDK dependencies**:

   ```bash
   cd generated/your-project
   npm install
   ```

2. **Check CDK version**:

   ```bash
   npx cdk --version
   # Should be 2.x or later
   ```

3. **Validate stack**:

   ```bash
   npx cdk synth --verbose
   ```

4. **Check for resource conflicts**:
   - Ensure resource names are unique
   - Check for circular dependencies

### Issue: Generated Lambda has runtime errors

**Symptoms**:

- Lambda function fails when invoked
- CloudWatch logs show errors

**Solutions**:

1. **Test locally first**:

   ```bash
   cd generated/your-project
   npm test
   ```

2. **Check environment variables**:

   - Ensure all required env vars are set
   - Verify values are correct

3. **Review CloudWatch logs**:

   ```bash
   aws logs tail /aws/lambda/your-function-name --follow
   ```

4. **Add debug logging**:
   - Set LOG_LEVEL=debug in Lambda environment
   - Redeploy and test again

## Deployment Issues

### Issue: CDK bootstrap fails

**Symptoms**:

- `cdk bootstrap` returns errors
- Permission denied errors

**Solutions**:

1. **Check AWS credentials**:

   ```bash
   aws sts get-caller-identity
   # Should return your account info
   ```

2. **Verify IAM permissions**:

   - Ensure you have AdministratorAccess or equivalent
   - Check for organization SCPs that might block

3. **Specify region explicitly**:

   ```bash
   cdk bootstrap aws://ACCOUNT-ID/REGION
   ```

4. **Use different profile**:
   ```bash
   cdk bootstrap --profile your-profile
   ```

### Issue: CDK deploy fails

**Symptoms**:

- Deployment fails partway through
- CloudFormation rollback occurs

**Solutions**:

1. **Check CloudFormation events**:

   ```bash
   aws cloudformation describe-stack-events \
     --stack-name YourStackName \
     --max-items 20
   ```

2. **Review error messages**:

   - Look for resource limit errors
   - Check for naming conflicts
   - Verify IAM permissions

3. **Deploy with verbose logging**:

   ```bash
   cdk deploy --verbose
   ```

4. **Clean up failed stack**:
   ```bash
   cdk destroy
   cdk deploy
   ```

### Issue: API Gateway returns 403 Forbidden

**Symptoms**:

- API calls fail with 403 error
- "Missing Authentication Token" error

**Solutions**:

1. **Check API key**:

   ```bash
   aws apigateway get-api-keys --include-values
   ```

2. **Verify usage plan**:

   ```bash
   aws apigateway get-usage-plans
   ```

3. **Include API key in request**:

   ```bash
   curl -X POST $API_URL/endpoint \
     -H "x-api-key: YOUR_API_KEY" \
     -H "Content-Type: application/json" \
     -d '{"data":"value"}'
   ```

4. **Check IAM permissions** (if using IAM auth):
   - Ensure caller has execute-api:Invoke permission
   - Verify resource policy on API Gateway

## Performance Issues

### Issue: Slow COBOL parsing

**Symptoms**:

- Parsing takes longer than 30 seconds
- Timeout errors

**Solutions**:

1. **Check file size**:

   - Files over 10,000 lines may be slow
   - Consider splitting large files

2. **Increase timeout**:

   ```typescript
   // In MCP tool configuration
   timeout: 60000 // 60 seconds
   ```

3. **Use streaming parser** (for very large files):
   - Process file in chunks
   - Parse incrementally

### Issue: High memory usage

**Symptoms**:

- Node.js process uses excessive memory
- Out of memory errors

**Solutions**:

1. **Increase Node.js memory limit**:

   ```bash
   export NODE_OPTIONS="--max-old-space-size=4096"
   node dist/mcp-server/index.js
   ```

2. **Clear caches periodically**:

   - Restart MCP server regularly
   - Implement cache eviction

3. **Process files in batches**:
   - Don't load all files at once
   - Use streaming where possible

### Issue: Slow web interface

**Symptoms**:

- UI is sluggish or unresponsive
- Long wait times for results

**Solutions**:

1. **Check backend performance**:

   ```bash
   # Monitor backend logs
   npm run api:dev
   ```

2. **Enable caching**:

   - Cache parsed ASTs
   - Cache analysis results
   - Use browser localStorage

3. **Optimize frontend**:
   - Lazy load components
   - Debounce user input
   - Use React.memo for expensive components

## Getting Help

### Check Documentation

1. **Setup Guide**: `docs/SETUP-GUIDE.md`
2. **Deployment Guide**: `docs/DEPLOYMENT-GUIDE.md`
3. **Quick Reference**: `docs/QUICK-REFERENCE.md`
4. **Steering Documents**: `.kiro/steering/*.md`

### Review Examples

1. **Complete Example**: `generated/interest-calculation-example/`
2. **Sample COBOL**: `examples/*.cbl`
3. **Test Cases**: `tests/unit/*.test.ts`

### Enable Debug Logging

```bash
# Set log level
export LOG_LEVEL=debug

# Run with verbose output
npm run build --verbose
npm run mcp:start --verbose
```

### Check System Status

```bash
# Node.js version
node --version

# npm version
npm --version

# TypeScript version
npx tsc --version

# AWS CLI version
aws --version

# CDK version
npx cdk --version
```

### Collect Diagnostic Information

When reporting issues, include:

1. **Environment**:

   - OS and version
   - Node.js version
   - npm version
   - COBRA version

2. **Error messages**:

   - Full error output
   - Stack traces
   - Log files

3. **Steps to reproduce**:

   - What you were trying to do
   - Commands you ran
   - Expected vs actual behavior

4. **Configuration**:
   - MCP configuration (`.kiro/settings/mcp.json`)
   - Package.json
   - Environment variables

### Community Support

- **GitHub Issues**: Report bugs and request features
- **Documentation**: Check `/docs` directory
- **Examples**: Review working examples in `generated/`
- **Steering Docs**: Read `.kiro/steering/` for patterns and strategies

### Emergency Fixes

If nothing else works:

```bash
# Nuclear option: complete reset
rm -rf node_modules package-lock.json dist
rm -rf src/web/frontend/node_modules
npm install
npm run build
cd src/web/frontend && npm install && cd ../../..

# Restart everything
# 1. Quit Kiro completely
# 2. Restart Kiro
# 3. Reconnect MCP servers
# 4. Test with simple example
```

---

**Still having issues?**

1. Check if your issue is already documented above
2. Review the complete example in `generated/interest-calculation-example/`
3. Try with the provided example COBOL files first
4. Enable debug logging to get more information
5. Open an issue on GitHub with diagnostic information

**Troubleshooting Guide v1.0**  
**Last Updated**: November 10, 2025
