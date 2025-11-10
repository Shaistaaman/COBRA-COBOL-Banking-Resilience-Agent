#!/bin/bash

# COBRA Integration Verification Script
# Verifies all components are properly wired together

set -e

echo "🐍 COBRA Integration Verification"
echo "=================================="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if Node.js is installed
echo "Checking Node.js installation..."
if ! command -v node &> /dev/null; then
    echo -e "${RED}✗ Node.js is not installed${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Node.js $(node --version)${NC}"

# Check if npm is installed
echo "Checking npm installation..."
if ! command -v npm &> /dev/null; then
    echo -e "${RED}✗ npm is not installed${NC}"
    exit 1
fi
echo -e "${GREEN}✓ npm $(npm --version)${NC}"

# Check if dependencies are installed
echo ""
echo "Checking dependencies..."
if [ ! -d "node_modules" ]; then
    echo -e "${YELLOW}⚠ Dependencies not installed${NC}"
    echo "Installing dependencies..."
    npm install
else
    echo -e "${GREEN}✓ Dependencies installed${NC}"
fi

# Build TypeScript
echo ""
echo "Building TypeScript..."
npm run build
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Build successful${NC}"
else
    echo -e "${RED}✗ Build failed${NC}"
    exit 1
fi

# Check if example COBOL file exists
echo ""
echo "Checking example files..."
if [ -f "examples/interest-calculation.cbl" ]; then
    echo -e "${GREEN}✓ Example COBOL file found${NC}"
else
    echo -e "${YELLOW}⚠ Example COBOL file not found${NC}"
fi

# Check component files
echo ""
echo "Verifying component files..."

components=(
    "dist/orchestrator.js:Orchestrator"
    "dist/mcp-server/index.js:MCP Server"
    "dist/mcp-server/tools/index.js:MCP Tools"
    "dist/parser/cobol-parser.js:COBOL Parser"
    "dist/analyzer/index.js:Logic Analyzer"
    "dist/generator/lambda-generator.js:Lambda Generator"
    "dist/generator/api-gateway-generator.js:API Gateway Generator"
    "dist/generator/cdk-generator.js:CDK Generator"
    "dist/web/backend/index.js:Web Backend"
)

all_found=true
for component in "${components[@]}"; do
    IFS=':' read -r file name <<< "$component"
    if [ -f "$file" ]; then
        echo -e "${GREEN}✓ $name${NC}"
    else
        echo -e "${RED}✗ $name (missing: $file)${NC}"
        all_found=false
    fi
done

if [ "$all_found" = false ]; then
    echo ""
    echo -e "${RED}✗ Some components are missing${NC}"
    echo "Run 'npm run build' to build all components"
    exit 1
fi

# Run integration tests
echo ""
echo "Running integration tests..."
echo "----------------------------"
node dist/integration-test.js

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ All integration tests passed!${NC}"
    echo ""
    echo "Component Integration Status:"
    echo "  ✓ MCP Server → Parser → Analyzer → Generators"
    echo "  ✓ Web Backend → Orchestrator → All Components"
    echo "  ✓ End-to-end workflow verified"
    echo ""
    echo "You can now:"
    echo "  1. Start MCP server: npm run mcp:start"
    echo "  2. Start web backend: npm run api:dev"
    echo "  3. Start web frontend: npm run web:dev"
    exit 0
else
    echo ""
    echo -e "${RED}✗ Integration tests failed${NC}"
    echo "Check the errors above for details"
    exit 1
fi
