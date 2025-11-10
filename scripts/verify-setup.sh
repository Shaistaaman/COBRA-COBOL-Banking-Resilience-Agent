#!/bin/bash

echo "🔍 Verifying COBRA setup..."
echo ""

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "❌ node_modules not found. Run: npm install"
    exit 1
fi
echo "✅ Dependencies installed"

# Check if dist exists
if [ ! -d "dist" ]; then
    echo "❌ dist directory not found. Run: npm run build"
    exit 1
fi
echo "✅ Project built"

# Check if MCP server exists
if [ ! -f "dist/mcp-server/index.js" ]; then
    echo "❌ MCP server not found in dist/"
    exit 1
fi
echo "✅ MCP server compiled"

# Check if MCP config exists
if [ ! -f ".kiro/settings/mcp.json" ]; then
    echo "❌ MCP configuration not found"
    exit 1
fi
echo "✅ MCP configuration present"

# Verify directory structure
echo ""
echo "📁 Directory structure:"
for dir in src/mcp-server src/parser src/analyzer src/generator templates examples tests docs; do
    if [ -d "$dir" ]; then
        echo "  ✅ $dir"
    else
        echo "  ❌ $dir (missing)"
    fi
done

echo ""
echo "🎉 Setup verification complete!"
echo ""
echo "Next steps:"
echo "  1. Restart Kiro or reconnect MCP servers"
echo "  2. COBRA tools will be available in Kiro"
echo "  3. Continue with task 2: COBOL parser integration"
