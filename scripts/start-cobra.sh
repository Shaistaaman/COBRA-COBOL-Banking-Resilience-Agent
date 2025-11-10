#!/bin/bash

# COBRA Startup Script
# Launches all components for local zero-cost operation

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo ""
echo -e "${CYAN}🐍 Starting COBRA (COBOL Banking Resilience Agent)${NC}"
echo -e "${CYAN}Zero-cost local execution mode${NC}"
echo ""

# Check if build is needed
if [ ! -d "dist" ]; then
    echo -e "${YELLOW}Building TypeScript...${NC}"
    npm run build
    echo -e "${GREEN}✓ Build complete${NC}"
fi

# Check for LLM API key
if [ -z "$OPENAI_API_KEY" ] && [ -z "$ANTHROPIC_API_KEY" ]; then
    echo -e "${YELLOW}⚠ No LLM API key found${NC}"
    echo -e "${BLUE}ℹ Set OPENAI_API_KEY or ANTHROPIC_API_KEY for natural-language explanations${NC}"
    echo -e "${BLUE}ℹ System will work without it, but explanations will be skipped${NC}"
    echo ""
fi

# Create PID directory
mkdir -p .cobra-pids

# Function to cleanup on exit
cleanup() {
    echo ""
    echo -e "${YELLOW}Shutting down COBRA components...${NC}"
    
    if [ -f .cobra-pids/api.pid ]; then
        kill $(cat .cobra-pids/api.pid) 2>/dev/null || true
        rm .cobra-pids/api.pid
    fi
    
    if [ -f .cobra-pids/frontend.pid ]; then
        kill $(cat .cobra-pids/frontend.pid) 2>/dev/null || true
        rm .cobra-pids/frontend.pid
    fi
    
    echo -e "${GREEN}✓ COBRA stopped${NC}"
    exit 0
}

trap cleanup SIGINT SIGTERM

# Start backend API
echo -e "${BLUE}Starting backend API server...${NC}"
npm run api:dev > logs/api.log 2>&1 &
API_PID=$!
echo $API_PID > .cobra-pids/api.pid

# Wait for API to start
sleep 3

if kill -0 $API_PID 2>/dev/null; then
    echo -e "${GREEN}✓ Backend API running on http://localhost:3001${NC}"
else
    echo -e "${RED}✗ Backend API failed to start${NC}"
    exit 1
fi

# Check if frontend exists
if [ -d "src/web/frontend" ]; then
    echo -e "${BLUE}Starting frontend development server...${NC}"
    cd src/web/frontend
    npm run dev > ../../../logs/frontend.log 2>&1 &
    FRONTEND_PID=$!
    echo $FRONTEND_PID > ../../../.cobra-pids/frontend.pid
    cd ../../..
    
    sleep 3
    
    if kill -0 $FRONTEND_PID 2>/dev/null; then
        echo -e "${GREEN}✓ Frontend running on http://localhost:3000${NC}"
    else
        echo -e "${YELLOW}⚠ Frontend failed to start (check logs/frontend.log)${NC}"
    fi
else
    echo -e "${YELLOW}⚠ Frontend not found (web interface not available)${NC}"
fi

# Display status
echo ""
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}COBRA is running!${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}Available endpoints:${NC}"
echo "  • Backend API:  http://localhost:3001"
echo "  • Health check: http://localhost:3001/health"
if [ -d "src/web/frontend" ]; then
    echo "  • Web UI:       http://localhost:3000"
fi
echo ""
echo -e "${BLUE}MCP Server:${NC}"
echo "  • Configure in .kiro/settings/mcp.json"
echo "  • Run: npm run mcp:start"
echo ""
echo -e "${BLUE}Logs:${NC}"
echo "  • API:      logs/api.log"
if [ -d "src/web/frontend" ]; then
    echo "  • Frontend: logs/frontend.log"
fi
echo ""
echo -e "${YELLOW}Press Ctrl+C to stop all services${NC}"
echo ""

# Keep script running
wait
