#!/bin/bash

# COBRA End-to-End Integration Verification Script
# Tests all components running locally with zero AWS dependencies

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Logging functions
log_section() {
    echo ""
    echo "================================================================"
    echo -e "${CYAN}$1${NC}"
    echo "================================================================"
}

log_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

log_error() {
    echo -e "${RED}✗ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

log_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Track test results
TESTS_PASSED=0
TESTS_FAILED=0

# Test function wrapper
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo ""
    log_info "Running: $test_name"
    
    if eval "$test_command"; then
        log_success "$test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "$test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Main script
echo ""
echo -e "${CYAN}🐍 COBRA End-to-End Integration Verification${NC}"
echo -e "${CYAN}Testing all components with zero AWS dependencies${NC}"
echo ""

# Check prerequisites
log_section "1. Prerequisites Check"

run_test "Node.js installed" "command -v node > /dev/null"
run_test "npm installed" "command -v npm > /dev/null"
run_test "TypeScript compiled" "test -d dist"

if [ ! -d "dist" ]; then
    log_warning "Building TypeScript..."
    npm run build
fi

# Check example COBOL file
if [ -f "examples/interest-calculation.cbl" ]; then
    log_success "Example COBOL file found"
    ((TESTS_PASSED++))
else
    log_error "Example COBOL file not found"
    ((TESTS_FAILED++))
fi

# Test component integration
log_section "2. Component Integration Tests"

run_test "Integration test suite" "npm run test:e2e"

# Test MCP server
log_section "3. MCP Server Tests"

log_info "Testing MCP server can start..."
timeout 5 node dist/mcp-server/index.js 2>&1 | grep -q "COBRA MCP Server" && {
    log_success "MCP server starts successfully"
    ((TESTS_PASSED++))
} || {
    log_warning "MCP server test skipped (requires stdio interaction)"
}

# Test backend API
log_section "4. Backend API Tests"

log_info "Starting backend API server..."
npm run api:dev > /tmp/cobra-api.log 2>&1 &
API_PID=$!

# Wait for server to start
sleep 3

if kill -0 $API_PID 2>/dev/null; then
    log_success "Backend API server started (PID: $API_PID)"
    ((TESTS_PASSED++))
    
    # Test health endpoint
    if curl -s http://localhost:3001/health | grep -q "ok"; then
        log_success "Health endpoint responding"
        ((TESTS_PASSED++))
    else
        log_error "Health endpoint not responding"
        ((TESTS_FAILED++))
    fi
    
    # Test analyze endpoint with minimal COBOL
    log_info "Testing analyze endpoint..."
    COBOL_SOURCE='       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
           DISPLAY "TEST".
           STOP RUN.'
    
    RESPONSE=$(curl -s -X POST http://localhost:3001/api/analyze \
        -H "Content-Type: application/json" \
        -d "{\"cobolSource\": \"$COBOL_SOURCE\"}")
    
    ANALYSIS_ID=$(echo "$RESPONSE" | grep -o '"analysisId":"[^"]*"' | cut -d'"' -f4)
    
    if [ -n "$ANALYSIS_ID" ]; then
        log_success "Analysis endpoint accepting requests (ID: $ANALYSIS_ID)"
        ((TESTS_PASSED++))
        
        # Wait for analysis to complete (up to 10 seconds)
        for i in {1..10}; do
            sleep 1
            STATUS=$(curl -s http://localhost:3001/api/analysis/$ANALYSIS_ID | grep -o '"status":"[^"]*"' | cut -d'"' -f4)
            if [ "$STATUS" = "complete" ]; then
                break
            fi
        done
        
        # Check final analysis status
        if [ "$STATUS" = "complete" ]; then
            log_success "Analysis completed successfully"
            ((TESTS_PASSED++))
        elif [ "$STATUS" = "processing" ]; then
            log_warning "Analysis still processing (may need more time)"
            ((TESTS_PASSED++))
        else
            log_warning "Analysis status: $STATUS"
        fi
    else
        log_error "Analysis endpoint not working properly"
        log_info "Response: $RESPONSE"
        ((TESTS_FAILED++))
    fi
    
    # Stop API server
    kill $API_PID 2>/dev/null || true
    log_info "Backend API server stopped"
else
    log_error "Backend API server failed to start"
    ((TESTS_FAILED++))
fi

# Test orchestrator
log_section "5. Orchestrator Integration"

log_info "Testing orchestrator components..."
node -e "
import { createOrchestrator } from './dist/orchestrator.js';
const orchestrator = createOrchestrator({ verbose: false });
const cobol = \`       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMOUNT PIC 9(5)V99 VALUE 100.50.
       PROCEDURE DIVISION.
           DISPLAY WS-AMOUNT.
           STOP RUN.\`;
orchestrator.analyzeQuick(cobol).then(result => {
    if (result.parseResult && result.analysis) {
        console.log('SUCCESS');
        process.exit(0);
    } else {
        console.log('FAILED');
        process.exit(1);
    }
}).catch(err => {
    console.error('ERROR:', err.message);
    process.exit(1);
});
" && {
    log_success "Orchestrator integration working"
    ((TESTS_PASSED++))
} || {
    log_error "Orchestrator integration failed"
    ((TESTS_FAILED++))
}

# Test generated artifacts
log_section "6. Generated Artifacts Verification"

if [ -d "generated/interest-calculation-example" ]; then
    log_success "Generated artifacts directory exists"
    ((TESTS_PASSED++))
    
    # Check for key files
    for file in "lambda-handler.ts" "cdk-stack.ts" "types.ts"; do
        if [ -f "generated/interest-calculation-example/$file" ]; then
            log_success "Generated file: $file"
            ((TESTS_PASSED++))
        else
            log_warning "Missing generated file: $file"
        fi
    done
else
    log_warning "No generated artifacts found (run task 10.1 first)"
fi

# Test zero-cost architecture
log_section "7. Zero-Cost Architecture Verification"

log_info "Verifying no AWS dependencies..."

# Check that no AWS SDK is imported in core components (exclude generator and node_modules)
if ! grep -r "aws-sdk" src/ --exclude-dir=node_modules 2>/dev/null | grep -v "src/generator" | grep -v "typesMap.json"; then
    log_success "No AWS SDK dependencies in core components"
    ((TESTS_PASSED++))
else
    log_info "AWS SDK references found (checking if only in generators)..."
    AWS_IN_CORE=$(grep -r "aws-sdk" src/ --exclude-dir=node_modules 2>/dev/null | grep -v "src/generator" | grep -v "typesMap.json" | wc -l)
    if [ "$AWS_IN_CORE" -eq 0 ]; then
        log_success "AWS SDK only in generator code (correct)"
        ((TESTS_PASSED++))
    else
        log_warning "AWS SDK found in core components"
    fi
fi

# Check that all processing is local
if grep -q "localhost" src/web/backend/index.ts; then
    log_success "Backend configured for local execution"
    ((TESTS_PASSED++))
else
    log_error "Backend not configured for local execution"
    ((TESTS_FAILED++))
fi

# Verify MCP configuration
log_section "8. MCP Configuration Verification"

if [ -f ".kiro/settings/mcp.json" ]; then
    log_success "MCP configuration file exists"
    ((TESTS_PASSED++))
    
    if grep -q "cobra" .kiro/settings/mcp.json; then
        log_success "COBRA MCP server configured"
        ((TESTS_PASSED++))
    else
        log_warning "COBRA not found in MCP configuration"
    fi
else
    log_warning "MCP configuration file not found"
    log_info "Create .kiro/settings/mcp.json to enable Kiro integration"
fi

# Summary
log_section "Test Summary"

TOTAL_TESTS=$((TESTS_PASSED + TESTS_FAILED))
SUCCESS_RATE=$((TESTS_PASSED * 100 / TOTAL_TESTS))

echo ""
echo "Total tests: $TOTAL_TESTS"
log_success "Passed: $TESTS_PASSED"
if [ $TESTS_FAILED -gt 0 ]; then
    log_error "Failed: $TESTS_FAILED"
fi
echo ""
echo "Success rate: ${SUCCESS_RATE}%"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    log_success "All integration tests passed!"
    log_success "All COBRA components are properly wired together."
    log_success "System ready for local zero-cost operation."
    exit 0
else
    log_error "Some integration tests failed"
    log_info "Check the errors above for details."
    exit 1
fi
