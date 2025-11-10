# COBRA Error Handling and User Feedback

This document describes the comprehensive error handling and user feedback system implemented in COBRA.

## Overview

COBRA now includes a robust error handling system that provides:

- **User-friendly error messages** with clear explanations
- **Actionable recovery suggestions** for every error type
- **Progress tracking** for long-running operations
- **Graceful degradation** when optional features fail
- **Consistent error format** across all components

## Error Handling System

### Error Codes

All errors in COBRA are categorized with specific error codes:

#### Parser Errors

- `PARSE_EMPTY_SOURCE` - No COBOL source code provided
- `PARSE_INVALID_SYNTAX` - Syntax error in COBOL code
- `PARSE_MISSING_DIVISION` - Required COBOL division missing
- `PARSE_TIMEOUT` - Parsing exceeded time limit
- `PARSE_UNSUPPORTED_DIALECT` - COBOL dialect not fully supported

#### Analyzer Errors

- `ANALYZE_INVALID_AST` - Invalid AST structure
- `ANALYZE_NO_PATTERNS` - No recognizable banking patterns found
- `ANALYZE_INCOMPLETE_DATA` - Analysis completed with missing information

#### LLM Errors

- `LLM_API_ERROR` - AI service temporarily unavailable
- `LLM_RATE_LIMIT` - AI service rate limit reached
- `LLM_INVALID_RESPONSE` - AI service returned unexpected response
- `LLM_TIMEOUT` - AI service request timed out

#### Generator Errors

- `GEN_INVALID_INPUT` - Invalid input for code generation
- `GEN_TEMPLATE_ERROR` - Code generation template error
- `GEN_VALIDATION_FAILED` - Generated code failed validation

#### System Errors

- `SYSTEM_OUT_OF_MEMORY` - System ran out of memory
- `SYSTEM_TIMEOUT` - Operation timed out
- `SYSTEM_UNKNOWN` - An unexpected error occurred

### Error Structure

Each error includes:

```typescript
interface CobraError {
  code: ErrorCode // Specific error code
  message: string // Technical error message
  userMessage: string // User-friendly explanation
  details?: any // Additional context
  recoverable: boolean // Can the user recover?
  suggestions: string[] // Steps to resolve the issue
  timestamp: Date // When the error occurred
}
```

### Example Error Output

```
❌ COBOL syntax error detected

Details: Missing period at end of statement on line 42

Suggestions:
  1. Check for missing periods at end of statements
  2. Verify division headers are correctly formatted
  3. Ensure proper indentation and column alignment
  4. Review COBOL syntax rules for your dialect
```

## Progress Tracking

### Multi-Stage Progress

COBRA tracks progress across multiple stages:

1. **Parsing** (20%) - Parsing COBOL source code
2. **Analyzing** (20%) - Analyzing business logic patterns
3. **Explaining** (20%) - Generating natural-language explanation
4. **Generating Spec** (20%) - Creating specification documents
5. **Generating Code** (20%) - Producing AWS infrastructure code

### Progress Updates

Progress updates include:

```typescript
interface ProgressUpdate {
  stage: string // Current stage name
  progress: number // Overall progress (0-100)
  message: string // What's happening now
  timestamp: Date // When this update occurred
  details?: any // Additional information
}
```

### Example Progress Output

```
[parsing] ████████████░░░░░░░░ 60% - Validating parsed structure...
[analyzing] ████████████████████ 100% - Found 3 banking patterns
[explaining] ██████░░░░░░░░░░░░░░ 30% - Generating explanation...
```

## Component-Specific Error Handling

### Parser Error Handling

The parser now provides:

- **Detailed syntax error messages** with line and column numbers
- **Suggestions for common COBOL mistakes**
- **Warnings for missing optional divisions**
- **Timeout detection** for large files
- **Dialect compatibility warnings**

Example:

```typescript
// Empty source
const error = createError(
  ErrorCode.PARSE_EMPTY_SOURCE,
  'Empty COBOL source provided'
)
// Suggestions: Provide valid COBOL source code, Check that the file is not empty, etc.

// Missing division
const error = createError(
  ErrorCode.PARSE_MISSING_DIVISION,
  'Missing IDENTIFICATION DIVISION'
)
// Suggestions: Add IDENTIFICATION DIVISION at the start, Include PROGRAM-ID statement, etc.
```

### LLM Error Handling

The LLM client now handles:

- **API timeouts** (60 second limit)
- **Rate limiting** with clear messages
- **Invalid responses** with fallback behavior
- **Network errors** with retry suggestions
- **API key issues** with configuration help

Example:

```typescript
// Rate limit
const error = createError(
  ErrorCode.LLM_RATE_LIMIT,
  'OpenAI rate limit exceeded'
)
// Suggestions: Wait a few minutes, Consider upgrading API plan, Use caching, etc.

// Timeout
const error = createError(ErrorCode.LLM_TIMEOUT, 'OpenAI request timed out')
// Suggestions: Try smaller code sample, Check internet connection, etc.
```

### Orchestrator Error Handling

The orchestrator provides:

- **Input validation** before processing
- **Stage-by-stage progress tracking**
- **Graceful degradation** for optional features
- **Detailed error context** for debugging
- **Elapsed time tracking**

Example:

```typescript
// Invalid input
const validation = orchestrator.validateCobolSource(source)
if (!validation.valid) {
  const error = createError(
    ErrorCode.PARSE_EMPTY_SOURCE,
    validation.errors.join('; ')
  )
  throw new Error(formatErrorForUser(error))
}

// Progress tracking
const tracker = new ProgressTracker(stages, onProgress)
tracker.startStage('parsing', 'Parsing COBOL source...')
// ... perform parsing ...
tracker.completeStage('parsing', 'Parsing complete ✓')
```

### Web API Error Handling

The web backend now provides:

- **Structured error responses** with suggestions
- **Progress updates** during analysis
- **Rate limiting** with clear messages
- **Input validation** with helpful feedback
- **Error details** for debugging

Example API responses:

```json
// Error response
{
  "error": "❌ COBOL syntax error detected\n\nSuggestions:\n  1. Check for missing periods...",
  "errorDetails": {
    "code": "PARSE_INVALID_SYNTAX",
    "suggestions": [
      "Check for missing periods at end of statements",
      "Verify division headers are correctly formatted"
    ]
  }
}

// Progress response
{
  "id": "abc-123",
  "status": "processing",
  "progress": {
    "stage": "analyzing",
    "percentage": 45,
    "message": "Analyzing business logic patterns..."
  }
}
```

## Graceful Degradation

COBRA handles optional feature failures gracefully:

### LLM Explanation Failure

If LLM explanation fails, COBRA:

- Logs a warning
- Continues with other stages
- Returns results without explanation
- Provides clear message about what's missing

```typescript
try {
  explanation = await generateExplanation(ast, analysis)
} catch (error) {
  this.log(`⚠️  LLM explanation failed: ${error.message}`)
  tracker.completeStage('explaining', 'Explanation skipped (LLM unavailable)')
}
```

### Pattern Recognition Failure

If no patterns are found, COBRA:

- Logs a warning
- Continues with code generation
- Provides suggestions for manual review
- Doesn't fail the entire analysis

```typescript
if (analysis.patterns.length === 0) {
  const error = createError(
    ErrorCode.ANALYZE_NO_PATTERNS,
    'No recognizable banking patterns found'
  )
  this.log(`⚠️  ${error.userMessage}`)
  // Continue processing...
}
```

### Unsupported COBOL Features

For unsupported features, COBRA:

- Warns about limitations
- Provides fallback behavior
- Suggests alternatives
- Documents what needs manual review

```typescript
if (!supportedDialects.includes(dialect)) {
  const error = createError(
    ErrorCode.PARSE_UNSUPPORTED_DIALECT,
    `Dialect ${dialect} not fully supported`
  )
  warnings.push(error.userMessage)
  warnings.push(...error.suggestions)
}
```

## User Feedback Best Practices

### Clear Messages

✅ **Good**: "COBOL syntax error detected on line 42: Missing period at end of statement"

❌ **Bad**: "Parse error at position 1234"

### Actionable Suggestions

✅ **Good**: "Add a period at the end of line 42, after the MOVE statement"

❌ **Bad**: "Fix the syntax error"

### Progress Updates

✅ **Good**: "Analyzing business logic patterns... Found 3 interest calculations"

❌ **Bad**: "Processing..."

### Error Recovery

✅ **Good**: "Rate limit reached. Wait 2 minutes or upgrade your API plan"

❌ **Bad**: "Error 429"

## Testing Error Handling

### Unit Tests

```typescript
import { createError, ErrorCode } from '../utils/error-handler.js'

test('creates recoverable error with suggestions', () => {
  const error = createError(ErrorCode.PARSE_EMPTY_SOURCE, 'Test error')

  expect(error.recoverable).toBe(true)
  expect(error.suggestions.length).toBeGreaterThan(0)
  expect(error.userMessage).toContain('COBOL source')
})
```

### Integration Tests

```typescript
test('handles parse errors gracefully', async () => {
  const result = await parseCobol('')

  expect(result.ast).toBeNull()
  expect(result.errors.length).toBeGreaterThan(0)
  expect(result.warnings.length).toBeGreaterThan(0)
})
```

### End-to-End Tests

```typescript
test('provides progress updates during analysis', async () => {
  const updates: ProgressUpdate[] = []

  await orchestrator.analyzeComplete(cobolSource, update =>
    updates.push(update)
  )

  expect(updates.length).toBeGreaterThan(0)
  expect(updates[0].stage).toBe('parsing')
  expect(updates[updates.length - 1].progress).toBe(100)
})
```

## Performance Impact

The error handling system has minimal performance impact:

- **Error creation**: < 1ms (no I/O operations)
- **Progress callbacks**: Synchronous, < 1ms each
- **Error formatting**: On-demand, only when displaying to user
- **Memory overhead**: Negligible (small objects)

## Future Enhancements

Planned improvements:

1. **Error Analytics**: Track common errors for documentation improvements
2. **Automatic Recovery**: Retry failed operations with exponential backoff
3. **Internationalization**: Support multiple languages for error messages
4. **Error Reporting**: Allow users to report errors with context
5. **Progress Persistence**: Resume long-running operations after restart

## Summary

COBRA's error handling system provides:

✅ **User-friendly** - Clear messages anyone can understand
✅ **Actionable** - Specific steps to resolve issues
✅ **Comprehensive** - Covers all error scenarios
✅ **Consistent** - Same format across all components
✅ **Informative** - Progress updates for long operations
✅ **Resilient** - Graceful degradation for optional features

This ensures a professional user experience even when things go wrong.
