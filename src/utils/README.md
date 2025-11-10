# COBRA Utility Functions

This directory contains utility functions for error handling, progress tracking, and user feedback throughout the COBRA system.

## Error Handler

The error handler provides centralized, user-friendly error management with recovery suggestions.

### Features

- **Standardized Error Codes**: Predefined error codes for all common failure scenarios
- **User-Friendly Messages**: Clear, actionable error messages for end users
- **Recovery Suggestions**: Specific steps users can take to resolve issues
- **Graceful Degradation**: Fallback handlers for non-critical failures
- **Structured Logging**: Consistent error format for debugging

### Usage

```typescript
import {
  createError,
  ErrorCode,
  formatErrorForUser
} from './utils/error-handler.js'

// Create a user-friendly error
const error = createError(
  ErrorCode.PARSE_INVALID_SYNTAX,
  'Missing period at end of statement',
  { line: 42, column: 10 }
)

// Display to user
console.log(formatErrorForUser(error))

// Log for debugging
console.error(formatErrorForLog(error))
```

### Error Codes

#### Parser Errors

- `PARSE_EMPTY_SOURCE`: No COBOL source provided
- `PARSE_INVALID_SYNTAX`: Syntax error in COBOL code
- `PARSE_MISSING_DIVISION`: Required division missing
- `PARSE_TIMEOUT`: Parsing took too long
- `PARSE_UNSUPPORTED_DIALECT`: COBOL dialect not supported

#### Analyzer Errors

- `ANALYZE_INVALID_AST`: Invalid AST structure
- `ANALYZE_NO_PATTERNS`: No banking patterns found
- `ANALYZE_INCOMPLETE_DATA`: Analysis incomplete

#### LLM Errors

- `LLM_API_ERROR`: API request failed
- `LLM_RATE_LIMIT`: Rate limit exceeded
- `LLM_INVALID_RESPONSE`: Unexpected response format
- `LLM_TIMEOUT`: Request timed out

#### Generator Errors

- `GEN_INVALID_INPUT`: Invalid input for generation
- `GEN_TEMPLATE_ERROR`: Template processing failed
- `GEN_VALIDATION_FAILED`: Generated code validation failed

#### System Errors

- `SYSTEM_OUT_OF_MEMORY`: Memory exhausted
- `SYSTEM_TIMEOUT`: Operation timed out
- `SYSTEM_UNKNOWN`: Unexpected error

### Async Error Handling

```typescript
import { withErrorHandling, ErrorCode } from './utils/error-handler.js'

const result = await withErrorHandling(
  async () => {
    return await riskyOperation()
  },
  ErrorCode.PARSE_INVALID_SYNTAX,
  'parsing COBOL file'
)

if (result.success) {
  console.log('Success:', result.data)
} else {
  console.error('Error:', formatErrorForUser(result.error))
}
```

### Fallback Handlers

```typescript
import { createFallbackHandler } from './utils/error-handler.js'

const patterns = await analyzePatterns(ast).catch(
  createFallbackHandler([], 'Pattern recognition failed, using empty list')
)
```

## Progress Tracker

The progress tracker provides real-time feedback for long-running operations.

### Features

- **Multi-Stage Tracking**: Track progress across multiple stages
- **Weighted Progress**: Different stages can have different weights
- **Progress Callbacks**: Real-time updates via callbacks
- **Time Estimation**: Calculate elapsed and remaining time
- **Console Reporter**: Built-in console progress display

### Usage

```typescript
import {
  ProgressTracker,
  createConsoleProgressReporter
} from './utils/progress-tracker.js'

// Create tracker with stage weights
const tracker = new ProgressTracker(
  {
    parsing: 30, // 30% of total
    analyzing: 40, // 40% of total
    generating: 30 // 30% of total
  },
  createConsoleProgressReporter()
)

// Start a stage
tracker.startStage('parsing', 'Parsing COBOL source...')

// Update progress within stage
tracker.updateProgress('parsing', 50, 'Validating syntax...')

// Complete stage
tracker.completeStage('parsing', 'Parsing complete!')

// Get elapsed time
console.log(`Completed in ${tracker.getFormattedElapsedTime()}`)
```

### Custom Progress Callbacks

```typescript
import { ProgressCallback } from './utils/progress-tracker.js'

const customCallback: ProgressCallback = update => {
  console.log(`[${update.stage}] ${update.progress}% - ${update.message}`)

  // Send to UI
  sendToUI({
    stage: update.stage,
    progress: update.progress,
    message: update.message,
    timestamp: update.timestamp
  })
}

const tracker = new ProgressTracker(stages, customCallback)
```

### Progress Bar Display

```typescript
import { createConsoleProgressReporter } from './utils/progress-tracker.js'

// Creates visual progress bars in console:
// [parsing] ████████████░░░░░░░░ 60% - Validating syntax...
const reporter = createConsoleProgressReporter()
```

### Time Estimation

```typescript
import { estimateRemainingTime } from './utils/progress-tracker.js'

const startTime = new Date()
const currentProgress = 45

const remaining = estimateRemainingTime(startTime, currentProgress)
console.log(remaining) // "~2m 15s remaining"
```

## Integration Examples

### Parser with Error Handling

```typescript
import { createError, ErrorCode } from './utils/error-handler.js'

async function parseCobol(source: string): Promise<ParseResult> {
  if (!source || source.trim().length === 0) {
    const error = createError(
      ErrorCode.PARSE_EMPTY_SOURCE,
      'Empty COBOL source provided'
    )
    return {
      ast: null,
      errors: [
        { message: error.userMessage, line: 0, column: 0, severity: 'error' }
      ],
      warnings: error.suggestions
    }
  }

  // Continue parsing...
}
```

### Orchestrator with Progress Tracking

```typescript
import { ProgressTracker } from './utils/progress-tracker.js'

async function analyzeComplete(
  cobolSource: string,
  onProgress?: ProgressCallback
) {
  const tracker = new ProgressTracker(
    {
      parsing: 20,
      analyzing: 30,
      generating: 50
    },
    onProgress
  )

  tracker.startStage('parsing', 'Parsing COBOL...')
  const parseResult = await parse(cobolSource)
  tracker.completeStage('parsing', 'Parse complete!')

  tracker.startStage('analyzing', 'Analyzing logic...')
  const analysis = await analyze(parseResult.ast)
  tracker.completeStage('analyzing', 'Analysis complete!')

  tracker.startStage('generating', 'Generating code...')
  const code = await generate(analysis)
  tracker.completeStage('generating', 'Generation complete!')

  return { parseResult, analysis, code }
}
```

### Web API with Error Responses

```typescript
import {
  createError,
  ErrorCode,
  formatErrorForUser
} from './utils/error-handler.js'

app.post('/api/analyze', async (req, res) => {
  try {
    const { cobolSource } = req.body

    if (!cobolSource) {
      const error = createError(
        ErrorCode.PARSE_EMPTY_SOURCE,
        'No COBOL source provided'
      )
      return res.status(400).json({
        error: error.userMessage,
        suggestions: error.suggestions
      })
    }

    const result = await analyzeCobol(cobolSource)
    res.json(result)
  } catch (error) {
    const cobraError = createError(ErrorCode.SYSTEM_UNKNOWN, error.message)
    res.status(500).json({
      error: formatErrorForUser(cobraError)
    })
  }
})
```

## Best Practices

### Error Handling

1. **Use Specific Error Codes**: Choose the most specific error code for the situation
2. **Provide Context**: Include relevant details in the error details object
3. **User-Friendly Messages**: Errors should be understandable by non-technical users
4. **Actionable Suggestions**: Always provide steps the user can take
5. **Log for Debugging**: Use `formatErrorForLog()` for technical logs

### Progress Tracking

1. **Meaningful Stages**: Use stage names that describe what's happening
2. **Accurate Weights**: Weight stages based on actual time taken
3. **Frequent Updates**: Update progress at meaningful milestones
4. **Clear Messages**: Progress messages should be specific and informative
5. **Handle Failures**: Update progress even when stages fail

### Graceful Degradation

1. **Non-Critical Failures**: Use fallback handlers for optional features
2. **Partial Results**: Return partial results when possible
3. **Warning Messages**: Warn users about degraded functionality
4. **Continue Processing**: Don't fail completely for minor issues

## Testing

### Error Handler Tests

```typescript
import { createError, ErrorCode } from './utils/error-handler.js'

test('creates user-friendly error', () => {
  const error = createError(ErrorCode.PARSE_EMPTY_SOURCE, 'Test error')

  expect(error.code).toBe(ErrorCode.PARSE_EMPTY_SOURCE)
  expect(error.recoverable).toBe(true)
  expect(error.suggestions.length).toBeGreaterThan(0)
})
```

### Progress Tracker Tests

```typescript
import { ProgressTracker } from './utils/progress-tracker.js'

test('tracks progress across stages', () => {
  const updates: any[] = []
  const tracker = new ProgressTracker({ stage1: 50, stage2: 50 }, update =>
    updates.push(update)
  )

  tracker.startStage('stage1', 'Starting...')
  tracker.completeStage('stage1', 'Done!')

  expect(updates.length).toBeGreaterThan(0)
  expect(updates[updates.length - 1].progress).toBe(50)
})
```

## Performance Considerations

- Error creation is lightweight (no I/O operations)
- Progress callbacks are synchronous (keep them fast)
- Use no-op callbacks when progress tracking not needed
- Error formatting is done on-demand (not during creation)

## Future Enhancements

- [ ] Error recovery strategies (automatic retry)
- [ ] Progress persistence (resume after restart)
- [ ] Error analytics and reporting
- [ ] Internationalization for error messages
- [ ] Progress visualization components
