# LLM Integration Module

This module provides natural-language explanation generation for COBOL analysis using OpenAI or Anthropic APIs.

## Features

- **Multi-Provider Support**: Works with both OpenAI (GPT-4) and Anthropic (Claude)
- **Intelligent Caching**: Caches analysis results to minimize API costs
- **Structured Output**: Generates formatted markdown documentation
- **Fallback Handling**: Provides basic explanations when LLM is unavailable
- **Cost Optimization**: Supports quick summaries for faster, cheaper analysis

## Usage

### Basic Setup

```typescript
import { createExplanationGenerator } from './llm/index.js'

// Configure with OpenAI
const generator = createExplanationGenerator({
  llmConfig: {
    provider: 'openai',
    apiKey: process.env.OPENAI_API_KEY || '',
    model: 'gpt-4o-mini', // or 'gpt-4'
    temperature: 0.3,
    maxTokens: 2000
  },
  cacheEnabled: true,
  cacheTTL: 3600 // 1 hour
})

// Or configure with Anthropic
const generator = createExplanationGenerator({
  llmConfig: {
    provider: 'anthropic',
    apiKey: process.env.ANTHROPIC_API_KEY || '',
    model: 'claude-3-5-sonnet-20241022',
    temperature: 0.3,
    maxTokens: 2000
  }
})
```

### Generate Explanation

```typescript
import { createCobolParser } from './parser/index.js'
import { createLogicAnalyzer } from './analyzer/index.js'

// Parse COBOL code
const parser = createCobolParser()
const parseResult = parser.parse(cobolSource)

// Analyze logic
const analyzer = createLogicAnalyzer()
const analysis = analyzer.analyze(parseResult.ast, 'program.cbl')

// Generate explanation
const explanation = await generator.generateExplanation(
  parseResult.ast,
  analysis,
  'program.cbl'
)

console.log(explanation.summary)
console.log(explanation.businessLogic)
console.log(explanation.rules)
```

### Format Output

```typescript
import { formatAnalysisAsMarkdown, formatAsJSON } from './llm/formatter.js'

// Format as markdown documentation
const markdown = formatAnalysisAsMarkdown(
  parseResult.ast,
  analysis,
  explanation,
  'program.cbl',
  {
    includeSourceReferences: true,
    includeMetadata: true,
    maxRulesDisplay: 20
  }
)

// Save to file
await fs.writeFile('analysis.md', markdown)

// Or format as JSON for API responses
const json = formatAsJSON(parseResult.ast, analysis, explanation, 'program.cbl')
```

### Quick Summary (Cost-Optimized)

```typescript
// Generate quick summary without full analysis
const summary = await generator.generateQuickSummary(
  parseResult.ast,
  'program.cbl'
)

console.log(summary)
```

## API Reference

### ExplanationGenerator

#### `generateExplanation(ast, analysis, fileName?)`

Generates comprehensive natural-language explanation from AST and analysis.

**Parameters:**

- `ast: AST` - Parsed COBOL Abstract Syntax Tree
- `analysis: LogicAnalysis` - Logic analysis results
- `fileName?: string` - Optional source file name

**Returns:** `Promise<ExplanationResponse>`

**Response Structure:**

```typescript
{
  summary: string              // Brief overview (2-3 sentences)
  businessLogic: string        // Detailed explanation
  inputs: string[]             // Input sources
  outputs: string[]            // Output destinations
  rules: string[]              // Business rules list
  complexity: string           // Low/Medium/High
  recommendations: string[]    // Modernization suggestions
}
```

#### `generateQuickSummary(ast, fileName?)`

Generates quick summary without full analysis (faster, cheaper).

**Returns:** `Promise<string>`

#### `getCacheStats()`

Returns cache statistics.

**Returns:**

```typescript
{
  size: number
  enabled: boolean
  ttlSeconds: number
}
```

#### `clearCache()`

Clears all cached entries.

### Formatter Functions

#### `formatAnalysisAsMarkdown(ast, analysis, explanation, fileName?, options?)`

Formats complete analysis as markdown documentation.

**Options:**

```typescript
{
  includeSourceReferences?: boolean  // Include line numbers (default: true)
  includeMetadata?: boolean          // Include metadata section (default: true)
  maxRulesDisplay?: number           // Max rules to display (default: 20)
}
```

#### `formatAsJSON(ast, analysis, explanation, fileName?)`

Formats analysis as JSON string.

#### `formatAsPlainText(ast, analysis, explanation)`

Formats analysis as plain text for console output.

#### `formatQuickSummary(ast, analysis, summary)`

Formats minimal summary output.

#### `formatDataStructureDetails(structure)`

Formats detailed data structure information with field table.

## Configuration

### Environment Variables

```bash
# OpenAI
export OPENAI_API_KEY="sk-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."
```

### LLM Provider Selection

**OpenAI (Recommended for Development):**

- Model: `gpt-4o-mini` (cheaper, faster) or `gpt-4` (more accurate)
- Cost: ~$0.10 per analysis with gpt-4o-mini
- Speed: 5-10 seconds per analysis

**Anthropic (Recommended for Production):**

- Model: `claude-3-5-sonnet-20241022`
- Cost: ~$0.10 per analysis
- Speed: 5-10 seconds per analysis

### Cache Configuration

```typescript
{
  cacheEnabled: true,      // Enable/disable caching
  cacheTTL: 3600          // Time-to-live in seconds (1 hour)
}
```

**Cache Benefits:**

- Eliminates duplicate API calls for same COBOL code
- Reduces costs by ~90% for repeated analysis
- Improves response time from 10s to <100ms

## Cost Optimization

### Strategies

1. **Enable Caching**: Reuse results for identical COBOL code
2. **Use Quick Summaries**: For simple overviews, use `generateQuickSummary()`
3. **Choose Cheaper Models**: Use `gpt-4o-mini` instead of `gpt-4`
4. **Batch Processing**: Analyze multiple files in sequence to reuse context
5. **Pre-generate Examples**: Cache explanations for example COBOL files at startup

### Cost Estimates

**Per Analysis (with gpt-4o-mini):**

- Input tokens: ~2,000 ($0.06)
- Output tokens: ~1,000 ($0.06)
- **Total: ~$0.12 per analysis**

**Monthly Costs:**

- 10 analyses/day: ~$36/month
- 5 analyses/day: ~$18/month
- Demo/testing only: ~$2-5/month

**With 90% cache hit rate:**

- 10 analyses/day: ~$3.60/month
- 5 analyses/day: ~$1.80/month

## Error Handling

The module includes comprehensive error handling:

1. **LLM API Failures**: Falls back to basic explanation using analysis data
2. **JSON Parsing Errors**: Attempts text-based extraction
3. **Network Timeouts**: Returns cached result if available
4. **Invalid API Keys**: Throws clear error message

```typescript
try {
  const explanation = await generator.generateExplanation(ast, analysis)
} catch (error) {
  console.error('Explanation generation failed:', error)
  // Fallback explanation is automatically provided
}
```

## Examples

See `examples/` directory for complete examples:

- `examples/interest-calculation.cbl` - Banking interest calculation
- Analysis output in `examples/analysis-output.md`

## Testing

```bash
# Run unit tests
npm test src/llm

# Test with real API (requires API key)
npm run test:integration -- src/llm
```

## Performance

- **Parse Time**: <1 second (local processing)
- **LLM Call**: 5-10 seconds (network dependent)
- **Formatting**: <100ms (local processing)
- **Total**: ~10 seconds for first analysis, <100ms for cached

## Limitations

- Requires internet connection for LLM API calls
- API costs apply per analysis (mitigated by caching)
- LLM output quality depends on COBOL code complexity
- Maximum COBOL file size: ~10,000 lines (token limit)
