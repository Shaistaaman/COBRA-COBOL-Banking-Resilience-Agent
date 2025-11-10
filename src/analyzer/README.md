# COBRA Logic Analyzer

The Logic Analyzer component provides comprehensive analysis of COBOL business logic, extracting patterns, rules, and data flows from parsed COBOL programs.

## Components

### 1. Pattern Recognizer (`pattern-recognizer.ts`)

Identifies common banking operations in COBOL code:

- **Interest Calculation**: Detects formulas involving principal, rate, and interest
- **Loan Amortization**: Recognizes payment schedules and installment calculations
- **Transaction Validation**: Identifies balance checks and limit validations
- **Batch Processing**: Detects file processing loops and workflows

**Usage:**

```typescript
import { createPatternRecognizer } from './pattern-recognizer.js'

const recognizer = createPatternRecognizer({ confidenceThreshold: 0.6 })
const patterns = recognizer.identifyPatterns(ast)
```

### 2. Business Rule Extractor (`business-rule-extractor.ts`)

Extracts business logic from COBOL PROCEDURE DIVISION:

- **IF-THEN-ELSE Logic**: Converts conditional statements to business rules
- **COMPUTE Operations**: Extracts calculation formulas
- **Arithmetic Operations**: Identifies ADD, SUBTRACT, MULTIPLY, DIVIDE operations
- **PERFORM Statements**: Maps workflow steps and loops

**Usage:**

```typescript
import { createBusinessRuleExtractor } from './business-rule-extractor.js'

const extractor = createBusinessRuleExtractor()
const rules = extractor.extractBusinessRules(ast, 'program.cbl')
```

### 3. Data Flow Analyzer (`data-flow-analyzer.ts`)

Tracks data movement through COBOL programs:

- **Variable Assignments**: Tracks MOVE, COMPUTE, and arithmetic operations
- **Input Sources**: Identifies ACCEPT and READ statements
- **Output Destinations**: Maps DISPLAY and WRITE statements
- **Dependency Graph**: Builds complete data flow visualization

**Usage:**

```typescript
import { createDataFlowAnalyzer } from './data-flow-analyzer.js'

const analyzer = createDataFlowAnalyzer()
const dataFlow = analyzer.buildDataFlowGraph(ast)
```

### 4. Main Logic Analyzer (`index.ts`)

Coordinates all analysis components and provides a unified interface:

**Usage:**

```typescript
import { createLogicAnalyzer } from './index.js'

const analyzer = createLogicAnalyzer()
const analysis = analyzer.analyze(ast, 'program.cbl')

// Access results
console.log(analysis.patterns) // Banking patterns
console.log(analysis.businessRules) // Extracted rules
console.log(analysis.dataStructures) // Data structures
console.log(analysis.dependencies) // Program dependencies
console.log(analysis.entryPoints) // Entry points
```

## Testing

Run the test script to verify the analyzer:

```bash
npm run build
node dist/analyzer/test-analyzer.js
```

## Requirements Satisfied

- **Requirement 1.2**: Extracts business logic patterns including interest calculations, batch processing, and transaction validations
- **Requirement 1.3**: Generates business rules from COBOL logic with inputs, outputs, and conditions
- **Requirement 1.4**: Identifies data structures and tracks data flow through the program
- **Requirement 5.3**: Provides pattern recognition for banking operations with confidence scores
