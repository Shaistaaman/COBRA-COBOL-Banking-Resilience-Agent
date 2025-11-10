# COBOL Parser Module

This module provides comprehensive COBOL parsing capabilities for the COBRA system.

## Features

### 1. COBOL Parser (`cobol-parser.ts`)

- **Multi-dialect support**: IBM, Micro Focus, GnuCOBOL, COBOL-85, COBOL-2002
- **AST Generation**: Converts COBOL source into structured Abstract Syntax Trees
- **Error Handling**: Graceful degradation with detailed diagnostic messages
- **Source Mapping**: Preserves line number information for error reporting
- **Performance**: Parses 10,000 line files within 30 seconds (requirement 1.1)

#### Supported COBOL Divisions

- **IDENTIFICATION DIVISION**: Program ID, author, date written
- **ENVIRONMENT DIVISION**: Configuration and I/O settings
- **DATA DIVISION**: Working storage, file section, data structures
- **PROCEDURE DIVISION**: Statements and business logic

#### Usage

```typescript
import { createParser } from './parser'

const parser = createParser({ dialect: 'IBM' })
const result = await parser.parse(cobolSource)

console.log('Program:', result.metadata.programName)
console.log('Complexity:', result.metadata.complexity)
console.log('Errors:', result.errors)
```

### 2. Copybook Extractor (`copybook-extractor.ts`)

- **Copybook Detection**: Identifies COPY statements in COBOL source
- **Content Resolution**: Resolves copybook references with actual content
- **Data Structure Extraction**: Parses copybook data structures
- **Validation**: Validates copybook syntax and structure

#### Usage

```typescript
import { extractCopybooks, resolveCopybooks } from './parser'

// Extract copybook references
const copybooks = extractCopybooks(cobolSource)

// Resolve copybooks with content
const copybookMap = new Map([
  ['CUSTOMER-REC', customerRecordContent],
  ['ACCOUNT-REC', accountRecordContent]
])
const resolvedSource = resolveCopybooks(cobolSource, copybookMap)
```

### 3. Data Structure Parser (`data-structure-parser.ts`)

- **PIC Clause Conversion**: Converts COBOL PIC clauses to TypeScript types
- **TypeScript Interface Generation**: Creates TypeScript interfaces from COBOL data structures
- **Nested Structure Handling**: Supports 01-49 level numbers with proper hierarchy
- **File Definition Extraction**: Parses FD statements and file organizations
- **Database Schema Extraction**: Extracts schema from EXEC SQL statements

#### Supported PIC Patterns

| COBOL PIC | TypeScript Type | Description        |
| --------- | --------------- | ------------------ |
| `X(n)`    | `string`        | Alphanumeric field |
| `9(n)`    | `number`        | Numeric field      |
| `9(n)V99` | `number`        | Decimal number     |
| `S9(n)`   | `number`        | Signed numeric     |
| `A(n)`    | `string`        | Alphabetic field   |
| `COMP`    | `number`        | Binary             |
| `COMP-3`  | `number`        | Packed decimal     |

#### Usage

```typescript
import { picToTypeScript, generateTypeScriptInterface } from './parser'

// Convert PIC clause
const tsType = picToTypeScript('9(13)V99')
console.log(tsType.tsType) // 'number'
console.log(tsType.description) // 'Decimal number, 13 digits, 2 decimal places'

// Generate TypeScript interface
const dataElements = [
  { name: 'ACCOUNT-NUMBER', level: 5, picture: '9(10)' },
  { name: 'ACCOUNT-BALANCE', level: 5, picture: '9(13)V99' }
]
const tsInterface = generateTypeScriptInterface(dataElements, 'AccountRecord')
console.log(tsInterface.code)
```

## Implementation Details

### Error Handling Strategy

The parser implements graceful degradation:

1. **Syntax Errors**: Captured with line/column information
2. **Partial Parsing**: Returns partial AST for valid sections
3. **Warnings**: Non-fatal issues logged as warnings
4. **Validation**: Pre-parse validation available via `validate()` method

### Performance Characteristics

- **Parse Time**: < 30 seconds for 10,000 line files
- **Memory**: Efficient AST representation
- **Complexity Calculation**: Based on data elements and statements

### Dialect Support

The parser handles common COBOL dialects:

- **IBM Enterprise COBOL**: Full support
- **Micro Focus COBOL**: Full support
- **COBOL-85/2002 Standards**: Full support
- **Banking Extensions**: CICS, DB2, IMS (basic support)

## Testing

A test script is provided to verify parser functionality:

```bash
npx tsx src/parser/test-parser.ts
```

This tests:

- COBOL parsing with sample banking program
- AST generation and structure
- Copybook extraction
- PIC to TypeScript conversion
- Interface generation

## Requirements Satisfied

- ✅ **1.1**: Parse COBOL files within 30 seconds for 10,000 lines
- ✅ **1.4**: Extract data structures, copybooks, file definitions
- ✅ **1.5**: Provide diagnostic messages for syntax errors

## Future Enhancements

- Integration with tree-sitter-cobol for syntax highlighting
- Support for additional COBOL dialects (ACUCOBOL, Visual COBOL)
- Enhanced CICS/DB2/IMS statement parsing
- Performance optimization for very large files (>50,000 lines)
