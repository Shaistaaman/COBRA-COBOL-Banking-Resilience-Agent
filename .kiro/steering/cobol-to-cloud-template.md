# COBOL-to-Cloud Spec Template

This document provides templates for creating Kiro spec documents when modernizing COBOL applications to AWS. Use these templates as starting points for your modernization projects.

## Template Structure

When creating a new COBOL modernization spec, create three documents:

1. **requirements.md**: Business requirements extracted from COBOL
2. **design.md**: AWS architecture design
3. **tasks.md**: Implementation tasks

## File Reference Syntax

To reference COBOL source files in your spec documents, use:

```markdown
#[[file:path/to/cobol-file.cbl]]
```

This allows Kiro to access the COBOL source code during implementation.

## Template Files

Complete templates are available in the `templates/spec-templates/` directory:

- **requirements-template.md**: #[[file:templates/spec-templates/requirements-template.md]]
- **design-template.md**: #[[file:templates/spec-templates/design-template.md]]
- **tasks-template.md**: #[[file:templates/spec-templates/tasks-template.md]]

## Usage Instructions

### Creating a New COBOL Modernization Spec

1. **Create Spec Directory**:

   ```bash
   mkdir -p .kiro/specs/[project-name]
   ```

2. **Copy Templates**:

   ```bash
   cp templates/spec-templates/requirements-template.md .kiro/specs/[project-name]/requirements.md
   cp templates/spec-templates/design-template.md .kiro/specs/[project-name]/design.md
   cp templates/spec-templates/tasks-template.md .kiro/specs/[project-name]/tasks.md
   ```

3. **Update File References**:

   - Replace `#[[file:path/to/source.cbl]]` with actual COBOL file path
   - Update all placeholder text in brackets `[like this]`
   - Fill in COBOL line number references

4. **Customize for Your Project**:
   - Add/remove requirements based on COBOL analysis
   - Adjust AWS services based on your architecture decisions
   - Modify tasks based on your implementation approach

### Using COBRA to Generate Specs

COBRA can automatically generate these spec documents from COBOL source:

```typescript
// Use the generateSpec MCP tool
const spec = await mcp.tools.generateSpec({
  cobolSource: fs.readFileSync('path/to/program.cbl', 'utf-8'),
  projectName: 'interest-calculation',
  approach: 'rewrite' // or 'wrapper', 'hybrid'
})
```

COBRA will:

1. Parse the COBOL source code
2. Analyze business logic and patterns
3. Generate requirements.md with EARS-compliant criteria
4. Create design.md with AWS architecture recommendations
5. Produce tasks.md with implementation steps

You can then review and customize the generated specs as needed.

## Template Customization Guidelines

### Requirements Document

- **Focus on Business Logic**: Extract the "what" from COBOL, not the "how"
- **Use EARS Patterns**: Ensure all acceptance criteria follow EARS syntax
- **Reference COBOL Source**: Always include line numbers and file references
- **Include Compliance**: Document regulatory requirements from COBOL comments

### Design Document

- **Map COBOL to AWS**: Show clear mapping from COBOL structures to AWS services
- **Justify Decisions**: Explain why specific AWS services were chosen
- **Include Diagrams**: Use Mermaid for architecture visualization
- **Plan for Migration**: Document the transition strategy

### Tasks Document

- **Incremental Steps**: Break down into small, testable tasks
- **Reference Requirements**: Link each task to specific requirements
- **Mark Optional Tasks**: Use `*` suffix for optional tasks (tests, docs)
- **Include COBOL References**: Help implementers find relevant COBOL code

## Best Practices

1. **Start with Analysis**: Use COBRA's `parseCobol` and `analyzeLogic` tools first
2. **Validate with SMEs**: Review generated specs with COBOL developers and business users
3. **Iterate**: Refine specs as you learn more during implementation
4. **Keep Updated**: Update specs when requirements or design changes
5. **Use as Living Docs**: Reference specs during code reviews and testing

## Example Projects

See the COBRA examples directory for complete spec examples:

- `examples/interest-calculation/`: Simple calculation modernization
- `examples/transaction-posting/`: Transaction processing with DynamoDB
- `examples/batch-reconciliation/`: Batch processing with Step Functions
