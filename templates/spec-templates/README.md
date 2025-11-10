# COBOL-to-Cloud Spec Templates

This directory contains templates for creating Kiro spec documents when modernizing COBOL applications to AWS.

## Templates

- **requirements-template.md**: Template for documenting business requirements extracted from COBOL
- **design-template.md**: Template for AWS architecture design
- **tasks-template.md**: Template for implementation task breakdown

## Usage

### Manual Creation

1. Copy templates to your spec directory:

   ```bash
   mkdir -p .kiro/specs/[project-name]
   cp templates/spec-templates/*.md .kiro/specs/[project-name]/
   ```

2. Rename files (remove `-template` suffix):

   ```bash
   cd .kiro/specs/[project-name]
   mv requirements-template.md requirements.md
   mv design-template.md design.md
   mv tasks-template.md tasks.md
   ```

3. Update placeholders:
   - Replace `[bracketed text]` with actual values
   - Update `#[[file:path/to/source.cbl]]` with real COBOL file paths
   - Fill in COBOL line number references
   - Customize AWS services based on your needs

### Automated Generation with COBRA

Use COBRA's `generateSpec` MCP tool to automatically generate specs from COBOL:

```typescript
import { MCPClient } from '@modelcontextprotocol/sdk'

const spec = await mcp.tools.generateSpec({
  cobolSource: cobolCode,
  projectName: 'my-modernization',
  approach: 'rewrite' // or 'wrapper', 'hybrid'
})
```

## Template Structure

### requirements-template.md

- Introduction and glossary
- User stories with EARS-compliant acceptance criteria
- COBOL source references
- Non-functional requirements
- Migration constraints

### design-template.md

- Architecture overview with Mermaid diagrams
- Component specifications
- COBOL to AWS data mapping
- Error handling strategy
- Security and compliance design
- Testing approach
- Cost estimation

### tasks-template.md

- Numbered task list with subtasks
- COBOL source references for each task
- Requirement traceability
- Optional tasks marked with `*`
- Success criteria

## Customization Guidelines

### For Different Modernization Approaches

**API Wrapper (Strangler Pattern)**:

- Focus on integration layer tasks (Amazon MQ, Lambda wrappers)
- Keep COBOL references for calling mainframe
- Add tasks for gradual logic migration

**Full Rewrite**:

- Focus on business logic translation tasks
- Add comprehensive testing tasks
- Include parallel run validation

**Hybrid Architecture**:

- Balance between wrapper and rewrite tasks
- Add data synchronization tasks
- Include mainframe integration testing

### For Different COBOL Patterns

**Batch Processing**:

- Add Step Functions workflow tasks
- Include S3 file processing tasks
- Add reconciliation and reporting tasks

**Online Transaction Processing**:

- Focus on API Gateway and Lambda tasks
- Add DynamoDB transaction tasks
- Include real-time monitoring tasks

**Data-Intensive Applications**:

- Add data migration tasks (DMS)
- Include database optimization tasks
- Add data validation tasks

## Best Practices

1. **Always Reference COBOL Source**: Use `#[[file:...]]` syntax so Kiro can access the code
2. **Be Specific**: Replace all placeholders with concrete values
3. **Link Requirements to Tasks**: Ensure traceability
4. **Mark Optional Work**: Use `*` for optional tasks to focus on MVP
5. **Update as You Learn**: Specs are living documents

## Examples

See complete examples in the COBRA project:

- Interest calculation modernization
- Transaction posting with DynamoDB
- Batch reconciliation with Step Functions

## Support

For questions or issues with templates:

- Review the COBRA documentation
- Check the banking-patterns.md steering document
- Consult the modernization-strategy.md steering document
