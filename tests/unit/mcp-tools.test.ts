/**
 * Unit tests for MCP tools
 */

import { describe, it, expect } from 'vitest'
import {
  parseCobol,
  analyzeLogic,
  generateSpec,
  suggestModernization
} from '../../src/mcp-server/tools/index.js'

describe('MCP Tools', () => {
  const sampleCobol = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST-CALC.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-PRINCIPAL PIC 9(7)V99.
       01 WS-RATE PIC 9V9999.
       01 WS-INTEREST PIC 9(7)V99.
       
       PROCEDURE DIVISION.
           COMPUTE WS-INTEREST = WS-PRINCIPAL * WS-RATE.
           DISPLAY "INTEREST: " WS-INTEREST.
           STOP RUN.
  `

  describe('parseCobol', () => {
    it('should parse valid COBOL source', async () => {
      const result = await parseCobol(sampleCobol)

      expect(result).toBeDefined()
      expect(result.metadata.programName).toBe('INTEREST-CALC')
      expect(result.metadata.lineCount).toBeGreaterThan(0)
      expect(result.errors).toHaveLength(0)
    })

    it('should handle empty source', async () => {
      const result = await parseCobol('')

      expect(result).toBeDefined()
      expect(result.errors.length).toBeGreaterThan(0)
      expect(result.ast).toBeNull()
    })

    it('should include complexity metrics', async () => {
      const result = await parseCobol(sampleCobol)

      expect(result.metadata.complexity).toBeGreaterThan(0)
    })
  })

  describe('analyzeLogic', () => {
    it('should analyze AST and extract business rules', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)

      expect(analysis).toBeDefined()
      expect(analysis.businessRules).toBeDefined()
      expect(analysis.patterns).toBeDefined()
      expect(analysis.dataStructures).toBeDefined()
      expect(analysis.dependencies).toBeDefined()
      expect(analysis.entryPoints).toBeDefined()
    })

    it('should include confidence scores for patterns', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)

      // Check that patterns have confidence scores
      analysis.patterns.forEach(pattern => {
        expect(pattern.confidence).toBeGreaterThan(0)
        expect(pattern.confidence).toBeLessThanOrEqual(1)
      })
    })

    it('should handle invalid AST', async () => {
      await expect(analyzeLogic(null as any)).rejects.toThrow()
    })
  })

  describe('generateSpec', () => {
    it('should generate spec documents from analysis', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const spec = await generateSpec(analysis)

      expect(spec).toBeDefined()
      expect(spec.requirements).toContain('Requirements Document')
      expect(spec.design).toContain('Design Document')
      expect(spec.tasks).toContain('Implementation Plan')
    })

    it('should include EARS-compliant requirements', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const spec = await generateSpec(analysis)

      // Check for EARS patterns (WHEN, THE, SHALL)
      expect(spec.requirements).toMatch(/WHEN.*THE.*SHALL/i)
    })

    it('should include AWS architecture in design', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const spec = await generateSpec(analysis)

      expect(spec.design).toContain('Lambda')
      expect(spec.design).toContain('API Gateway')
    })
  })

  describe('suggestModernization', () => {
    it('should provide modernization recommendations', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const plan = await suggestModernization(analysis)

      expect(plan).toBeDefined()
      expect(plan.recommendations).toBeDefined()
      expect(plan.prioritizedModules).toBeDefined()
      expect(plan.deRiskingStrategies).toBeDefined()
    })

    it('should prioritize modules by coupling and complexity', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const plan = await suggestModernization(analysis)

      expect(plan.prioritizedModules.length).toBeGreaterThan(0)

      const module = plan.prioritizedModules[0]
      expect(module.name).toBeDefined()
      expect(module.priority).toBeGreaterThan(0)
      expect(module.coupling).toBeGreaterThanOrEqual(0)
      expect(module.complexity).toBeGreaterThan(0)
    })

    it('should suggest appropriate AWS services', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const plan = await suggestModernization(analysis)

      expect(plan.recommendations.length).toBeGreaterThan(0)

      const recommendation = plan.recommendations[0]
      expect(recommendation.awsService).toBeDefined()
      expect(recommendation.rationale).toBeDefined()
      expect(recommendation.estimatedEffort).toBeDefined()
    })

    it('should provide de-risking strategies', async () => {
      const parseResult = await parseCobol(sampleCobol)

      if (!parseResult.ast) {
        throw new Error('Failed to parse COBOL')
      }

      const analysis = await analyzeLogic(parseResult.ast)
      const plan = await suggestModernization(analysis)

      expect(plan.deRiskingStrategies.length).toBeGreaterThan(0)

      const strategy = plan.deRiskingStrategies[0]
      expect(strategy.approach).toBeDefined()
      expect(strategy.description).toBeDefined()
      expect(strategy.risks).toBeDefined()
      expect(strategy.mitigations).toBeDefined()
    })
  })
})
