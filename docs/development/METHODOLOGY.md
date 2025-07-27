# Experimental Engineering Methodology

This document describes the methodology used to build Ollama Topic Forge - an approach we call **Experimental Engineering**.

## Overview

Traditional software development follows predetermined architectures. Experimental Engineering discovers architecture through controlled experiments, letting real-world constraints shape the final design.

## The Process

### Phase 1: Feasibility (Experiments 01-03)
**Goal**: Validate core assumptions

- **01-ollama-structured-output**: Verify Ollama can generate structured JSON
- **02-ollama-structured-scheme**: Test Guile Scheme integration
- **03-spec-json-conversion**: Explore contract definition approaches

**Key Learning**: LLMs follow schemas imperfectly; validation is essential

### Phase 2: Integration (Experiments 04-06)
**Goal**: Solve API integration challenges

- **04-github-api-integration**: GitHub API client implementation
- **05-spec-validation-framework**: Unified validation system
- **06-api-composition**: Workflow orchestration

**Key Learning**: Leverage existing tools (GitHub CLI) over custom implementations

### Phase 3: Robustness (Experiments 07-09)
**Goal**: Handle edge cases and failures

- **07-repo-topic-simulator**: Quality prediction before API calls
- **08-json-parsing-scheme**: **CRITICAL FAILURE** - Regex JSON parsing
- **09-json-schema-validation**: Runtime contract enforcement

**Key Learning**: Use proper tools (`jq`) - failed experiment saved weeks of debugging

## Failed Experiments as Success

**Experiment 08** categorically failed when attempting regex-based JSON parsing. This failure was invaluable:

1. **Immediate pivot** to external tools
2. **Architectural simplification** 
3. **Prevented production bugs**
4. **Informed tool selection** (`jq` over custom parsing)

## Contracts Emerge from Experiments

Rather than designing contracts upfront, they emerged from observed behavior:

```scheme
;; Discovered through experimentation
(define-contract ollama-response
  '((model . string)
    (done . boolean)
    (response . string))) ; Contains nested JSON

;; Validated through testing  
(define-contract github-topic
  '((name . (string-matching "^[a-z0-9][a-z0-9-]*[a-z0-9]?$"))
    (length . (range 1 50))))
```

## Production Synthesis

The final tool (`ollama-topic-forge`) synthesizes all learnings:

1. **Use `jq` for JSON parsing** (from failed experiment 08)
2. **Validate at every boundary** (from experiment 05)
3. **Retry with backoff** (from experiment 04)
4. **Leverage existing tools** (from experiment 04)
5. **Simple formats when possible** (from experiment 02)

## Benefits of This Approach

1. **Discovers real constraints** vs imagined ones
2. **Validates assumptions early** before major investment
3. **Preserves failure knowledge** for future decisions
4. **Creates robust systems** through observed behavior
5. **Enables rapid pivots** when experiments fail

## When to Use Experimental Engineering

✅ **Good for:**
- Unknown problem domains
- External API integrations
- AI/ML system integration
- New technology adoption
- Uncertain requirements

❌ **Not suitable for:**
- Well-understood problems
- Regulated environments requiring upfront design
- Systems with fixed architectural constraints
- Time-critical projects

## Experiment Structure

Each experiment follows a standard pattern:

```
experiments/NNN-descriptive-name/
├── README.md          # Hypothesis and method
├── Makefile          # Standard targets: run, test, clean
├── FINDINGS.md       # Results and learnings
├── src/              # Experimental code
└── output/           # Generated artifacts
```

## From Experiments to Contracts

1. **Observe behavior** in experiments
2. **Extract patterns** from successful approaches
3. **Formalize contracts** based on real constraints
4. **Validate contracts** in subsequent experiments
5. **Evolve contracts** as understanding deepens

## Metrics and Success Criteria

- **Experiment velocity**: 1-2 days per experiment
- **Failure tolerance**: 30-40% of experiments expected to fail
- **Knowledge preservation**: All experiments documented
- **Architecture emergence**: No upfront design decisions
- **Contract evolution**: Specifications updated with each phase

## Tools and Infrastructure

- **Experiment framework**: Standardized Makefiles
- **Documentation**: README + FINDINGS for each experiment
- **Version control**: Each experiment is a commit
- **Artifact preservation**: Generated code and data saved
- **Contract validation**: Automated testing of specifications

## Lessons for Future Projects

1. **Start with smallest possible experiments**
2. **Document failures as thoroughly as successes**
3. **Preserve experimental code** - it teaches
4. **Let architecture emerge** - don't force patterns
5. **Formalize late** - contracts after behavior is understood

This methodology produced a production-ready CLI tool through 13 small experiments, with each failure contributing to the final robust architecture.

## References

- Full RFC: [Experimental Engineering Methodology](rfc-experimental-engineering.md)
- Experiment Archive: [experiments/](experiments/)
- System Contracts: [../specs/contracts/](../specs/contracts/)
- Test Results: [../../tests/](../../tests/)