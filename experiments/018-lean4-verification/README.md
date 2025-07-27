# Experiment 018: LEAN4 Verification Opportunities

## Overview

This experiment explores components of ollama-topic-forge that could benefit from LEAN4's theorem proving capabilities, focusing on areas where mathematical verification would add value.

## Components Suitable for LEAN4 Verification

### 1. JSON Parser Correctness

The custom JSON parser (from failed regex experiment) could be formally verified:

```lean4
-- LEAN4 specification for JSON parsing
inductive JsonValue where
  | null : JsonValue
  | bool : Bool → JsonValue
  | number : Float → JsonValue
  | string : String → JsonValue
  | array : List JsonValue → JsonValue
  | object : List (String × JsonValue) → JsonValue

def parseJson : String → Option JsonValue := sorry

theorem parse_inverse : ∀ (j : JsonValue), 
  parseJson (toString j) = some j := by sorry
```

### 2. Topic Validation Rules

GitHub topic constraints could be formally specified:

```lean4
def validTopicChar (c : Char) : Bool :=
  c.isAlphaNum || c == '-'

def validTopic (s : String) : Prop :=
  1 ≤ s.length ∧ s.length ≤ 50 ∧
  s.all validTopicChar ∧
  s.front.isAlphaNum ∧
  (s.length > 1 → s.back.isAlphaNum)

theorem topic_length_bounds : ∀ (t : String),
  validTopic t → 1 ≤ t.length ∧ t.length ≤ 50 := by
  intro t h
  exact ⟨h.1, h.2.1⟩
```

### 3. Retry Logic Correctness

Exponential backoff with bounded retries:

```lean4
def backoffDelay (attempt : Nat) : Nat :=
  min (2^attempt * 1000) 60000  -- ms, capped at 60s

theorem backoff_bounded : ∀ (n : Nat),
  backoffDelay n ≤ 60000 := by
  intro n
  unfold backoffDelay
  apply min_le_right
```

### 4. Contract Refinement

Prove implementation matches specification:

```lean4
-- High-level spec
def topicGenSpec (repo : Repository) : List Topic := sorry

-- Implementation
def topicGenImpl (repo : Repository) : IO (List Topic) := sorry

-- Refinement theorem
theorem impl_refines_spec : ∀ (repo : Repository),
  topicGenImpl repo = pure (topicGenSpec repo) := sorry
```

## Benefits of LEAN4 Verification

1. **Mathematical Certainty**: Proofs guarantee correctness
2. **Edge Case Discovery**: Proof attempts reveal missing cases
3. **Documentation**: Types and theorems document intent
4. **Refactoring Safety**: Proofs ensure behavior preservation

## Practical Application

### JSON Parsing Verification
Most valuable for the JSON parsing component:
- Prove parser handles all valid JSON
- Prove parser rejects all invalid JSON
- Prove round-trip property (parse ∘ serialize = id)

### Topic Validation
Verify GitHub's undocumented rules:
- Character set constraints
- Length boundaries
- Special case handling

## Implementation Sketch

```lean4
-- Complete topic generation verification
structure TopicGenState where
  repo : Repository
  retries : Nat
  topics : List Topic
  error : Option Error

inductive TopicGenM : Type → Type where
  | pure : α → TopicGenM α
  | bind : TopicGenM α → (α → TopicGenM β) → TopicGenM β
  | readRepo : TopicGenM Repository
  | callOllama : String → TopicGenM (Option JsonValue)
  | validateTopics : List String → TopicGenM (List Topic)
  | updateGitHub : List Topic → TopicGenM Unit
  | retry : TopicGenM α → TopicGenM α

-- Monad instance
instance : Monad TopicGenM where
  pure := TopicGenM.pure
  bind := TopicGenM.bind

-- Main theorem: generation terminates
theorem generation_terminates : ∀ (repo : Repository),
  ∃ (result : List Topic ⊕ Error), 
    runTopicGen repo = result := sorry
```

## Future Work

1. **Port JSON parser to LEAN4**: Verify correctness
2. **Formalize GitHub API**: Model rate limits and pagination
3. **Verify workflow properties**: Deadlock freedom, progress
4. **Performance bounds**: Prove complexity bounds

## Conclusion

LEAN4 verification would be most valuable for:
1. JSON parsing correctness (critical component)
2. Topic validation rules (GitHub compatibility)
3. Retry logic bounds (resource usage)
4. Overall workflow termination (liveness)

The investment in formal verification pays off for core algorithms that are difficult to test exhaustively.