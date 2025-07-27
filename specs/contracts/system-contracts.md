# System Contracts and Validation Specification

## Overview

This document defines the contracts between `repo-topics` and external systems (Ollama, GitHub), including validation requirements and error handling for breaking changes or malformed responses.

## System Dependencies

### 1. Ollama API Contract

#### Endpoint: `http://localhost:11434/api/generate`

**Request Contract:**
```json
{
  "model": "string",
  "prompt": "string",
  "stream": false,
  "format": {
    "type": "object",
    "properties": {
      "topics": {
        "type": "array",
        "items": {"type": "string"},
        "minItems": 3,
        "maxItems": 10
      }
    },
    "required": ["topics"]
  }
}
```

**Response Contract:**
```json
{
  "model": "string",
  "created_at": "string",
  "response": "string",  // JSON string containing {"topics": [...]}
  "done": true
}
```

**Current Validation:**
- ✅ Checks if Ollama is running via `/api/tags` endpoint
- ✅ Validates response is valid JSON using `jq`
- ✅ Parses nested JSON response string
- ✅ Validates each topic with `valid-github-topic?`
- ⚠️  **No validation for response schema structure**
- ⚠️  **No handling for partial/streaming responses**

**Breaking Change Risks:**
1. Response format changes (e.g., `response` → `content`)
2. Nested JSON structure changes
3. Streaming enabled by default
4. Model-specific format differences

### 2. GitHub API Contract

#### Endpoint: `PUT /repos/{owner}/{repo}/topics`

**Request Contract:**
```json
{
  "names": ["topic1", "topic2", "..."]
}
```

**Response Contract:**
```json
{
  "names": ["topic1", "topic2", "..."]
}
```

**Current Validation:**
- ✅ Uses GitHub CLI (`gh`) which handles auth
- ✅ Checks exit code of `gh api` command
- ⚠️  **No response validation**
- ⚠️  **No error message parsing**
- ⚠️  **No rate limit handling**

**Breaking Change Risks:**
1. API endpoint changes
2. Authentication method changes
3. Rate limiting becomes stricter
4. Topic validation rules change

## Validation Gaps Analysis

### Current Validations

#### Topic Validation (`valid-github-topic?`)
```scheme
(define (valid-github-topic? topic)
  "Check if topic meets GitHub requirements"
  (and (string? topic)
       (> (string-length topic) 0)
       (<= (string-length topic) 50)
       (string-match "^[a-z0-9][a-z0-9-]*[a-z0-9]?$" topic)))
```

**Coverage:**
- ✅ Type checking (string)
- ✅ Length constraints (1-50 chars)
- ✅ Character set validation (lowercase, numbers, hyphens)
- ✅ Format rules (no leading/trailing hyphens)

#### JSON Validation (`valid-json?`)
```scheme
(define (valid-json? json-str)
  "Check if string is valid JSON"
  (let* ((temp-file (format #f "/tmp/json-check-~a.json" (getpid))))
    (call-with-output-file temp-file
      (lambda (port) (display json-str port)))
    (let ((result (zero? (system* "jq" "." temp-file))))
      (delete-file temp-file)
      result)))
```

**Coverage:**
- ✅ Syntax validation via `jq`
- ⚠️  No schema validation
- ⚠️  No type checking for expected fields

### Missing Validations

1. **Ollama Response Schema Validation**
   - No check for required fields (`model`, `response`, `done`)
   - No validation of nested topic structure
   - No handling of error responses

2. **GitHub API Error Handling**
   - No parsing of error messages
   - No distinction between auth/rate limit/validation errors
   - No retry logic for transient failures

3. **Network Error Handling**
   - Connection timeouts not handled
   - DNS failures not distinguished
   - No circuit breaker pattern

## Proposed Contract Enforcement

### 1. Ollama Response Validator

```scheme
(define (validate-ollama-response response)
  "Validate Ollama API response structure"
  (guard (ex (else (values #f (format #f "Invalid response: ~a" ex))))
    (let* ((parsed (json-string->scm response))
           (model (assoc-ref parsed "model"))
           (done (assoc-ref parsed "done"))
           (response-str (assoc-ref parsed "response")))
      (cond
        ((not model) (values #f "Missing 'model' field"))
        ((not done) (values #f "Missing 'done' field"))
        ((not response-str) (values #f "Missing 'response' field"))
        ((not (eq? done #t)) (values #f "Response not complete"))
        (else
          (let ((topics-data (json-string->scm response-str)))
            (if (assoc-ref topics-data "topics")
                (values #t topics-data)
                (values #f "Missing 'topics' in response"))))))))
```

### 2. GitHub API Response Validator

```scheme
(define (validate-github-response response expected-topics)
  "Validate GitHub API response matches request"
  (guard (ex (else (values #f "Invalid GitHub response")))
    (let* ((parsed (json-string->scm response))
           (returned-topics (assoc-ref parsed "names")))
      (cond
        ((not returned-topics) (values #f "Missing 'names' field"))
        ((not (equal? (sort returned-topics string<?)
                     (sort expected-topics string<?)))
         (values #f "Topics mismatch"))
        (else (values #t returned-topics))))))
```

### 3. Error Response Handler

```scheme
(define (handle-api-error response api-type)
  "Parse and categorize API errors"
  (guard (ex (else `((type . unknown) (message . ,response))))
    (let ((parsed (json-string->scm response)))
      (case api-type
        ((github)
         (let ((message (assoc-ref parsed "message"))
               (errors (assoc-ref parsed "errors")))
           `((type . ,(categorize-github-error message))
             (message . ,message)
             (errors . ,errors)
             (recoverable . ,(recoverable-error? message)))))
        ((ollama)
         (let ((error (assoc-ref parsed "error")))
           `((type . ,(categorize-ollama-error error))
             (message . ,error)
             (recoverable . #t))))))))

(define (categorize-github-error message)
  (cond
    ((string-contains message "rate limit") 'rate-limit)
    ((string-contains message "401") 'auth)
    ((string-contains message "validation") 'validation)
    (else 'unknown)))
```

## Resilience Patterns

### 1. Retry with Exponential Backoff

```scheme
(define (with-retry fn max-attempts initial-delay)
  "Execute function with exponential backoff retry"
  (let loop ((attempt 1) (delay initial-delay))
    (guard (ex 
            ((< attempt max-attempts)
             (format #t "Attempt ~a failed, retrying in ~as...~%" 
                     attempt delay)
             (sleep delay)
             (loop (+ attempt 1) (* delay 2)))
            (else (raise ex)))
      (fn))))
```

### 2. Circuit Breaker

```scheme
(define (make-circuit-breaker threshold reset-time)
  "Create a circuit breaker for API calls"
  (let ((failures 0)
        (last-failure-time 0)
        (state 'closed))
    (lambda (fn)
      (case state
        ((open)
         (if (> (- (current-time) last-failure-time) reset-time)
             (begin (set! state 'half-open)
                    (set! failures 0))
             (error "Circuit breaker open")))
        ((closed half-open)
         (guard (ex 
                 (else
                  (set! failures (+ failures 1))
                  (set! last-failure-time (current-time))
                  (when (>= failures threshold)
                    (set! state 'open))
                  (raise ex)))
           (let ((result (fn)))
             (when (eq? state 'half-open)
               (set! state 'closed)
               (set! failures 0))
             result)))))))
```

### 3. Timeout Wrapper

```scheme
(define (with-timeout fn timeout-seconds)
  "Execute function with timeout"
  (let ((result-box (make-atomic-box #f))
        (error-box (make-atomic-box #f)))
    (let ((thread (make-thread
                   (lambda ()
                     (guard (ex (else (atomic-box-set! error-box ex)))
                       (atomic-box-set! result-box (fn)))))))
      (thread-start! thread)
      (thread-join! thread timeout-seconds)
      (cond
        ((atomic-box-ref error-box) => raise)
        ((atomic-box-ref result-box))
        (else (error "Operation timed out"))))))
```

## Implementation Priority

1. **High Priority:**
   - Ollama response schema validation
   - GitHub error parsing
   - Basic retry logic

2. **Medium Priority:**
   - Circuit breaker for Ollama
   - Timeout handling
   - Detailed error categorization

3. **Low Priority:**
   - Response caching
   - Alternative API endpoints
   - Fallback strategies

## Testing Strategy

### Contract Tests
```scheme
;; Test malformed Ollama responses
(test-malformed-ollama-response
  '(("{}")                           ; Empty response
    ("{\"model\":\"test\"}")         ; Missing required fields  
    ("{\"response\":\"not-json\"}")  ; Invalid nested JSON
    ("{\"response\":\"{}\"}"))       ; Missing topics array

;; Test GitHub API errors
(test-github-error-responses
  '(("{\"message\":\"Bad credentials\"}")
    ("{\"message\":\"API rate limit exceeded\"}")
    ("{\"message\":\"Validation Failed\",\"errors\":[]}"))
```

### Integration Tests
- Mock Ollama server with various response formats
- Mock GitHub API with error conditions
- Network failure simulation
- Timeout testing

## Monitoring and Alerting

### Metrics to Track
1. API response times
2. Validation failure rates
3. Retry counts
4. Circuit breaker trips
5. Schema mismatch frequency

### Logging Requirements
```scheme
(define (log-validation-failure api-type expected actual)
  (format (current-error-port) 
          "[~a] Validation failed for ~a~%Expected: ~a~%Actual: ~a~%"
          (current-time-string) api-type expected actual))
```

## Migration Path

1. **Phase 1:** Add response validation without breaking changes
2. **Phase 2:** Add retry logic and improve error messages
3. **Phase 3:** Implement circuit breakers and advanced resilience
4. **Phase 4:** Add comprehensive monitoring and alerting