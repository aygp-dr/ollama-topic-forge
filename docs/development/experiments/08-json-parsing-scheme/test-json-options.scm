#!/usr/bin/env guile
!#
;;; test-json-options.scm - Test various JSON parsing approaches in Guile

(use-modules (ice-9 format)
             (ice-9 regex)
             (srfi srfi-1))

;;; Test data
(define test-json-simple "{\"name\": \"test\", \"count\": 42}")
(define test-json-array "{\"topics\": [\"scheme\", \"ollama\", \"github-api\"]}")
(define test-json-escaped "{\"response\": \"{\\\"topics\\\": [\\\"scheme\\\", \\\"ollama\\\"]}\"}")
(define test-ollama-response "{\"model\":\"llama3.2:3b\",\"created_at\":\"2025-07-25T21:53:28.96684Z\",\"response\":\"{\\\"topics\\\": [\\\"lisp\\\", \\\"functional-programming\\\", \\\"project-purpose/dynamic-repl\\\", \\\"key-technologies/llm\\\", \\\"project-type/tool\\\"]}\"}")

(format #t "=== Guile JSON Parsing Options Test ===~%~%")

;;; Option 1: Check for built-in JSON module
(format #t "Option 1: Checking for built-in JSON module...~%")
(catch #t
  (lambda ()
    (use-modules (json))
    (format #t "✓ Found (json) module!~%")
    (format #t "  Testing: ~a~%" (json-string->scm test-json-simple))
    #t)
  (lambda (key . args)
    (format #t "✗ No (json) module found~%")
    #f))

(newline)

;;; Option 2: Check for ice-9 json
(format #t "Option 2: Checking for (ice-9 json)...~%")
(catch #t
  (lambda ()
    (use-modules (ice-9 json))
    (format #t "✓ Found (ice-9 json) module!~%")
    #t)
  (lambda (key . args)
    (format #t "✗ No (ice-9 json) module found~%")
    #f))

(newline)

;;; Option 3: Check for guile-json
(format #t "Option 3: Checking for guile-json...~%")
(catch #t
  (lambda ()
    ;; Try to load it
    (use-modules (json))
    (format #t "✓ Found guile-json!~%")
    ;; Test parsing
    (let ((parsed (json-string->scm test-json-simple)))
      (format #t "  Parsed simple: ~a~%" parsed))
    (let ((parsed (json-string->scm test-json-array)))
      (format #t "  Parsed array: ~a~%" parsed))
    #t)
  (lambda (key . args)
    (format #t "✗ guile-json not available~%")
    #f))

(newline)

;;; Option 4: Check SRFI-180
(format #t "Option 4: Checking for SRFI-180 (JSON)...~%")
(catch #t
  (lambda ()
    (use-modules (srfi srfi-180))
    (format #t "✓ Found SRFI-180!~%")
    #t)
  (lambda (key . args)
    (format #t "✗ No SRFI-180 support~%")
    #f))

(newline)

;;; Option 5: Simple custom parser for our specific case
(format #t "Option 5: Custom minimal parser...~%")

(define (extract-json-array str key)
  "Extract array value for a specific key from JSON string"
  (let* ((pattern (format #f "\"~a\"\\s*:\\s*\\[([^\\]]+)\\]" key))
         (match (string-match pattern str)))
    (if match
        (let ((array-content (match:substring match 1)))
          ;; Split by comma and clean each item
          (map (lambda (item)
                 (let ((trimmed (string-trim-both item)))
                   ;; Remove quotes
                   (if (and (> (string-length trimmed) 1)
                           (char=? (string-ref trimmed 0) #\")
                           (char=? (string-ref trimmed (- (string-length trimmed) 1)) #\"))
                       (substring trimmed 1 (- (string-length trimmed) 1))
                       trimmed)))
               (string-split array-content #\,)))
        #f)))

(define (extract-nested-json-array response)
  "Extract array from nested JSON response"
  ;; First extract the inner JSON string
  (let ((response-match (string-match "\"response\"\\s*:\\s*\"([^\"]+(?:\\\\\"[^\"]+)*)\"" response)))
    (if response-match
        (let* ((inner-json (match:substring response-match 1))
               ;; Unescape the inner JSON
               (unescaped (regexp-substitute/global #f "\\\\\"" inner-json 'pre "\"" 'post)))
          (extract-json-array unescaped "topics"))
        #f)))

(define (string-trim-both str)
  (let ((len (string-length str)))
    (if (= len 0)
        str
        (let* ((start (let loop ((i 0))
                       (cond
                         ((>= i len) len)
                         ((char-whitespace? (string-ref str i)) (loop (+ i 1)))
                         (else i))))
               (end (let loop ((i (- len 1)))
                     (cond
                       ((< i start) start)
                       ((char-whitespace? (string-ref str i)) (loop (- i 1)))
                       (else (+ i 1))))))
          (substring str start end)))))

;; Test custom parser
(format #t "Testing custom parser:~%")
(let ((result1 (extract-json-array test-json-array "topics")))
  (format #t "  Simple array: ~a~%" result1))
(let ((result2 (extract-nested-json-array test-ollama-response)))
  (format #t "  Nested Ollama response: ~a~%" result2))

(newline)

;;; Option 6: Shell out to jq if available
(format #t "Option 6: Checking for jq command...~%")
(if (zero? (system* "which" "jq"))
    (begin
      (format #t "✓ jq is available~%")
      (format #t "  Could use: echo '<json>' | jq -r '.response | fromjson | .topics[]'~%"))
    (format #t "✗ jq not found~%"))

(newline)
(format #t "=== Recommendation ===~%")
(format #t "Based on availability, use:~%")
(format #t "1. guile-json if installed (most robust)~%")
(format #t "2. Custom parser for simple cases (no dependencies)~%")
(format #t "3. Shell out to jq for complex parsing (if available)~%")