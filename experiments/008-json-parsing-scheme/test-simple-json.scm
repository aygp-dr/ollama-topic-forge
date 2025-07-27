#!/usr/bin/env guile
!#
;;; test-simple-json.scm - Debug JSON parsing

(use-modules (ice-9 format)
             (ice-9 regex))

;;; Test simple regex matching
(format #t "=== Simple JSON Parsing Tests ===~%~%")

;; Test 1: Match simple array
(define test1 "{\"topics\": [\"scheme\", \"lisp\"]}")
(format #t "Test 1: ~a~%" test1)
(let ((match (string-match "\"topics\"\\s*:\\s*\\[([^\\]]+)\\]" test1)))
  (if match
      (format #t "  Match: ~a~%" (match:substring match 1))
      (format #t "  No match~%")))

;; Test 2: Match escaped JSON
(define test2 "{\"response\": \"{\\\"topics\\\": [\\\"scheme\\\", \\\"lisp\\\"]}\"}")
(format #t "~%Test 2: ~a~%" test2)
(let ((match (string-match "\"response\"\\s*:\\s*\"([^\"]+)\"" test2)))
  (if match
      (begin
        (format #t "  Response field: ~a~%" (match:substring match 1))
        ;; Now unescape
        (let* ((content (match:substring match 1))
               (unescaped (regexp-substitute/global #f "\\\\\"" content 'pre "\"" 'post)))
          (format #t "  Unescaped: ~a~%" unescaped)
          ;; Try to extract topics
          (let ((topics-match (string-match "\"topics\"\\s*:\\s*\\[([^\\]]+)\\]" unescaped)))
            (if topics-match
                (format #t "  Topics: ~a~%" (match:substring topics-match 1))
                (format #t "  No topics match~%")))))
      (format #t "  No response match~%")))

;; Test 3: Real Ollama response
(define test3 "{\"model\":\"llama3.2:3b\",\"response\":\"{\\\"topics\\\": [\\\"lisp\\\", \\\"scheme\\\"]}\",\"done\":true}")
(format #t "~%Test 3 (Ollama): ~a~%" test3)
(let ((match (string-match "\"response\"\\s*:\\s*\"([^\"]+)\"" test3)))
  (if match
      (format #t "  Response extracted: ~a~%" (match:substring match 1))
      (format #t "  No match~%")))