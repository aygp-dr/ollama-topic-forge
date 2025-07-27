#!/usr/bin/env guile
!#
;;; test-with-samples.scm - Test JSON parser with sample files

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 ftw)
             (ice-9 rdelim)
             (srfi srfi-1))

;;; Define read-string if not available
(define (read-string port)
  "Read entire contents of a port as a string"
  (let loop ((chars '()) (ch (read-char port)))
    (if (eof-object? ch)
        (list->string (reverse chars))
        (loop (cons ch chars) (read-char port)))))

;;; Include parser functions directly
(load "custom-json-parser.scm")

(format #t "=== Testing JSON Parser with Sample Files ===~%~%")

;;; Read and test each sample file
(define samples-dir "samples")

(define (test-sample-file filepath)
  "Test parsing a sample JSON file"
  (format #t "Testing: ~a~%" filepath)
  (catch #t
    (lambda ()
      (let* ((content (call-with-input-file filepath read-string))
             (topics (parse-ollama-topics-response content)))
        (if topics
            (begin
              (format #t "  ✓ Parsed successfully~%")
              (format #t "  Topics: ~a~%" topics)
              (format #t "  Count: ~a~%" (length topics))
              (for-each (lambda (topic)
                         (if (valid-github-topic? topic)
                             (format #t "    ✓ Valid: ~a~%" topic)
                             (format #t "    ✗ Invalid: ~a~%" topic)))
                       topics))
            (format #t "  ✗ Failed to parse topics~%"))))
    (lambda (key . args)
      (format #t "  ✗ Error: ~a~%" args)))
  (newline))

(define (valid-github-topic? topic)
  "Check if topic is valid for GitHub"
  (and (string? topic)
       (<= 1 (string-length topic) 50)
       (string-match "^[a-z0-9][a-z0-9-]*[a-z0-9]?$" topic)))

;;; Process all sample files
(let ((sample-files (scandir samples-dir 
                            (lambda (file) 
                              (string-suffix? ".json" file)))))
  (if sample-files
      (for-each 
       (lambda (file)
         (test-sample-file (string-append samples-dir "/" file)))
       (sort sample-files string<?))
      (format #t "No sample files found in ~a/~%" samples-dir)))

;;; Additional inline tests
(format #t "=== Inline Test Cases ===~%~%")

(define test-cases
  '(("Simple topics" 
     . "{\"model\":\"llama3.2:3b\",\"response\":\"{\\\"topics\\\": [\\\"scheme\\\", \\\"lisp\\\"]}\",\"done\":true}")
    ("Topics with special chars"
     . "{\"model\":\"llama3.2:3b\",\"response\":\"{\\\"topics\\\": [\\\"c++\\\", \\\"objective-c\\\", \\\".net\\\"]}\",\"done\":true}")
    ("Empty topics array"
     . "{\"model\":\"llama3.2:3b\",\"response\":\"{\\\"topics\\\": []}\",\"done\":true}")
    ("Malformed JSON"
     . "{\"model\":\"llama3.2:3b\",\"response\":\"not json\",\"done\":true}")))

(for-each
 (lambda (test)
   (format #t "Test: ~a~%" (car test))
   (let ((topics (parse-ollama-topics-response (cdr test))))
     (format #t "  Result: ~a~%" (or topics "Failed"))
     (newline)))
 test-cases)

(define (string-suffix? suffix str)
  "Check if string ends with suffix"
  (let ((slen (string-length str))
        (suflen (string-length suffix)))
    (and (>= slen suflen)
         (string=? suffix (substring str (- slen suflen))))))