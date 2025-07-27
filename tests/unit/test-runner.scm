#!/usr/bin/env guile
!#
;;; test-runner.scm - Main test runner for repo-topics tests

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 regex)
             (srfi srfi-1))

;;; Test framework
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *current-test* "")

(define (test-start name)
  (set! *current-test* name)
  (format #t "Testing ~a... " name))

(define (test-pass)
  (set! *tests-passed* (+ *tests-passed* 1))
  (format #t "✓~%"))

(define (test-fail reason)
  (set! *tests-failed* (+ *tests-failed* 1))
  (format #t "✗~%  Reason: ~a~%" reason))

(define (assert condition reason)
  (if condition
      #t
      (begin
        (test-fail reason)
        #f)))

(define (assert-equal actual expected)
  (assert (equal? actual expected)
          (format #f "Expected ~s but got ~s" expected actual)))

(define (assert-match pattern string)
  (assert (string-match pattern string)
          (format #f "String '~a' does not match pattern '~a'" string pattern)))

(define (run-command cmd)
  "Run a command and return (exit-code . output)"
  (let* ((port (open-pipe* OPEN_BOTH "/bin/sh" "-c" cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (cons (status:exit-val status) output)))

;;; Test utilities
(define (load-test-file filename)
  ;; Get the directory of this script
  (let* ((script-dir (dirname (car (command-line))))
         (path (string-append script-dir "/" filename)))
    (if (file-exists? path)
        (begin
          (format #t "~%Loading tests from ~a~%" filename)
          (load path))
        (format #t "Warning: Test file ~a not found~%" path))))

;;; Main test runner
(define (run-tests)
  (format #t "~%=== Running repo-topics tests ===~%~%")
  
  ;; Load all test files
  (load-test-file "test-validation.scm")
  (load-test-file "test-parsing.scm")
  (load-test-file "test-environment.scm")
  (load-test-file "test-integration.scm")
  
  ;; Print summary
  (format #t "~%=== Test Summary ===~%")
  (format #t "Passed: ~a~%" *tests-passed*)
  (format #t "Failed: ~a~%" *tests-failed*)
  (format #t "Total:  ~a~%" (+ *tests-passed* *tests-failed*))
  
  ;; Exit with appropriate code
  (exit (if (zero? *tests-failed*) 0 1)))

;;; Run tests if called directly
(if (string=? (car (command-line)) "test-runner.scm")
    (run-tests))