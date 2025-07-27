#!/usr/bin/env guile3
!#
;; Interactive debugging example - simplified version

(use-modules (ice-9 format))

(define (factorial n)
  "Calculate factorial with debug output"
  (format #t "DEBUG: factorial(~a) called~%" n)
  (if (<= n 1)
      (begin
        (format #t "DEBUG: factorial(~a) = 1 (base case)~%" n)
        1)
      (let ((sub-result (factorial (- n 1))))
        (let ((result (* n sub-result)))
          (format #t "DEBUG: factorial(~a) = ~a * ~a = ~a~%" 
                  n n sub-result result)
          result))))

;; Test with a small number
(display "Running factorial with debug output:\n")
(define result (factorial 5))
(format #t "\nFinal result: ~a~%" result)

;; Demonstrate error handling with debugging
(define (divide-with-debug x y)
  "Division with error handling and debug info"
  (format #t "\nDEBUG: divide-with-debug(~a, ~a) called~%" x y)
  (catch #t
    (lambda ()
      (if (zero? y)
          (begin
            (format #t "DEBUG: Division by zero detected!~%")
            (error "Division by zero!" x y))
          (let ((result (/ x y)))
            (format #t "DEBUG: ~a / ~a = ~a~%" x y result)
            result)))
    (lambda (key . args)
      (format #t "ERROR caught: ~a~%" key)
      (format #t "ERROR details: ~a~%" args)
      #f)))

(display "\nTesting error handling:\n")
(divide-with-debug 10 2)
(divide-with-debug 10 0)

(format #t "\nScript completed successfully!~%")