(use-modules (ice-9 format)
             (ice-9 regex))

(define test-response "{\"model\":\"llama3.2:3b\",\"created_at\":\"2025-07-25T21:53:28.96684Z\",\"response\":\"{\\\"topics\\\": [\\\"lisp\\\", \\\"functional-programming\\\", \\\"project-purpose/dynamic-repl\\\", \\\"key-technologies/llm\\\", \\\"project-type/tool\\\"]}\"}")

(format #t "Testing JSON parsing...~%")
(format #t "Response: ~a~%~%" test-response)

;; Try to extract topics directly
(let ((match (string-match "\\\\\\[([^\\\\\\]]+)\\\\\\]" test-response)))
  (if match
      (let ((topics-str (match:substring match 1)))
        (format #t "Found topics string: ~a~%" topics-str)
        ;; Clean up each topic
        (let ((topics (map (lambda (s)
                            (let ((cleaned (string-trim-both s)))
                              ;; Remove escaped quotes
                              (regexp-substitute/global #f "\\\\\"" cleaned 'pre "" 'post)))
                          (string-split topics-str #\,))))
          (format #t "Parsed topics: ~a~%" topics)))
      (format #t "No match found~%")))

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