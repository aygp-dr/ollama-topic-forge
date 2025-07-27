#!/usr/bin/env guile
!#
;; Letter frequency analyzer - implements the complex pipeline in Guile

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26))

(define (read-words-file)
  "Read /usr/share/dict/words and return as string"
  (call-with-input-file "/usr/share/dict/words"
    (lambda (port)
      (read-string port))))

(define (normalize-text text)
  "Convert to lowercase and keep only alphabetic characters"
  (list->string
   (filter char-alphabetic?
           (string->list (string-downcase text)))))

(define (count-letters text)
  "Count frequency of each letter"
  (let ((counts (make-hash-table)))
    (string-for-each
     (lambda (char)
       (hash-set! counts char 
                  (1+ (hash-ref counts char 0))))
     text)
    counts))

(define (hash-table->sorted-alist table)
  "Convert hash table to association list sorted by value (descending)"
  (sort (hash-map->list cons table)
        (lambda (a b)
          (> (cdr a) (cdr b)))))

(define (calculate-percentages counts total)
  "Calculate percentage for each count"
  (map (lambda (pair)
         (cons (car pair)
               (* 100.0 (/ (cdr pair) total))))
       counts))

(define (main)
  (format #t "Letter Frequency Analysis of /usr/share/dict/words~%")
  (format #t "~a~%" (make-string 50 #\=))
  
  (catch #t
    (lambda ()
      ;; Read and process the words file
      (let* ((text (read-words-file))
             (normalized (normalize-text text))
             (letter-counts (count-letters normalized))
             (sorted-counts (hash-table->sorted-alist letter-counts))
             (total-letters (string-length normalized))
             (percentages (calculate-percentages sorted-counts total-letters)))
        
        (format #t "Total letters analyzed: ~:d~%" total-letters)
        (format #t "~%Letter frequencies:~%")
        (format #t "~a~%" (make-string 30 #\-))
        
        ;; Display results
        (for-each
         (lambda (pair)
           (format #t "~6,2f% ~c (~:d occurrences)~%"
                   (cdr pair)
                   (car pair)
                   (cdr (assoc (car pair) sorted-counts))))
         percentages)
        
        ;; Summary statistics
        (format #t "~%~a~%" (make-string 30 #\-))
        (format #t "Most common: '~c' (~,2f%)~%"
                (caar percentages)
                (cdar percentages))
        (format #t "Least common: '~c' (~,2f%)~%"
                (car (last percentages))
                (cdr (last percentages)))))
    
    (lambda (key . args)
      (format #t "Error: ~a~%" key)
      (when (eq? key 'system-error)
        (format #t "Could not read /usr/share/dict/words~%")
        (format #t "Using sample text instead...~%~%")
        
        ;; Fallback: analyze sample text
        (let* ((sample "The quick brown fox jumps over the lazy dog")
               (normalized (normalize-text sample))
               (letter-counts (count-letters normalized))
               (sorted-counts (hash-table->sorted-alist letter-counts))
               (total-letters (string-length normalized))
               (percentages (calculate-percentages sorted-counts total-letters)))
          
          (format #t "Analyzing: \"~a\"~%" sample)
          (format #t "Letters: ~a~%~%" total-letters)
          
          (for-each
           (lambda (pair)
             (format #t "~6,2f% ~c~%"
                     (cdr pair)
                     (car pair)))
           percentages))))))

(main)