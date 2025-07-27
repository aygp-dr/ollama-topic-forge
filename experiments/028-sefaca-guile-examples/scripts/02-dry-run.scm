#!/usr/bin/env guile
!#
;; Dry-run example - simulates file operations without actually performing them

(use-modules (ice-9 getopt-long)
             (ice-9 format)
             (srfi srfi-1))

(define *dry-run* #f)

(define (log-action action target)
  "Log what would be done"
  (if *dry-run*
      (format #t "[DRY-RUN] Would ~a: ~a~%" action target)
      (format #t "[EXECUTE] ~a: ~a~%" action target)))

(define (safe-delete-file filename)
  "Delete file with dry-run support"
  (log-action "delete file" filename)
  (unless *dry-run*
    (catch 'system-error
      (lambda () (delete-file filename))
      (lambda (key . args)
        (format #t "Error deleting ~a: ~a~%" filename args)))))

(define (safe-rename-file old new)
  "Rename file with dry-run support"
  (log-action "rename" (format #f "~a -> ~a" old new))
  (unless *dry-run*
    (catch 'system-error
      (lambda () (rename-file old new))
      (lambda (key . args)
        (format #t "Error renaming ~a: ~a~%" old args)))))

(define (safe-create-directory dir)
  "Create directory with dry-run support"
  (log-action "create directory" dir)
  (unless *dry-run*
    (catch 'system-error
      (lambda () (mkdir dir))
      (lambda (key . args)
        (format #t "Error creating directory ~a: ~a~%" dir args)))))

;; Command-line option processing
(define option-spec
  '((dry-run (single-char #\n) (value #f))
    (help    (single-char #\h) (value #f))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (dry-run (option-ref options 'dry-run #f)))
    
    (when help-wanted
      (display "Usage: dry-run.scm [--dry-run|-n] [--help|-h]\n")
      (display "Demonstrates dry-run functionality\n")
      (exit 0))
    
    (set! *dry-run* dry-run)
    
    (format #t "Running in ~a mode~%~%" 
            (if *dry-run* "DRY-RUN" "EXECUTE"))
    
    ;; Demonstrate various operations
    (safe-create-directory "/tmp/test-dir")
    (safe-delete-file "/tmp/test-file.txt")
    (safe-rename-file "/tmp/old-name.txt" "/tmp/new-name.txt")
    
    ;; Batch operations
    (format #t "~%Batch operations:~%")
    (for-each 
     (lambda (i)
       (safe-create-directory (format #f "/tmp/dir-~a" i)))
     (iota 5))))

(main (command-line))