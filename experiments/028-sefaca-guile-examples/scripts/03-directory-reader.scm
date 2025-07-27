#!/usr/bin/env guile
!#
;; Directory reader - reads all files in current directory

(use-modules (ice-9 ftw)
             (ice-9 format)
             (srfi srfi-1))

(define (file-info path)
  "Get detailed information about a file"
  (let ((st (stat path)))
    `((path . ,path)
      (type . ,(stat:type st))
      (size . ,(stat:size st))
      (mode . ,(format #f "~4,'0o" (logand (stat:mode st) #o7777)))
      (mtime . ,(strftime "%Y-%m-%d %H:%M:%S" (localtime (stat:mtime st)))))))

(define (read-directory dir)
  "Read all files in directory and return info"
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (eq? flag 'regular)
             (set! files (cons (file-info filename) files)))
           #t))
    (reverse files)))

(define (format-bytes bytes)
  "Format bytes in human-readable form"
  (cond
   ((< bytes 1024) (format #f "~aB" bytes))
   ((< bytes (* 1024 1024)) (format #f "~,1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format #f "~,1fMB" (/ bytes 1024.0 1024.0)))
   (else (format #f "~,1fGB" (/ bytes 1024.0 1024.0 1024.0)))))

(define (main)
  (let* ((dir (or (and (> (length (command-line)) 1)
                       (cadr (command-line)))
                  "."))
         (files (read-directory dir)))
    
    (format #t "Directory listing for: ~a~%" dir)
    (format #t "~a~%" (make-string 60 #\=))
    (format #t "~4a ~10a ~19a ~a~%" "Mode" "Size" "Modified" "Name")
    (format #t "~a~%" (make-string 60 #\-))
    
    (for-each
     (lambda (file)
       (format #t "~4a ~10a ~19a ~a~%"
               (assoc-ref file 'mode)
               (format-bytes (assoc-ref file 'size))
               (assoc-ref file 'mtime)
               (basename (assoc-ref file 'path))))
     (sort files (lambda (a b)
                   (string<? (assoc-ref a 'path)
                            (assoc-ref b 'path)))))
    
    (format #t "~a~%" (make-string 60 #\-))
    (format #t "Total files: ~a~%" (length files))
    (format #t "Total size: ~a~%"
            (format-bytes (apply + (map (lambda (f) (assoc-ref f 'size)) files))))))

(main)