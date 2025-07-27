#!/usr/bin/env guile
!#
;; Disk space analyzer - checks disk usage and recommends files to clean

(use-modules (ice-9 ftw)
             (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1))

(define (get-disk-usage path)
  "Get disk usage information for a path"
  (let* ((port (open-pipe* OPEN_READ "df" "-k" path))
         (header (read-line port))
         (data (read-line port)))
    (close-pipe port)
    (if (eof-object? data)
        #f
        (let ((fields (string-split data #\space)))
          (let ((filtered (filter (lambda (s) (not (string-null? s))) fields)))
            (if (>= (length filtered) 6)
                `((filesystem . ,(list-ref filtered 0))
                  (total-kb . ,(string->number (list-ref filtered 1)))
                  (used-kb . ,(string->number (list-ref filtered 2)))
                  (available-kb . ,(string->number (list-ref filtered 3)))
                  (use-percent . ,(string->number 
                                  (string-trim-right (list-ref filtered 4) #\%)))
                  (mount-point . ,(list-ref filtered 5)))
                #f))))))

(define (find-large-files dir min-size-mb)
  "Find files larger than min-size-mb in directory"
  (let ((large-files '())
        (min-size-bytes (* min-size-mb 1024 1024)))
    (catch #t
      (lambda ()
        (ftw dir
             (lambda (filename statinfo flag)
               (when (and (eq? flag 'regular)
                         (> (stat:size statinfo) min-size-bytes))
                 (set! large-files 
                       (cons `((path . ,filename)
                              (size . ,(stat:size statinfo))
                              (mtime . ,(stat:mtime statinfo)))
                             large-files)))
               #t)
             (lambda (filename statinfo flag)
               ;; Error handler - skip inaccessible files
               #t)))
      (lambda args
        (format #t "Warning: Error scanning directory ~a~%" dir)))
    large-files))

(define (find-old-files dir days-old)
  "Find files not accessed in the last days-old days"
  (let ((old-files '())
        (cutoff-time (- (current-time) (* days-old 24 60 60))))
    (catch #t
      (lambda ()
        (ftw dir
             (lambda (filename statinfo flag)
               (when (and (eq? flag 'regular)
                         (< (stat:atime statinfo) cutoff-time))
                 (set! old-files
                       (cons `((path . ,filename)
                              (size . ,(stat:size statinfo))
                              (atime . ,(stat:atime statinfo))
                              (days-old . ,(inexact->exact 
                                          (/ (- (current-time) (stat:atime statinfo))
                                             (* 24 60 60)))))
                             old-files)))
               #t)
             (lambda (filename statinfo flag)
               ;; Error handler
               #t)))
      (lambda args
        (format #t "Warning: Error scanning for old files~%")))
    old-files))

(define (format-size bytes)
  "Format bytes in human-readable form"
  (cond
   ((< bytes 1024) (format #f "~aB" bytes))
   ((< bytes (* 1024 1024)) (format #f "~,1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format #f "~,1fMB" (/ bytes 1024.0 1024.0)))
   (else (format #f "~,1fGB" (/ bytes 1024.0 1024.0 1024.0)))))

(define (main)
  (let* ((home-dir (getenv "HOME"))
         (disk-info (get-disk-usage home-dir)))
    
    (format #t "Disk Space Analysis~%")
    (format #t "~a~%" (make-string 60 #\=))
    
    ;; Display disk usage
    (when disk-info
      (format #t "Filesystem: ~a~%" (assoc-ref disk-info 'filesystem))
      (format #t "Mount point: ~a~%" (assoc-ref disk-info 'mount-point))
      (format #t "Total space: ~a~%" (format-size (* 1024 (assoc-ref disk-info 'total-kb))))
      (format #t "Used space: ~a (~a%)~%" 
              (format-size (* 1024 (assoc-ref disk-info 'used-kb)))
              (assoc-ref disk-info 'use-percent))
      (format #t "Available: ~a~%~%" 
              (format-size (* 1024 (assoc-ref disk-info 'available-kb)))))
    
    ;; Find large files in common directories
    (format #t "Large files (>100MB) in your home directory:~%")
    (format #t "~a~%" (make-string 60 #\-))
    
    (let ((large-files (find-large-files home-dir 100)))
      (if (null? large-files)
          (format #t "No large files found.~%")
          (for-each
           (lambda (file)
             (format #t "~10a  ~a~%"
                     (format-size (assoc-ref file 'size))
                     (assoc-ref file 'path)))
           (take (sort large-files 
                       (lambda (a b) 
                         (> (assoc-ref a 'size) (assoc-ref b 'size))))
                 10))))
    
    ;; Find old files
    (format #t "~%Old files (>90 days) in ~/tmp and ~/.cache:~%")
    (format #t "~a~%" (make-string 60 #\-))
    
    (let ((old-tmp-files (find-old-files (string-append home-dir "/tmp") 90))
          (old-cache-files (find-old-files (string-append home-dir "/.cache") 90)))
      (let ((all-old (append old-tmp-files old-cache-files)))
        (if (null? all-old)
            (format #t "No old files found.~%")
            (begin
              (for-each
               (lambda (file)
                 (format #t "~10a  ~3a days  ~a~%"
                         (format-size (assoc-ref file 'size))
                         (assoc-ref file 'days-old)
                         (assoc-ref file 'path)))
               (take (sort all-old
                          (lambda (a b)
                            (> (assoc-ref a 'size) (assoc-ref b 'size))))
                     10))
              (format #t "~%Total space in old files: ~a~%"
                      (format-size (apply + (map (lambda (f) (assoc-ref f 'size)) 
                                                all-old))))))))))

(main)