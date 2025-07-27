(use-modules (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim))

;; Make a direct Ollama request
(let* ((prompt "List 3 programming topics as JSON: {\"topics\": [\"topic1\", \"topic2\", \"topic3\"]}")
       (request (format #f "{\"model\":\"llama3.2:3b\",\"prompt\":\"~a\",\"stream\":false}" prompt))
       (temp-file "/tmp/test-ollama.json"))
  
  (call-with-output-file temp-file
    (lambda (port) (display request port)))
  
  (let* ((cmd (format #f "curl -s -X POST http://localhost:11434/api/generate -H 'Content-Type: application/json' -d @~a" temp-file))
         (port (open-pipe* OPEN_READ "/bin/sh" "-c" cmd))
         (response (read-string port)))
    (close-pipe port)
    (delete-file temp-file)
    
    (format #t "Raw response:~%~a~%" response)))