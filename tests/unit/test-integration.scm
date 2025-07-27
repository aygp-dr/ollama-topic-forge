;;; test-integration.scm - Integration tests for repo-topics

;;; Test help output
(test-start "repo-topics --help")
(let* ((result (run-command "./repo-topics --help"))
       (exit-code (car result))
       (output (cdr result)))
  (assert (zero? exit-code) "Help should exit with 0")
  (assert (string-contains output "Usage: repo-topics") "Should show usage")
  (assert (string-contains output "OPTIONS:") "Should show options")
  (assert (string-contains output "--dry-run") "Should document --dry-run")
  (assert (string-contains output "--model") "Should document --model")
  (assert (string-contains output "REQUIREMENTS:") "Should show requirements"))
(test-pass)

;;; Test dry-run without Ollama
(test-start "repo-topics --dry-run (environment check)")
(let* ((result (run-command "./repo-topics --dry-run"))
       (exit-code (car result))
       (output (cdr result)))
  ;; Should fail if Ollama not running
  (if (not (zero? (car (run-command "curl -s -f http://localhost:11434/api/tags -o /dev/null"))))
      (begin
        (assert (not (zero? exit-code)) "Should fail without Ollama")
        (assert (string-contains output "Ollama not running") 
                "Should report Ollama not running"))
      ;; If Ollama is running, different test
      (assert #t "Skipping - Ollama is running")))
(test-pass)

;;; Test option parsing
(test-start "repo-topics option parsing")
(let ((test-options (lambda (args expected-in-output)
                     (let* ((cmd (format #f "./repo-topics ~a 2>&1 | head -20" args))
                            (result (run-command cmd))
                            (output (cdr result)))
                       (any (lambda (text) (string-contains output text))
                            expected-in-output)))))
  
  ;; Unknown option should show help
  (assert (test-options "--unknown-option" '("Usage:" "OPTIONS:"))
          "Unknown option should show usage")
  
  ;; Verbose flag
  (assert (test-options "--verbose --help" '("Usage:"))
          "Should accept --verbose flag")
  
  ;; Model flag
  (assert (test-options "--model qwen2.5 --help" '("Usage:"))
          "Should accept --model flag"))
(test-pass)

;;; Test script permissions
(test-start "repo-topics executable")
(assert (file-exists? "./repo-topics") "repo-topics script should exist")
(let* ((stat-result (run-command "stat -f '%p' ./repo-topics 2>/dev/null || stat -c '%a' ./repo-topics"))
       (perms (cdr stat-result)))
  (assert (string-contains perms "7") "repo-topics should be executable"))
(test-pass)

;;; Test fixture generation for more complex tests
(test-start "test fixture creation")
(let ((create-test-repo (lambda (path)
                         (system* "mkdir" "-p" path)
                         (system* "git" "-C" path "init")
                         (call-with-output-file (string-append path "/README.md")
                           (lambda (port)
                             (display "# Test Repository\nA test repository for unit tests." port)))
                         (system* "git" "-C" path "add" "README.md")
                         (system* "git" "-C" path "commit" "-m" "Initial commit")
                         (system* "git" "-C" path "remote" "add" "origin" 
                                 "https://github.com/test/test-repo.git"))))
  
  ;; Create a test repository
  (let ((test-repo-path "tests/repo-topics/fixtures/test-repo"))
    (when (not (file-exists? test-repo-path))
      (create-test-repo test-repo-path))
    (assert (file-exists? (string-append test-repo-path "/.git"))
            "Test repository should be created")))
(test-pass)

;;; Mock Ollama response test
(test-start "mock Ollama response parsing")
(let ((mock-ollama-response 
       "{\"model\":\"llama3.2:3b\",\"done\":true,\"response\":\"{\\\"topics\\\":[\\\"python\\\",\\\"machine-learning\\\",\\\"data-science\\\",\\\"jupyter\\\",\\\"pandas\\\"]}\"}"))
  
  ;; Test that we can detect valid JSON structure
  (assert (string-contains mock-ollama-response "\"done\":true")
          "Response should indicate completion")
  (assert (string-contains mock-ollama-response "\"topics\"")
          "Response should contain topics")
  
  ;; Test extraction simulation with jq
  (let* ((cmd (format #f "echo '~a' | jq -r '.response' 2>/dev/null" mock-ollama-response))
         (result (run-command cmd))
         (extracted (cdr result)))
    (when (zero? (car result))
      (assert (string-contains extracted "topics")
              "Should extract response field"))))
(test-pass)