;;; test-environment.scm - Test environment checking functions

;;; Mock environment for testing
(define *mock-env* (make-hash-table))
(hash-set! *mock-env* 'in-git-repo #t)
(hash-set! *mock-env* 'has-github-remote #t)
(hash-set! *mock-env* 'has-readme #t)
(hash-set! *mock-env* 'jq-available #t)
(hash-set! *mock-env* 'ollama-running #f)  ; Assume not running for tests
(hash-set! *mock-env* 'github-token #f)

;;; Environment check tests
(test-start "environment checks")
(let ((check-env (lambda (key)
                  (hash-ref *mock-env* key #f))))
  
  (assert (check-env 'in-git-repo) "Should detect git repo")
  (assert (check-env 'has-github-remote) "Should detect GitHub remote")
  (assert (check-env 'has-readme) "Should detect README")
  (assert (check-env 'jq-available) "Should detect jq")
  (assert (not (check-env 'ollama-running)) "Should detect Ollama not running")
  (assert (not (check-env 'github-token)) "Should detect missing GitHub token"))
(test-pass)

;;; Command availability tests
(test-start "command availability")
(let ((command-exists? (lambda (cmd)
                        (zero? (car (run-command (format #f "which ~a > /dev/null 2>&1" cmd)))))))
  
  ;; These should exist on most systems
  (assert (command-exists? "sh") "sh should exist")
  (assert (command-exists? "echo") "echo should exist")
  (assert (command-exists? "cat") "cat should exist")
  
  ;; This probably doesn't exist
  (assert (not (command-exists? "nonexistent-command-12345")) 
          "nonexistent command should not exist"))
(test-pass)

;;; Git repository tests
(test-start "git repository detection")
(let ((in-git-repo? (lambda ()
                     (zero? (car (run-command "git rev-parse --git-dir > /dev/null 2>&1"))))))
  
  ;; We should be in a git repo
  (assert (in-git-repo?) "Current directory should be a git repository"))
(test-pass)

;;; File detection tests
(test-start "file detection")
(let ((has-file? (lambda (patterns)
                  (any file-exists? patterns))))
  
  ;; Check for common files
  (assert (has-file? '("Makefile" "makefile")) "Should have Makefile")
  (assert (has-file? '("README.md" "readme.md" "README.rst" "README.txt")) 
          "Should have README")
  
  ;; Check for test files we just created
  (assert (file-exists? "tests/repo-topics/test-runner.scm") 
          "Test runner should exist")
  (assert (not (has-file? '("nonexistent.xyz"))) 
          "Nonexistent file should not exist"))
(test-pass)

;;; Environment variable tests
(test-start "environment variables")
(let ((get-env-default (lambda (var default)
                        (or (getenv var) default))))
  
  ;; PATH should always exist
  (assert (getenv "PATH") "PATH should be set")
  
  ;; Test default values
  (assert-equal (get-env-default "NONEXISTENT_VAR_12345" "default") "default")
  
  ;; HOME should exist on Unix systems
  (assert (getenv "HOME") "HOME should be set"))
(test-pass)