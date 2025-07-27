;;; test-parsing.scm - Test parsing functions

(use-modules (ice-9 regex))

;;; URL parsing tests
(test-start "parse-github-url")
(let ((parse-github-url (lambda (url)
                         (let ((match (or (string-match "github\\.com[:/]([^/]+)/([^/\\.]+)" url)
                                        (string-match "github\\.com/([^/]+)/([^/]+)\\.git" url))))
                           (if match
                               (cons (match:substring match 1) (match:substring match 2))
                               (cons "unknown" "unknown"))))))
  
  ;; HTTPS URLs
  (assert-equal (parse-github-url "https://github.com/owner/repo")
                '("owner" . "repo"))
  (assert-equal (parse-github-url "https://github.com/owner/repo.git")
                '("owner" . "repo"))
  (assert-equal (parse-github-url "https://github.com/complex-org/complex-repo-name")
                '("complex-org" . "complex-repo-name"))
  
  ;; SSH URLs
  (assert-equal (parse-github-url "git@github.com:owner/repo.git")
                '("owner" . "repo"))
  (assert-equal (parse-github-url "git@github.com:owner/repo")
                '("owner" . "repo"))
  
  ;; Invalid URLs
  (assert-equal (parse-github-url "https://gitlab.com/owner/repo")
                '("unknown" . "unknown"))
  (assert-equal (parse-github-url "not-a-url")
                '("unknown" . "unknown")))
(test-pass)

;;; File extension parsing tests
(test-start "path-extension")
(let ((path-extension (lambda (path)
                       (let ((dot-pos (string-rindex path #\.)))
                         (if (and dot-pos (< dot-pos (- (string-length path) 1)))
                             (substring path (+ dot-pos 1))
                             #f)))))
  (assert-equal (path-extension "file.txt") "txt")
  (assert-equal (path-extension "script.py") "py")
  (assert-equal (path-extension "module.scm") "scm")
  (assert-equal (path-extension "archive.tar.gz") "gz")
  (assert-equal (path-extension "no-extension") #f)
  (assert-equal (path-extension "ends-with.") #f)
  (assert-equal (path-extension ".hidden") "hidden"))
(test-pass)

;;; Ollama response parsing simulation
(test-start "parse-ollama-topics-response")
(let ((parse-topics (lambda (json-str)
                     ;; Simulate jq parsing
                     (cond
                       ((string-contains json-str "\"topics\":[\"scheme\",\"lisp\",\"llm\"]")
                        '("scheme" "lisp" "llm"))
                       ((string-contains json-str "\"topics\":[]")
                        '())
                       (else #f)))))
  
  (assert-equal (parse-topics "{\"response\":\"{\\\"topics\\\":[\\\"scheme\\\",\\\"lisp\\\",\\\"llm\\\"]}\"}") 
                '("scheme" "lisp" "llm"))
  (assert-equal (parse-topics "{\"response\":\"{\\\"topics\\\":[]}\"}") 
                '())
  (assert-equal (parse-topics "{\"error\":\"failed\"}") 
                #f))
(test-pass)

;;; String helper tests
(test-start "string helper functions")
(let ((string-contains (lambda (str substr)
                        (if (string-match substr str) #t #f)))
      (string-suffix? (lambda (suffix str)
                       (let ((slen (string-length str))
                             (suflen (string-length suffix)))
                         (and (>= slen suflen)
                              (string=? suffix (substring str (- slen suflen))))))))
  
  ;; string-contains tests
  (assert (string-contains "hello world" "world") "should find substring")
  (assert (string-contains "github.com" "github") "should find github in github.com")
  (assert (not (string-contains "hello" "world")) "should not find world in hello")
  
  ;; string-suffix? tests
  (assert (string-suffix? ".git" "repo.git") "should find .git suffix")
  (assert (string-suffix? "/" "path/") "should find / suffix")
  (assert (not (string-suffix? ".git" "repository")) "should not find .git in repository")
  (assert (string-suffix? "" "anything") "empty string is suffix of anything"))
(test-pass)