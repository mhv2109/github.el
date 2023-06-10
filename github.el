;;
;; Global variables
;;

(defvar github-access-token ""
  "GitHub Personal Access Token (PAT) as a string.

See: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens")

(defvar github-api-base-url "https://api.github.com"
  "Base URL for GitHub API.")

(defvar github-base-url "https://github.com"
  "Base URL for GitHub UI.")

;;
;; Client
;;

(defun github-req (method path)
  "Make a request to GitHub's API. METHOD is the HTTP method and PATH is the HTTP request path."
  (let (
	(url-request-method method)

	;; request headers
	(url-request-extra-headers (list
				     ;; auth token (PAT)
				     (cons "Authorization" (concat "Bearer: " github-access-token))

				     ;; gh's content-type
				     (cons "Accept" "application/vnd.github+json")))
	;; full request URL
	(url (concat github-api-base-url "/" path)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun github-get (path)
  "Make a GET request to GitHub's API."
  (github-req "GET" path))

;;
;; Lib
;;

(defun git-commit (&optional ref)
  "Returns the long SHA for REF."
  (let* ((ref (if ref ref "HEAD"))
	 (sha (shell-command-to-string (format "git rev-parse %s" ref))))
    (string-clean-whitespace sha)))

(defun git-root ()
  "Returns an absolute path to the project root that contains the nearest .git folder."
  (let ((root (shell-command-to-string "git rev-parse --show-toplevel")))
    (string-clean-whitespace root)))

(defun print-or-return (print val)
  "Helper function to either print VAL to minibuffer and copy to kill ring if called interactively,
or simply return VAL otherwise."
  (if print
      (progn
	(kill-new val)
	(message val))
    val))

(defun github-remotes ()
  "Get the GitHub (fetch) remote repositories for the current file as (name . url) pairs. Currently only works with https URLs."
  (with-current-buffer (current-buffer)
    (let ((remotes (split-string (shell-command-to-string "git remote -v") "\n"))
	  (extract (lambda (remote)
		     (cond
		      ;; HTTP GH remotes
		      ((string-match "^\\(.*\\)[[:space:]]+\\(https://github\.com.*\\)[[:space:]]+(fetch)$" remote) (let ((handle (match-string 1 remote))
															  (url (match-string 2 remote)))
														      (cons handle url)))
		      ;; TODO: SSH remotes
		      ;; ()
		      ;; default -- no remote
		      (t nil)))))
      (remove-if (lambda (elem) (if elem nil t)) (mapcar extract remotes)))))

(defun github-remote (&optional handle)
  "Get one GitHub (fetch) remote repository for the current file. Currently only works with https URLs. HANDLE defaults to origin."
  (-> (read-string "Handle: " "origin")
      list
      interactive)
  (cl-flet ((lookup-handle (handle) (assoc handle (github-remotes))))
    (let ((res (-> handle
		   (if handle "origin")
		   lookup-handle
		   cdr)))
      (-> (called-interactively-p 'any)
	  (print-or-return res)))))

(defun github-user-org (url)
  "Extracts GitHub User/Org from URL. Currently only works with https URLs."
  (cond
   ;; HTTP
   ((string-match "^https://github\.com/\\(.*\\)/.*$" url) (match-string 1 url))
   ;; TODO: SSH
   ;; ()
   ;; default -- no match
   (t nil)))

(defun github-project-name (url)
  "Extracts the GitHub Project name from URL. Currently only works with https URLs."
  (cond
   ;; HTTP
   ((string-match "^https://github\.com/.*/\\(.*\\)$" url) (match-string 1 url))
   ;; TODO: SSH
   ;; ()
   ;; default -- no match
   (t nil)))

(defun github-commit-url-format-string (commit)
  "Builds a format string for COMMIT, in long SHA form."
  (let* ((remote (github-remote))
	 (user-org (github-user-org remote))
         (project-name (github-project-name remote))
	 (trailing-slash (-> github-base-url
			     (substring -1)
			     (string= "/")
			     (if "" "/"))))
    (concat github-base-url trailing-slash user-org "/" project-name "/%s/" commit)))

(defun github-project-permalink ()
  "Returns a permalink to the HEAD of the current file's project's ref. If called interactively,
 will print to minibuffer and copy to kill-ring. Otherwise, returns the link as a string."
  (interactive "p")
  (let* ((res (-> (git-commit)
		  github-commit-url-format-string
		  (format "tree"))))
    (-> (called-interactively-p 'any)
	(print-or-return res))))

(defun github-file-permalink ()
  "Returns a permalink to the HEAD of the current file. If called interactively, will
print to minibuffer and copy to kill-ring. Otherwise, returns the link as a string."
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((current-commit (git-commit))
	   (trimmed (-> buffer-file-name
			(string-trim-left (git-root))))
	   (res (-> (github-commit-url-format-string current-commit)
		    (format "blob")
		    (concat trimmed))))
      (-> (called-interactively-p 'any)
	  (print-or-return res)))))

(defun github-permalink-at-point ()
  "Returns a permalink to the HEAD of the current file at the line under point. If
called interactively, will print to minibuffer and copy to kill-ring. Otherwise,
returns the link as a string."
  (interactive)
  (with-current-buffer (current-buffer)
    (cl-flet ((build-permalink (lineno) (concat (github-file-permalink) "#L" lineno)))
      (let ((res (-> (line-number-at-pos)
		     number-to-string
		     build-permalink)))
	(-> (called-interactively-p 'any)
	    (print-or-return res))))))

;;
;; Tests
;;

(ert-deftest test-gitub-remotes ()
  "Tests that github-remotes can properly return the github.com Git remote repository for this very project."
  (should (equal (github-remotes) '(("origin" . "https://github.com/mhv2109/github.el")))))

(ert-deftest test-github-remote ()
  "Tests that github-remote can properly return the github.com Git remote repository for this very project."
  (should (equal (github-remote) "https://github.com/mhv2109/github.el")))

(ert-deftest test-github-user-org ()
  "Tests that github-user-org can properly return the username for this very project."
  (should (equal (github-user-org "https://github.com/mhv2109/github.el") "mhv2109")))

(ert-deftest test-github-project-name ()
  "Tests that github-project-name can properly return the project name for this very project."
  (should (equal (github-project-name "https://github.com/mhv2109/github.el") "github.el")))
