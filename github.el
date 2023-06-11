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
  "Get the GitHub (fetch) remote repositories for the current file as (NAME . URL) pairs. Works with https and ssh URLs."
  (with-current-buffer (current-buffer)
    (cl-flet* ((pluck-match (remote)
			    (let ((handle (match-string 1 remote))
				  (url (match-string 2 remote)))
			      (cons handle url)))
	       (extract (remote)
			(cond
			 ;; HTTP GH remotes
			 ((string-match "^\\(.*\\)[[:space:]]+\\(https://github\.com.*\\)[[:space:]]+(fetch)$" remote) (pluck-match remote))
			 ;; SSH GH remotes
			 ((string-match "^\\(.*\\)[[:space:]]+\\([ssh://]?git@github\.com:.*\.git\\)[[:space:]]+(fetch)$" remote) (pluck-match remote))
			 ;; default -- no remote
			 (t nil)))
	       (is-nil (item) (if item nil t)))
      (let ((remotes (-> "git remote -v"
			 shell-command-to-string
			 (split-string "\n"))))
	(->> (mapcar (lambda (remote) (extract remote)) remotes)
	     (remove-if (lambda (item) (is-nil item))))))))

(defun github-remote (&optional handle)
  "Get one GitHub (fetch) remote repository for the current file. Works with https and ssh URLs. HANDLE defaults to origin."
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
  "Extracts GitHub User/Org from URL. Works with https and ssh URLs."
  (cl-flet* ((pluck-match (remote)
			  (match-string 1 remote))
	     (extract-from (remote)
			   (cond
			    ;; HTTP
			    ((string-match "^https://github\.com/\\(.*\\)/.*$" remote) (pluck-match remote))
			    ;; SSH
			    ((string-match "^[ssh://]?git@github\.com:\\(.*\\)/.*\.git$" remote) (pluck-match remote))
			    ;; default -- no match
			    (t nil))))
    (extract-from url)))

(defun github-project-name (url)
  "Extracts the GitHub Project name from URL. Works with https and ssh URLs."
  (cl-flet* ((pluck-match (remote)
			  (match-string 1 remote))
	     (extract-from (remote)
			   (cond
			    ;; HTTP
			    ((string-match "^https://github\.com/.*/\\(.*\\)$" remote) (pluck-match remote))
			    ;; SSH
			    ((string-match "^[ssh://]?git@github\.com:.*/\\(.*\\)\.git$" remote) (pluck-match remote))
			    ;; default -- no match
			    (t nil))))
    (extract-from url)))

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
  "Returns a permalink to the HEAD of the current file at the line under point, or
the active region as a range. If called interactively, will print to minibuffer and
copy to kill-ring. Otherwise, returns the link as a string."
  (interactive)
  (with-current-buffer (current-buffer)
    (cl-flet ((get-line-numbers () (if (use-region-p)
				       (mapcar 'line-number-at-pos (list (region-beginning) (region-end)))
				     (list (line-number-at-pos))))
	      (build-permalink (linenos)
			       (let ((begin (car linenos))
				     (end (cadr linenos)))
				 (concat (github-file-permalink) "#L" begin (if end (concat "-L" end))))))
      (let ((permalink (->> (get-line-numbers)
			    (mapcar 'number-to-string)
			    build-permalink)))
	(-> (called-interactively-p 'any)
	    (print-or-return permalink))))))

(provide 'github)
