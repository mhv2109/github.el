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

(defclass github-pageable ()
  ((page
    :initarg :page
    :accessor page)
   (next-link
    :initarg :next-link
    :accessor next-link)
   (prev-link
    :initarg :prev-link 
    :accessor prev-link))
  (:documentation "https://docs.github.com/en/rest/guides/using-pagination-in-the-rest-api?apiVersion=2022-11-28#using-link-headers"))

(defun make-github-pageable (&rest args)
  "Parses HTTP response per GitHub's docs to build a GITHUB-PAGEABLE.

See: https://docs.github.com/en/rest/guides/using-pagination-in-the-rest-api?apiVersion=2022-11-28#using-link-headers"
  (let* ((body (plist-get args :body))
	 (headers (plist-get args :headers))
	 (regex-format-string-template "[,]?[[:space:]]+<\\([^<>]*\\)>;[[:space:]]+rel=\"%s\""))
    (cl-flet* ((regex-format-string (rel)
				    (format regex-format-string-template rel))
	       (extract-link (rel)
			     (if (-> rel
				     regex-format-string
				     (string-match headers))
				 (match-string 1 headers))))
      (make-instance github-pageable
		     :page (json-parse-string body)
		     :next-link (extract-link "next")
		     :prev-link (extract-link "prev")))))

(cl-defgeneric pageable-next-page (pageable)
  (:documentation "Returns the next PAGEABLE using PAGEABLE's next-link"))

(cl-defgeneric pageable-prev-page (pageable)
  (:documentation "Returns the next PAGEABLE using PAGEABLE's prev-link"))

(cl-defmethod pageable-next-page ((pageable github-pageable))
  (-> pageable
      next-link
      (github-req "GET")
      make-github-pageable))

(cl-defmethod pageable-prev-page ((pageable github-pageable))
  (-> pageable
      prev-link
      (github-req "GET")
      make-github-pageable))

(defun github-req (method url)
  "Make a request with http method METHOD to URL that set's the required headers for the GitHub API. Returns HEADERS and BODY as a plist with symbol keys."
  (message (format "Making GitHub Request: %s\t%s" method url))
  (let ((url-request-method method)

	;; request headers
	(url-request-extra-headers (list
				     ;; auth token (PAT)
				     (cons "Authorization" (concat "Bearer " github-access-token))

				     ;; gh's content-type
				     (cons "Accept" "application/vnd.github+json")

				     ;; gh API version
				     (cons "X-GitHub-Api-Version" "2022-11-28"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((headers (buffer-substring (point-min) url-http-end-of-headers))
	    (body (buffer-substring url-http-end-of-headers (point-max))))
	(list :headers headers :body body)))))

(defun github-req-path (method path)
  "Make a request to GitHub's API. METHOD is the HTTP method and PATH is the HTTP request path. Uses GITHUB-API-BASE-URL."
  (->> path
       (concat github-api-base-url "/")
       (github-req method)))

(defun github-get-path (path)
  "Make a GET request to GitHub's API."
  (github-req-path "GET" path))

(defun github-get-issues (remote)
  (->> '(github-user-org github-project-name)
       (mapcar (lambda (f) (funcall f remote)))
       (apply 'format "repos/%s/%s/issues")
       github-get-path
       (apply 'make-github-pageable)))

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

;;
;; Permalinks
;;

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

;;
;; Issues
;;

(defun github-project-issues (&optional handle &rest args)
  "List all open issues in buffer named *GitHub Issues*."
  (-> (read-string "Handle: " "origin")
      list
      interactive)
  (let ((called-interactively (called-interactively-p 'any)))
    (cl-flet* ((to-string (val) (if (numberp val) (number-to-string val) val))

	       ;; extract the relevant fields from each issue, join with a comma (,) and split with a newline (\n)
	       (build-row (issue)
			  ;; TODO: add keymap to visit issue in browser on <Enter>
			  (let ((params '("number" "created_at" "title")))
			    (string-join (mapcar (lambda (param) (-> param
								     (gethash issue)
								     to-string))
						 params)
					 "\t")))
	       ;; build number, age, title header
	       (build-header ()
			     (-> "number\tcreated_at\ttitle\n"
				 (propertize 'read-only t)
				 insert))
	       ;; init buffer, add the header, and each row
	       (build-table (pageable)
			    (let ((issues (page pageable)))
			      (when called-interactively
				(let ((buffer (-> args
						  (plist-get :buffer)
						  (or (get-buffer-create "*GitHub Issues*")))))
				  (with-current-buffer buffer
				    (read-only-mode)
				    (let ((inhibit-read-only t))
				      (erase-buffer)
				      (build-header)
				      (mapc (lambda (issue) (-> issue
								build-row
								(insert "\n")))
					    issues)
				      (goto-char (point-min))
				      (display-buffer buffer)))))
			      issues)))
      (-> handle
	  github-remote
	  github-get-issues
	  build-table))))

(provide 'github)
