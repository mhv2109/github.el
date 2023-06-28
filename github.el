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
    :accessor prev-link)
   (first-link
    :initarg :first-link
    :accessor first-link)
   (last-link
    :initarg :last-link
    :accessor last-link))
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
		     :prev-link (extract-link "prev")
		     :first-link (extract-link "first")
		     :last-link (extract-link "last")))))

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

(defun github-get (url)
  (github-req "GET" url))

(defun github-get-pageable (url)
  (->> (-> url
	   github-get)
       (apply 'make-github-pageable)))

(defun github-get-pageable-if (url)
  (if url
      (github-get-pageable url)))

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
       (apply 'format "repos/%s/%s/issues?pulls=false") ;; pulls=false omits pull requests
       github-get-path
       (apply 'make-github-pageable)))

(defun github-get-pull-requests (remote)
  (->> '(github-user-org github-project-name)
       (mapcar (lambda (f) (funcall f remote)))
       (apply 'format "repos/%s/%s/pulls")
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
    (-> url
	extract-from
	(string-trim-right ".git[ \t\n\r]*"))))

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

;; Links for resources on tabulated-list-mode
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Tabulated-List-Mode.html
;; http://rgrinberg.com/posts/emacs-table-display/
;; https://stackoverflow.com/questions/11272632/how-to-create-a-column-view-in-emacs-lisp

;; https://stackoverflow.com/questions/38147620/shell-script-to-open-a-url
(defun visit-url-in-browser (url)
  "Open URL in your system browser using shell commands."
  (cond ((eq system-type 'gnu/linux) (shell-command (format "xdg-open %s" url)))
	((eq system-type 'darwin) (shell-command (format "open %s" url)))
	((eq system-type 'windows) (shell-command (format "start %s" url)))
	(t (error "Unsupported system-type: %s" system-type))))

(defun display-table-in-buffer (buffer rows columns-to-keep mode)
  "Given a sequence of hashtables ROWS, display COLUMNS-TO-KEEP in BUFFER. Clicking on
or pressing ENTER on any text will open the respective page in the browser. MODE should
be a quoted variable of a major mode that extends tabulated-list-mode."
  
  (with-current-buffer buffer
    (read-only-mode)
    (let ((inhibit-read-only t))
      (let* ((column-header-names columns-to-keep)
	     (this-tabulated-list-entries (let ((rownum 0))
					    (mapcar (lambda (row)
						      (progn
							(setq rownum (+ 1 rownum))
							(list
							 (number-to-string rownum)
							 (vconcat (mapcar (lambda (key)
									    (let* ((value (gethash key row))
										   (cast-value (if (numberp value) (number-to-string value) value)))
									      (cons
									       cast-value
									       (list
										'action `(lambda (x)
											   (-> "html_url"
											       (gethash ,row)
											       visit-url-in-browser))))))
									  column-header-names)))))
						    rows))))
	(erase-buffer)
	(setq tabulated-list-entries this-tabulated-list-entries)
	(funcall mode)
	(goto-char (point-min))
	(display-buffer buffer)))))

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

(defvar github-project-issues-tabulated-list-format
  [("number" 20 t) ;; most-positive-fixnum string repr on my system has length 19
   ("created_at" 21 t) ;; pretty sure the timestamp format always has length 20
   ("title" 256 t)]    ;; 256 length was chosen arbitrarily
  )

(defvar github-project-next-issues-url nil)
(defvar github-project-prev-issues-url nil)
(defvar github-project-first-issues-url nil)
(defvar github-project-last-issues-url nil)

(define-derived-mode github-issues-mode tabulated-list-mode "github-issues-mode" "Major mode for viewing GitHub Issues."
  (setq tabulated-list-format github-project-issues-tabulated-list-format) 
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(define-key github-issues-mode-map (kbd "p") (lambda ()
    					       (interactive)
					       (let ((pageable (github-get-pageable-if github-project-prev-issues-url)))
						 (if pageable
						     (github-project-issues-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "n") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-next-issues-url)))
						   (if pageable
						       (github-project-issues-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "f") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-first-issues-url)))
						   (if pageable
						       (github-project-issues-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "l") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-last-issues-url)))
						   (if pageable
						       (github-project-issues-display-table-in-buffer pageable)))))

(defun github-project-issues-display-table-in-buffer (pageable &rest kwargs)
  (let ((issues (page pageable))
	(next (next-link pageable))
	(prev (prev-link pageable))
	(first (first-link pageable))
	(last (last-link pageable))
	(buffer (-> kwargs
		    (plist-get :buffer)
		    (or (get-buffer-create "*GitHub Issues*"))))
	(columns-to-keep (mapcar (lambda (item)
				   (car item))
				 github-project-issues-tabulated-list-format)))
    (setq github-project-next-issues-url next)
    (setq github-project-prev-issues-url prev)
    (setq github-project-first-issues-url first)
    (setq github-project-last-issues-url last)
    (display-table-in-buffer buffer issues columns-to-keep 'github-issues-mode)))

(defun github-project-issues (&optional handle &rest args)
  "List all open issues in buffer named *GitHub Issues*."
  (-> (read-string "Handle: " "origin")
      list
      interactive)
  (-> handle
      github-remote
      github-get-issues
      github-project-issues-display-table-in-buffer))

;;
;; Pull Requests
;;

(defvar github-project-pull-requests-tabulated-list-format
  [("number" 20 t) ;; most-positive-fixnum string repr on my system has length 19
   ("created_at" 21 t) ;; pretty sure the timestamp format always has length 20
   ("title" 256 t)]    ;; 256 length was chosen arbitrarily
  )

(defvar github-project-next-pull-requests-url nil)
(defvar github-project-prev-pull-requests-url nil)
(defvar github-project-first-pull-requests-url nil)
(defvar github-project-last-pull-requests-url nil)

(define-derived-mode github-pull-requests-mode tabulated-list-mode "github-pull-requests-mode" "Major mode for viewing GitHub Pull Requests."
  (setq tabulated-list-format github-project-pull-requests-tabulated-list-format) 
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(define-key github-issues-mode-map (kbd "p") (lambda ()
    					       (interactive)
					       (let ((pageable (github-get-pageable-if github-project-prev-pull-requests-url)))
						 (if pageable
						     (github-project-pull-requests-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "n") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-next-pull-requests-url)))
						   (if pageable
						       (github-project-pull-requests-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "f") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-first-pull-requests-url)))
						   (if pageable
						       (github-project-pull-requests-display-table-in-buffer pageable)))))

(define-key github-issues-mode-map (kbd "l") (lambda ()
    						 (interactive)
    						 (let ((pageable (github-get-pageable-if github-project-last-pull-requests-url)))
						   (if pageable
						       (github-project-pull-requests-display-table-in-buffer pageable)))))

(defun github-project-pull-requests-display-table-in-buffer (pageable &rest kwargs)
  (let ((prs (page pageable))
	(next (next-link pageable))
	(prev (prev-link pageable))
	(first (first-link pageable))
	(last (last-link pageable))
	(buffer (-> kwargs
		    (plist-get :buffer)
		    (or (get-buffer-create "*GitHub Pull Requests*"))))
	(columns-to-keep (mapcar (lambda (item)
				   (car item))
				 github-project-pull-requests-tabulated-list-format)))
    (setq github-project-next-pull-requests-url next)
    (setq github-project-prev-pull-requests-url prev)
    (setq github-project-first-pull-requests-url first)
    (setq github-project-last-pull-requests-url last)
    (display-table-in-buffer buffer prs columns-to-keep 'github-issues-mode)))

(defun github-project-pull-requests (&optional handle &rest args)
  "List all open issues in buffer named *GitHub Pull Requests*."
  (-> (read-string "Handle: " "origin")
      list
      interactive)
  (-> handle
      github-remote
      github-get-pull-requests
      github-project-pull-requests-display-table-in-buffer))

(provide 'github)
