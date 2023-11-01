;;;  -*- lexical-binding: t -*-

(require 'eieio)
(require 'url)

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
			                 (if (string-match (regex-format-string rel) headers)
				                 (match-string 1 headers))))
      (make-instance github-pageable
		             :page (json-parse-string body)
		             :next-link (extract-link "next")
		             :prev-link (extract-link "prev")
		             :first-link (extract-link "first")
		             :last-link (extract-link "last")))))

(defun github-req (method url)
  "Make a request with http method METHOD to URL that set's the
required headers for the GitHub API. Returns HEADERS and BODY as a
plist with symbol keys."
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
        (unless (string-match-p "^HTTP/1.1\s200\sOK\n.*$" headers)
          (error body))
        (list :headers headers :body body)))))

(defun github-get (url)
  (github-req "GET" url))

(defun github-get-pageable (url)
  (apply 'make-github-pageable (github-get url)))

(defun github-get-pageable-if (url)
  (if url
      (github-get-pageable url)))

(defun github-req-path (method path)
  "Make a request to GitHub's API. METHOD is the HTTP method and PATH
is the HTTP request path. Uses GITHUB-API-BASE-URL."
  (github-req method (concat github-api-base-url "/" path)))

(defun github-get-path (path)
  "Make a GET request to GitHub's API."
  (github-req-path "GET" path))

(defun github-get-issues (remote)
  (let* ((user-org (github-user-org remote))
         (project-name (github-project-name remote))
         (path (format "repos/%s/%s/issues?pulls=false" user-org project-name)) ;; pulls=false omits pull requests
         (resp (github-get-path path)))
    (make-github-pageable resp)))

(defun github-get-pull-requests (remote)
  (let* ((user-org (github-user-org remote))
         (project-name (github-project-name remote))
         (path (format "repos/%s/%s/pulls" user-org project-name))
         (resp (github-get-path path)))
    (make-github-pageable resp)))

(defun github-get-releases (remote)
  (let* ((user-org (github-user-org remote))
         (project-name (github-project-name remote))
         (path (format "repos/%s/%s/releases" user-org project-name))
         (resp (github-get-path path)))
    (make-github-pageable resp)))

;;
;; Lib
;;

(defun call-git-process (&rest args)
  (with-temp-buffer
    (apply #'call-process "git" nil (current-buffer) nil args)
    (buffer-string)))

(defun git-commit (&optional ref)
  "Returns the long SHA for REF."
  (let* ((ref (if ref ref "HEAD"))
	     (sha (call-git-process "rev-parse" ref)))
    (string-clean-whitespace sha)))

(defun git-root ()
  "Returns an absolute path to the project root that contains the
nearest .git folder."
  (let ((root (call-git-process "rev-parse" "--show-toplevel")))
    (string-clean-whitespace root)))

(defun git-remotes ()
  (split-string (call-git-process "remote" "-v") "\n"))

(defun print-or-return (print val)
  "Helper function to either print VAL to minibuffer and copy to kill
ring if called interactively, or simply return VAL otherwise."
  (if print
      (progn
	    (kill-new val)
	    (message val))
    val))

(defun github-remotes ()
  "Get the GitHub (fetch) remote repositories for the current file as
(NAME . URL) pairs. Works with https and ssh URLs."
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
      (let* ((remotes (git-remotes))
             (gh-remotes (mapcar (lambda (remote) (extract remote)) remotes)))
        (cl-remove-if (lambda (item) (is-nil item)) gh-remotes)))))

(defun github-remote (&optional handle)
  "Get one GitHub (fetch) remote repository for the current file.
Works with https and ssh URLs. HANDLE defaults to origin."
  (interactive (list (read-string "Handle: " "origin")))
  (cl-flet ((lookup-handle (handle) (assoc handle (github-remotes))))
    (let* ((handle (if handle handle "origin"))
           (res (cdr (lookup-handle handle))))
      (print-or-return (called-interactively-p 'any) res))))

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
    (string-trim-right (extract-from url) ".git[ \t\n\r]*")))

(defun github-commit-url-format-string (commit)
  "Builds a format string for COMMIT, in long SHA form."
  (let* ((remote (github-remote))
	     (user-org (github-user-org remote))
         (project-name (github-project-name remote))
	     (trailing-slash (if (string= (substring github-base-url -1) "/") "" "/")))
    (concat github-base-url trailing-slash user-org "/" project-name "/%s/" commit)))

;;
;; Permalinks
;;

(defun github-project-permalink ()
  "Returns a permalink to the HEAD of the current file's project's
ref. If called interactively, will print to minibuffer and copy to
kill-ring. Otherwise, returns the link as a string."
  (interactive)
  (let* ((commit (git-commit))
         (format-string (github-commit-url-format-string commit))
         (formatted (format format-string "tree")))
    (print-or-return (called-interactively-p 'any) formatted)))

(defun github-file-permalink ()
  "Returns a permalink to the HEAD of the current file. If called
interactively, will print to minibuffer and copy to kill-ring.
Otherwise, returns the link as a string."
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((current-commit (git-commit))
           (root (git-root))
	       (trimmed (string-trim-left buffer-file-name root))
           (format-string (github-commit-url-format-string current-commit))
           (formatted (format format-string "blob"))
           (link (concat formatted trimmed)))
      (print-or-return (called-interactively-p 'any) link))))

(defun github-permalink-at-point ()
  "Returns a permalink to the HEAD of the current file at the line
under point, or the active region as a range. If called interactively,
will print to minibuffer and copy to kill-ring. Otherwise, returns the
link as a string."
  (interactive)
  (with-current-buffer (current-buffer)
    (cl-flet ((get-line-numbers () (if (use-region-p)
				                       (mapcar 'line-number-at-pos (list (region-beginning) (region-end)))
				                     (list (line-number-at-pos))))
	          (build-permalink (linenos)
			                   (let ((begin (car linenos))
				                     (end (cadr linenos)))
				                 (concat (github-file-permalink) "#L" begin (if end (concat "-L" end))))))
      (let* ((linenos (get-line-numbers))
             (linenos-str (mapcar 'number-to-string linenos))
             (permalink (build-permalink linenos-str)))
        (print-or-return (called-interactively-p 'any) permalink)))))

;;
;; Table
;;

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
  "Given a sequence of hashtables ROWS, display COLUMNS-TO-KEEP in 
BUFFER. Clicking on or pressing ENTER on any text will open the
respective page in the browser. MODE should be a quoted variable of a
major mode that extends tabulated-list-mode."
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
										                                           (cast-value (if (numberp value)
                                                                                                   (number-to-string value)
                                                                                                 value)))
									                                          (cons
									                                           cast-value
									                                           (list
										                                        'action `(lambda (x)
                                                                                           (visit-url-in-browser (gethash "html_url" ,row)))))))
									                                      column-header-names)))))
						                            rows))))
	    (erase-buffer)
	    (setq tabulated-list-entries this-tabulated-list-entries)
	    (funcall mode)
	    (goto-char (point-min))
	    (display-buffer buffer)))))

(defclass github-table ()
  ((pageable
    :initarg :pageable
    :accessor pageable)
   (table-buffer
    :initarg :table-buffer
    :accessor table-buffer)
   (table-columns
    :initarg :table-columns
    :accessor table-columns)
   (table-mode
    :initarg :table-mode
    :accessor table-mode))
  (:documentation "Represents the state of a displayed table."))

(cl-defgeneric github-table-next (table)
  (:documentation "Get the next TABLE."))

(cl-defgeneric github-table-prev (table)
  (:documentation "Get the previous TABLE."))

(cl-defgeneric github-table-first (table)
  (:documentation "Get the first TABLE."))

(cl-defgeneric github-table-last (table)
  (:documentation "Get the last TABLE."))

(cl-defgeneric github-table-display-table-in-buffer (table)
  (:documentation "Display the TABLE in TABLE's TABLE-BUFFER keeping
only TABLE-COLUMNS. TABLE-MODE is a quoted symbol of a mode derived
from tabulated-list-mode."))

(cl-defmethod github-table-display-table-in-buffer ((table github-table))
  (display-table-in-buffer
   (table-buffer table)
   (page (pageable table))
   (table-columns table)
   (table-mode table)))

(cl-flet ((make-if (tbl pf)
		           (let ((np (github-get-pageable-if (funcall pf (pageable tbl)))))
		             (when np
		               (make-instance github-table
			                          :pageable np
			                          :table-buffer (table-buffer tbl)
			                          :table-columns (table-columns tbl)
			                          :table-mode (table-mode tbl))))))
  (cl-defmethod github-table-next ((table github-table))
    (make-if table 'next-link))
  (cl-defmethod github-table-prev ((table github-table))
    (make-if table 'prev-link))
  (cl-defmethod github-table-first ((table github-table))
    (make-if table 'first-link))
  (cl-defmethod github-table-last ((table github-table))
    (make-if table 'last-link)))

;;
;; Issues
;;

(defvar github-project-issues-tabulated-list-format
  [("number" 20 t) ;; most-positive-fixnum string repr on my system has length 19
   ("created_at" 21 t) ;; pretty sure the timestamp format always has length 20
   ("title" 256 t)]    ;; 256 length was chosen arbitrarily
  )

(define-derived-mode github-issues-mode tabulated-list-mode "github-issues-mode" "Major mode for viewing GitHub Issues."
  (setq tabulated-list-format github-project-issues-tabulated-list-format) 
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defvar github-issues-current-table nil)

(cl-flet ((handler (f)
		           (lambda ()
		             (interactive)
		             (let ((nt (funcall f github-issues-current-table)))
		               (when nt
			             (github-table-display-table-in-buffer nt) 
			             (setq github-issues-current-table nt))))))
  (define-key github-issues-mode-map (kbd "p") (handler 'github-table-prev))
  (define-key github-issues-mode-map (kbd "n") (handler 'github-table-next))
  (define-key github-issues-mode-map (kbd "f") (handler 'github-table-first))
  (define-key github-issues-mode-map (kbd "l") (handler 'github-table-last)))

(defun github-project-issues (&optional handle &rest kwargs)
  "List all open issues in buffer named *GitHub Issues*."
  (interactive (list (read-string "Handle: " "origin")))
  (let* ((buffer (or (plist-get kwargs :buffer)
                     (get-buffer-create "*GitHub Issues*")))
	     (columns-to-keep (mapcar (lambda (item)
				                    (car item))
				                  github-project-issues-tabulated-list-format))
	     (pageable (github-get-issues (github-remote handle)))
	     (tbl (make-instance github-table
			                 :pageable pageable
			                 :table-buffer buffer
			                 :table-columns columns-to-keep
			                 :table-mode 'github-issues-mode)))
    (github-table-display-table-in-buffer tbl)
    (setq github-issues-current-table tbl)))

;;
;; Pull Requests
;;

(defvar github-project-pull-requests-tabulated-list-format
  [("number" 20 t) ;; most-positive-fixnum string repr on my system has length 19
   ("created_at" 21 t) ;; pretty sure the timestamp format always has length 20
   ("title" 256 t)]    ;; 256 length was chosen arbitrarily
  )

(define-derived-mode github-pull-requests-mode tabulated-list-mode "github-pull-requests-mode" "Major mode for viewing GitHub Pull Requests."
  (setq tabulated-list-format github-project-pull-requests-tabulated-list-format) 
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defvar github-pull-requests-current-table nil)

(cl-flet ((handler (f)
		           (lambda ()
		             (interactive)
		             (let ((nt (funcall f github-pull-requests-current-table)))
		               (when nt
			             (github-table-display-table-in-buffer nt) 
			             (setq github-pull-requests-current-table nt))))))
  (define-key github-pull-requests-mode-map (kbd "p") (handler 'github-table-prev))
  (define-key github-pull-requests-mode-map (kbd "n") (handler 'github-table-next))
  (define-key github-pull-requests-mode-map (kbd "f") (handler 'github-table-first))
  (define-key github-pull-requests-mode-map (kbd "l") (handler 'github-table-last)))

(defun github-project-pull-requests (&optional handle &rest kwargs)
  "List all open Pull Requests in buffer named *GitHub Pull Requests*."
  (interactive (list (read-string "Handle: " "origin")))
  (let* ((buffer (or (plist-get kwargs :buffer)
                     (get-buffer-create "*GitHub Pull Requests*")))
	     (columns-to-keep (mapcar (lambda (item)
				                    (car item))
				                  github-project-pull-requests-tabulated-list-format))
	     (pageable (github-get-pull-requests (github-remote handle)))
	     (tbl (make-instance github-table
			                 :pageable pageable
			                 :table-buffer buffer
			                 :table-columns columns-to-keep
			                 :table-mode 'github-pull-requests-mode)))
    (github-table-display-table-in-buffer tbl)
    (setq github-issues-current-table tbl)))

;;
;; Releases
;;

(defvar github-project-releases-tabulated-list-format
  [("id" 20 t) ;; most-positive-fixnum string repr on my system has length 19
   ("created_at" 21 t) ;; pretty sure the timestamp format always has length 20
   ("tag_name" 16 t)   ;; 16 length was chosen arbitrarily
   ("name" 16 t)       ;; 16 length was chosen arbitrarily
   ("body" 128 t)]     ;; 128 length was chosen arbitrarily
  )

(define-derived-mode github-releases-mode tabulated-list-mode "github-releases-mode" "Major mode for viewing GitHub Releases."
  (setq tabulated-list-format github-project-releases-tabulated-list-format) 
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tabulated-list-print t))

(defvar github-releases-current-table nil)

(cl-flet ((handler (f)
		           (lambda ()
		             (interactive)
		             (let ((nt (funcall f github-releases-current-table)))
		               (when nt
			             (github-table-display-table-in-buffer nt) 
			             (setq github-releases-current-table nt))))))
  (define-key github-releases-mode-map (kbd "p") (handler 'github-table-prev))
  (define-key github-releases-mode-map (kbd "n") (handler 'github-table-next))
  (define-key github-releases-mode-map (kbd "f") (handler 'github-table-first))
  (define-key github-releases-mode-map (kbd "l") (handler 'github-table-last)))

(defun github-project-releases (&optional handle &rest kwargs)
  "List all Releases in buffer named *GitHub Releases*."
  (interactive (list (read-string "Handle: " "origin")))
  (let* ((buffer (or (plist-get kwargs :buffer)
                     (get-buffer-create "*GitHub Releases*")))
	     (columns-to-keep (mapcar (lambda (item)
				                    (car item))
				                  github-project-releases-tabulated-list-format))
	     (pageable (github-get-releases (github-remote handle)))
	     (tbl (make-instance github-table
			                 :pageable pageable
			                 :table-buffer buffer
			                 :table-columns columns-to-keep
			                 :table-mode 'github-releases-mode)))
    (github-table-display-table-in-buffer tbl)
    (setq github-releases-current-table tbl)))

(provide 'github)
