;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'github)

(defun read-file-to-string (filepath)
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(ert-deftest test-gitub-remotes ()
  "Tests that github-remotes can properly return the github.com Git remote repository for this very project."
  (should (equal (github-remotes) '(("origin" . "https://github.com/mhv2109/github.el")))))

(ert-deftest test-github-remote ()
  "Tests that github-remote can properly return the github.com Git remote repository for this very project."
  (should (equal (github-remote) "https://github.com/mhv2109/github.el")))

(ert-deftest test-github-user-org ()
  "Tests that github-user-org can properly return the username for this very project."
  (should (equal (github-user-org "https://github.com/mhv2109/github.el") "mhv2109"))
  (should (equal (github-user-org "git@github.com:mhv2109/github.el.git") "mhv2109")))

(ert-deftest test-github-project-name ()
  "Tests that github-project-name can properly return the project name for this very project."
  (should (equal (github-project-name "https://github.com/mhv2109/github.el") "github.el"))
  (should (equal (github-project-name "git@github.com:mhv2109/github.el.git") "github.el")))

(ert-deftest test-github-pageable ()
  (let* ((headers (read-file-to-string "./testdata/issues_headers.txt"))
	     (body (read-file-to-string "./testdata/issues.json"))
	     (pageable (make-github-pageable headers body)))
    (should (equal (next-link pageable) "https://api.github.com/repositories/1300192/issues?page=3"))
    (should (equal (prev-link pageable) "https://api.github.com/repositories/1300192/issues?page=1"))
    (should (equal (first-link pageable) "https://api.github.com/repositories/1300192/issues?page=1"))
    (should (equal (last-link pageable) "https://api.github.com/repositories/1300192/issues?page=536"))
    (should (equal (type-of (page pageable)) 'vector)))
  (let* ((body (read-file-to-string "./testdata/issues.json"))
	     (pageable (make-github-pageable "" body)))
    (should (equal (next-link pageable) nil)))
  (let* ((headers (read-file-to-string "./testdata/empty_issues_headers.txt"))
	     (body (read-file-to-string "./testdata/empty_issues.json"))
	     (pageable (make-github-pageable headers body)))
    (should (not (next-link pageable)))
    (should (not (prev-link pageable)))
    (should (not (first-link pageable)))
    (should (not (last-link pageable)))
    (should (equal (page pageable) [])))
  (let* ((headers (read-file-to-string "./testdata/issues_headers.txt"))
	     (body (read-file-to-string "./testdata/issues.json"))
         (pageable (make-github-pageable headers body))
         (pageable-resp (make-github-pageable-from-resp (list :headers headers :body body))))
    (should (and ;; TODO: compare vectors of hashtables
                 (equal (next-link pageable) (next-link pageable-resp))
                 (equal (prev-link pageable) (prev-link pageable-resp))
                 (equal (last-link pageable) (last-link pageable-resp))))))

(provide 'github-tests)
