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