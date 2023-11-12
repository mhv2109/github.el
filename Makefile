# Based on:  https://github.com/skeeto/bitpack/blob/a082de045d91340a009158b4963a374815807dc6/Makefile
# See also: https://nullprogram.com/blog/2020/01/22/

.POSIX:
EMACS = emacs

compile: github.elc github-tests.elc

github.elc: github.el
github-tests.elc: github-tests.el github.elc

clean:
	rm -f *.elc

test: github-tests.elc
	$(EMACS) --batch -Q -L . -l github-tests.elc -f ert-run-tests-batch-and-exit

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<
