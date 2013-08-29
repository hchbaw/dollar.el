MAINTAINERCLEANFILES = \
Makefile.in \
aclocal.m4 \
configure \
install-sh \
missing

EMACS := @emacs@

SOURCES = dollar.el

ELCFILES = $(SOURCES:.el=.elc)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . -f batch-byte-compile $<

all-local: $(ELCFILES)

CLEANFILES = $(ELCFILES)

.PHONY: test

test: all
	$(EMACS) --batch -Q -L . -L t -l test.el \
--eval "(progn (dollar-test-initialize) (ert-run-tests-batch-and-exit))"
