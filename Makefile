EMACS ?= emacs

ELS = plan-polsl.el \
      plan-polsl-http.el \
      plan-polsl-parser.el \
      plan-polsl-search.el \
      plan-polsl-org.el \
      plan-polsl-view.el \
      plan-polsl-ui.el

ELCS = $(ELS:.el=.elc)

.PHONY: all compile test clean

all: compile

compile: $(ELCS)

%.elc: %.el
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

test: compile
	$(EMACS) -Q --batch -L . -l plan-polsl.el --eval '(progn (message "All modules loaded successfully!"))'

clean:
	rm -f *.elc *-autoloads.el *.cache
