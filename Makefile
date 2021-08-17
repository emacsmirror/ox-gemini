EMACS =		emacs

compile: clean lint
	${EMACS} --version
	${EMACS} -batch -l test/elpa.el -l test/compile.el

clean:
	rm -f *.elc

lint:
	${EMACS} --batch -l test/lint.el

update:
	${EMACS} -batch -l test/make-update.el

.PHONY: update compile test clean
