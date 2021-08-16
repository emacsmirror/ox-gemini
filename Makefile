EMACS =		emacs

update:
	${EMACS} -batch -l test/make-update.el

compile: clean lint
	${EMACS} --version
	${EMACS} -batch -l test/elpa.el -l test/compile.el

clean:
	rm -f *.elc

lint:
	${EMACS} --batch -l test/lint.el

.PHONY: update compile test clean
