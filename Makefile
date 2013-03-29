EMACS?=emacs

TEMPFILE=temp.el

all: $(TEMPFILE) cryptol-mode.el
	@$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f cryptol-mode-compile
	@rm -f $(TEMPFILE)

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' >> $(TEMPFILE)
	@echo '(defun cryptol-mode-compile () (mapcar (lambda (x) (byte-compile-file x))' >> $(TEMPFILE)
	@echo '  (list "cryptol-mode.el")))' >> $(TEMPFILE)

clean:
	@rm -f $(TEMPFILE) *.elc *~
