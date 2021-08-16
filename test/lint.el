(message "Emacs version: %s" emacs-version)

(dolist (file (file-expand-wildcards "*.el"))
  (let ((checkdoc-diagnostic-buffer "*warn*")
        ;; bail out on checkdoc warnings
        (checkdoc-create-error-function (lambda (text start end &optional unfixable)
                                          (message "%s:%s %s"
                                                   file
                                                   (line-number-at-pos start)
                                                   text)
                                          (kill-emacs 1))))
    (with-current-buffer (find-file-noselect file)
      ;; Eval the buffer first because otherwise checkdoc isn't smart
      ;; enough to recognize that some symbols are defined.
      (eval-buffer)
      (checkdoc-current-buffer t))))
