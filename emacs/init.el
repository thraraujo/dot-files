(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; Some custom functions

(defun eval-init ()
  "Function to evaluate the init file"
  (interactive)
  (setq my-buffer (find-file-noselect "/home/thiago/.config/dot-files/emacs/init.el"))
  (eval-buffer my-buffer)
  (message "init file reevaluated"))

