(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; Some custom functions

(defun eval-init ()
  "Function to evaluate the init file."
  (interactive)
  (setq my-buffer (find-file-noselect "~/.config/dot-files/emacs/init.el"))
  (eval-buffer my-buffer)
  (message "init file reevaluated"))

(defun open-config ()
  "Function to open the config file."
  (interactive)
  (find-file "~/.config/dot-files/emacs/config.org"))
