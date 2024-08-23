(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;; SOME CUSTOM FUNCTIONS

(defun th-eval-init ()
  "Function to evaluate the init file."
  (interactive)
  (setq my-buffer (find-file-noselect "~/.config/dot-files/emacs/init.el"))
  (eval-buffer my-buffer)
  (message "init file reevaluated"))

(defun th-open-config ()
  "Function to open the config file."
  (interactive)
  (find-file "~/.config/dot-files/emacs/config.org"))

(defun th-wiki ()
  "Function to open the wiki directory."
  (interactive)
  (find-file "~/Sync/projects/wiki"))

(defun th-work ()
  "Function to open the work directory."
  (interactive)
  (find-file "~/Sync/projects/work"))

(defun th-dot-files ()
  "Function to open my dot files"
  (interactive)
  (find-file "~/.config/dot-files"))

(defun th-scripts ()
  "Function to open my dot files"
  (interactive)
  (find-file "~/.config/scripts"))



;; Here I want to define a function that runs my git-status command

(defun generate-numbers ()
  (interactive)
  (dotimes (i 10)
    (print i)))

(defun show-numbers-in-buffer ()
  (interactive)
  (with-output-to-temp-buffer "*Output Buffer*"
    (generate-numbers)))
