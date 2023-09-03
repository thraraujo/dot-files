;; Recommended minimal starting Emacs Init File
;; extracted from the book:
;; /Use GNU Emacs: The Plain Text Computing Environment/
;; by Keith Waclena
;; https://www.lib.uchicago.edu/keith/emacs/
;;
(require 'package)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(when (version< emacs-version "26.3")
  ;; older emacsen < 26.3 may need this
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(with-eval-after-load 'package
  (dolist (arc '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                 ("melpa" . "https://melpa.org/packages/")))
    (add-to-list 'package-archives arc t))
  (setq package-archive-priorities
        '(("gnu" . 10) ("nongnu" . 9) ("melpa-stable" . 8) ("melpa" . 7))))

;; thanks to an anonymous EmacsWiki coder
(defun undo-yank (arg)
  "Undo the yank you just did.  Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "C-M-Y") 'undo-yank)

(setq enable-recursive-minibuffers t)

(with-eval-after-load 'chistory
  (setq list-command-history-max 120)
  (define-key command-history-map (kbd "<return>") 'command-history-repeat))

(setq completion-styles '(partial-completion substring flex))
(unless (package-installed-p 'vertico)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'vertico)))
(with-demoted-errors "%s" (vertico-mode +1))

(unless (package-installed-p 'marginalia)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'marginalia)))
(with-demoted-errors "%s" (marginalia-mode +1))

(unless (package-installed-p 'windmove)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'windmove)))
;; <S-{left,right,up,down}> switches windows
(with-demoted-errors "%s" (windmove-default-keybindings))

(winner-mode 1) ; undo window config changes
;; add more felicitous bindings
(define-key winner-mode-map [(control c) (control left)] 'winner-undo)
(define-key winner-mode-map [(control c) (control right)] 'winner-redo)

(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

(setq large-file-warning-threshold (* 100 1024 1024)) ; 100MB

(when (version<= "27.1" emacs-version)  ; only available recently...
  (global-so-long-mode +1))    ; speed up long lines

(setq view-read-only t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

(add-hook 'dired-load-hook (lambda () (require 'dired-x)))

(setq dired-dwim-target t)  ; suggest other visible dired buffer

(unless (package-installed-p 'wgrep)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'wgrep)))

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

(desktop-save-mode 1)   ; restore files from previous session

(save-place-mode 1)   ; come back to where we were in that file

(global-set-key (kbd "C-+") 'text-scale-adjust) ; embiggen font

(setq-default indent-tabs-mode nil)      ; don't insert tabs

(setq async-shell-command-buffer 'new-buffer) ;multiple async commands ok!
(setq async-shell-command-display-buffer nil) ;don't pop up the buffer

(setq comint-buffer-maximum-size 65336) ; must be able to cat War and Peace!
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; goto-address-mode is handy in these modes
(dolist (hook '(shell-mode-hook eshell-mode-hook))
  (add-hook hook #'goto-address-mode))
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

(setq calendar-mark-holidays-flag t        ; colorize holidays in the calendar
      calendar-mark-diary-entries-flag t)  ; also diary entries

(setq org-agenda-include-diary t)  ; incorporate the diary into the agenda

(appt-activate +1)        ; appointment notifications, please
(require 'notifications)  ; also via desktop notifications

;; don't use a separate Frame for the control panel
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; horizontal split is more readable
(setq ediff-split-window-function 'split-window-horizontally)

;; restore window config upon quitting ediff
(defvar ue-ediff-window-config nil "Window config before ediffing.")
(add-hook 'ediff-before-setup-hook
          (lambda ()
            (setq ue-ediff-window-config (current-window-configuration))))
(dolist (hook '(ediff-suspend-hook ediff-quit-hook))
  (add-hook hook
            (lambda ()
              (set-window-configuration ue-ediff-window-config))))


;; MY CONFIG 
(require 'evil)
(evil-mode 1)

(global-display-line-numbers-mode 1)
(setq global-display-line-numbers 'relative)

(setq initial-buffer-choice "~/start.org")

(setq scroll-conservatively most-positive-fixnum)
(setq make-backup-files nil) ; stop creating ~ files

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'lsp-mode)
(add-hook 'latex-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(add-hook 'after-init-hook 'global-company-mode) ; autocompletion
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;; I think that it is the configuration to execute python codes in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(load-theme 'dracula)


