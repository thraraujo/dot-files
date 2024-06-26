;; Some Useful keybindings
;; C-x C-f - find file
;; C-x C-s - save file
;; C-M-x - load expression 

;; I can find information about the variables, functions using
;; C-h v -- variables
;; C-h f -- funtions
;; C-h o -- describe symbols

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			            ("melpa-stable" . "http://stable.melpa.org/packages/") 
                        ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Evil Mode
(evil-mode 1)     
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))


;; Fonts
(set-face-attribute 'default nil :font "Fira Code Nerd Font")
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font")
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code Nerd Font")

;; Initial Definitions
(setq inhibit-startup-message t)                                       ; don't show the splash screen
(setq visible-bell t)				                       ; flash when the bell rings

; For a better scrolling experience
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Some UI elements				                       
(tool-bar-mode -1)                                                     ; turn off the toolbar
(menu-bar-mode -1)                                                     ; turn off the menubar
(scroll-bar-mode -1)                                                   ; turn off scrollbar
(blink-cursor-mode -1)                                                 ; turn off blink cursor
(hl-line-mode 1)                                                       ; highlight line
(setq fringe-mode 20)

(column-number-mode)
(global-display-line-numbers-mode t)                                   ; line numberting
(setq display-line-numbers-type 'relative)                             ; relative numbers

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
		vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
		dired-mode-hook
		ibuffer-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
						                       
(use-package rainbow-delimiters                                        ; make delimiters colorful
  :hook (prog-mode . rainbow-delimiters-mode))

;; Some useful configurations
(recentf-mode 1)                                                       ; recent files
(save-place-mode 1)                                                    ; remember and restore the last cursor location of open files
(icomplete-mode 1)                                                     ; completion in the minibuffers
						                       
(savehist-mode 1)                                                      ; remembering what I type in minibuffers
(setq history-length 25)                                               ; set this hystoty to 25


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; copy paste more efficiently
(setq dired-dwim-target t)

;; We want to avoid emacs messing with this file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))           ; emacs save modification elsewhere
(load custom-file 'noerror 'nomessage)                                 ; it loads the custom file and does't mess with me

(setq use-dialog-box nil)                                              ; they are annoying

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backup")))     ; save backup files in ~/.emacs.d/emacs-backup/
						                       
;; Automatically revert buffers for changed files                      
(global-auto-revert-mode 1)                                            ; now emacs watches what happens to the files so that they are updated when changed outside emacs
(setq global-auto-revert-non-file-buffers t)                           ; the dired is updated as well

;; doom-themes repo: https://github.com/doomemacs/themes?tab=readme-ov-file#install
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;(setq doom-theme 'doom-city-lights)

;; Doom Modeline - just to make the modeline prettier
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq doom-modeline-project-detection 'auto)

;; Ivy and Counsel - This is for autocompletion in the minibuffer
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ;("C-r" . 'counsel-minibuffer-history)
	 )
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))


;; Which-key package is helpful for suggestions
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; We need to need to install the icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; org-mode config
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org-faces

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Fira Code Nerd Font" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Fira Code Nerd Font" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Documents/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

;; magit
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; pdf-tools
(use-package pdf-tools
   :defer t
   :config
       (pdf-tools-install)
       (setq-default pdf-view-display-size 'fit-page)
   :bind (:map pdf-view-mode-map
         ("\\" . hydra-pdftools/body)
         ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("l"  . image-forward-hscroll)
         ("h"  . image-backward-hscroll)
         ("j"  . pdf-view-next-page)
         ("k"  . pdf-view-previous-page)
         ("e"  . pdf-view-goto-page)
         ("u"  . pdf-view-revert-buffer)
         ("al" . pdf-annot-list-annotations)
         ("ad" . pdf-annot-delete)
         ("aa" . pdf-annot-attachment-dired)
         ("am" . pdf-annot-add-markup-annotation)
         ("at" . pdf-annot-add-text-annotation)
         ("y"  . pdf-view-kill-ring-save)
         ("i"  . pdf-misc-display-metadata)
         ("s"  . pdf-occur)
         ("b"  . pdf-view-set-slice-from-bounding-box)
         ("r"  . pdf-view-reset-slice)))



;; Treesitter
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c.git")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (latex "https://github.com/latex-lsp/tree-sitter-latex")
     (tex "https://github.com/latex-lsp/tree-sitter-latex")
    ))

(setq major-mode-remap-alist
 '((python-mode . python-ts-mode)
   (sh-mode . bash-ts-mode)
   (c-mode . c-ts-mode)
   ;;(latex-mode . latex-ts-mode) ; This does not work 
   ;;(elisp-mode . elisp-ts-mode)
    ))

;; LSP
(use-package lsp-mode)

(add-hook 'latex-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'markdown-mode-hook #'lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Languages
(setq lsp-tex-server 'digestif)

;; snippets
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;bookmark
(setq bookmark-default-file "~/.emacs.d/bookmarks")

;; The following code changes a tab to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.djvu\\'" "zathura" (file))))
(setq openwith-associations '(("\\.pdf\\'" "zathura" (file))))

;; Initial buffer
(setq initial-buffer-choice "~/Sync/projects/wiki/README.org")
