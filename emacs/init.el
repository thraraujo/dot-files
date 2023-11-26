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




;; Initial Definitions
(setq inhibit-startup-message t)                                       ; don't show the splash screen
(setq visible-bell t)				                       ; flash when the bell rings
						                       
;; Some UI elements				                       
(tool-bar-mode -1)                                                     ; turn off the toolbar
(menu-bar-mode -1)                                                     ; turn off the menubar
(scroll-bar-mode -1)                                                   ; turn off scrollbar
(blink-cursor-mode -1)                                                 ; turn off blink cursor
(hl-line-mode 1)                                                       ; highlight line
(setq fringe-mode 10)

(column-number-mode)
(global-display-line-numbers-mode t)                                   ; line numberting
;(setq display-line-numbers 'relative)                                 ; relative numbers

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
						                       
(use-package rainbow-delimiters                                        ; make delimiters colorful
  :hook (prog-mode . rainbow-delimiters-mode))


;; Fonts
(set-face-attribute 'default nil :font "Fira Code Nerd Font")

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell")

;; Set frame transparency
;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Useful modes					                       
(recentf-mode 1)                                                       ; recent files
(save-place-mode 1)                                                    ; remember and restore the last cursor location of open files
(evil-mode 1)                                                          ; activate evil-mode
(icomplete-mode 1)                                                     ; completion in the minibuffers
						                       
(savehist-mode 1)                                                      ; remembering what I type in minibuffers
(setq history-length 25)                                               ; set this hystoty to 25

;; We want to avoid emacs messing with this file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))           ; emacs save modification elsewhere
(load custom-file 'noerror 'nomessage)                                 ; it loads the custom file and does't mess with me

(setq use-dialog-box nil)                                              ; they are annoying

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backup")))     ; save backup files in ~/.emacs.d/emacs-backup/
						                       
;; Automatically revert buffers for changed files                      
(global-auto-revert-mode 1)                                            ; now emacs watches what happens to the files so that they are updated when changed outside emacs
(setq global-auto-revert-non-file-buffers t)                           ; the dired is updated as well


;; Theme Configuration: Modus Vivendi - before loading theme
(setq modus-themes-mode-line '(accented borderless))                   ; I didn't like the padded
(setq modus-themes-region '(bg-only))                                  ; the selected region is purple -- the accented is also cool but does not preserve the code highlights
(setq modus-themes-completions 'optionated)

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-syntax '(faint))
(setq modus-themes-syntax '(alt-syntax faint))
(setq modus-themes-syntax '(yellow-comments))                          ; might try green-strings, but the blue strings look better

(setq modus-themes-headings
      '((1. (rainbow overline background 1.4))
	(2. (rainbow background 1.3))
	(3. (rainbow bold 1.2))
	(t. (semilight 1.1))))
(setq modus-themes-scale-headings t)

;(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-org-blocks 'tinted-background)

;; Load theme
(load-theme 'modus-vivendi t)                                          ; I can toggle between the light and dark version with M-x modus-theme-toggle

;; Doom Modeline - just to make the modeline prettier
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
         ("C-r" . 'counsel-minibuffer-history))
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

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; We need to need to install the icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))


;; Which-key package is helpful for suggestions
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))



