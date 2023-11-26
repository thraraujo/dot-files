   ;; Some Useful keybindings
   ;; C-x C-f - find file
   ;; C-x C-s - save file
   ;; C-M-x - load expression 

   ;; I can find information about the variables, functions using
   ;; C-h v -- variables
   ;; C-h f -- funtions
   ;; C-h o -- describe symbols



;; Initial Definitions
(setq inhibit-startup-message t)                                       ; don't show the splash screen
(setq visible-bell t)				                       ; flash when the bell rings
						                       
;; Some UI elements				                       
(tool-bar-mode -1)                                                     ; turn off the toolbar
(menu-bar-mode -1)                                                     ; turn off the menubar
(scroll-bar-mode -1)                                                   ; turn off scrollbar
(blink-cursor-mode -1)                                                 ; turn off blink cursor
(hl-line-mode 1)                                                       ; highlight line
(global-display-line-numbers-mode 1)                                   ; turn on line numbers
(setq display-line-numbers 'relative)                                  ; relative numbers
						                       
;; Useful modes					                       
(recentf-mode 1)                                                       ; recent files
(save-place-mode 1)                                                    ; remember and restore the last cursor location of open files
(evil-mode 1)                                                          ; activate evil-mode
(icomplete-mode 1)                                                     ; completion in the minibuffers
						                       
(savehist-mode 1)                                                      ; remembering what I type in minibuffers
(setq history-length 25)                                               ; set this hystoty to 25

;; We want to avoid emacs messing with this file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil)                                              ; they are annoying
						                       
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
      '((1 . (rainbow overline background 1.4))
	(2 . (rainbow background 1.3))
	(3 . (rainbow bold 1.2))
	(t . (semilight 1.1)))
(setq modus-themes-scale-headings t)

;(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-org-blocks 'tinted-background)

;; Load theme
(load-theme 'modus-vivendi t)                                          ; I can toggle between the light and dark version with M-x modus-theme-toggle

