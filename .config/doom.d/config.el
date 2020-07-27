;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; -----
;;; Functions
(defun load-dir (dir) ; loads other dirs relative to DOOMDIR
  (setq dir (concat (or (getenv "DOOMDIR") "~/.doom.d") "/" dir))
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun set-lang (layout) ; changes keyboard layout
  (shell-command (concat "kblayout set " layout) nil nil))

(defun xterm-here () ; opens an external terminal in the working directory
  (interactive "@")
  (shell-command
   (concat "cd '" (file-name-directory (or (or load-file-name buffer-file-name) "")) "' ; exec nohup terminal >/dev/null 2>&1")
   nil nil))
;;; -----

;;; -----
;;; Extra files
(load-dir "skeletons")
;;; -----

;;; -----
;;; Modes
(general-evil-setup) ; enables General commands in this file
(auto-save-visited-mode)
;; (use-package! nyan-mode :config
;;               (nyan-mode)
;;               ;; (nyan-start-animation)
;;               (nyan-toggle-wavy-trail))
(use-package! tex-fold :defer t :config
              (add-hook 'after-save-hook #'TeX-fold-buffer))
;;; -----

;;; -----
;;; Custom
(custom-set-variables
 '(auto-insert-query nil)
 '(bidi-paragraph-direction nil) ; ensures Emacs' default behaviour
 ;; '(centaur-tabs-set-close-button nil)
 '(centaur-tabs-set-icons t)
 '(centaur-tabs-gray-out-icons 'buffer) ; grays out icons for unselected tabs
 '(centaur-tabs-height 20)
 '(doom-font (font-spec :family "Iosevka" :size 16 :inherit 'italic))
 '(doom-unicode-font (font-spec :family "Material Design Icons" :size 16))
 ;; '(line-spacing 8)
 ;; '(lsp-log-io t) ; shows lsp-log in an accessible buffer
 '(lsp-keymap-prefix "M-l") ; for Window Managers that use the Super key
 '(display-line-number t)
 '(display-line-numbers-type 'relative)
 '(display-time-default-load-average 3) ; shows a simple clock (load time not included)
 '(display-time-mode t) ; adds a clock to the bar
 '(doom-theme 'doom-solarized-dark) ; default theme
 '(global-display-line-numbers-mode) ; enables line number mode
 '(global-prettify-symbols-mode) ; disable it already I know you all hate it (why?)
 '(treemacs-position (quote right)))
(setq auto-insert-alist '( ; insers skeletons automatically
                          ("\\.sent\\'" . sent-skeleton)
                          ("\\.snt\\'"  . sent-skeleton)))
(setq auto-mode-alist (append '( ; opens specific extensions with given modes
                                ("\\.sent\\'" . org-mode)
                                ("\\.snt\\'"  . org-mode))
                              auto-mode-alist))
(setq company-idle-delay nil) ; disables Company-Auto-Completion (makes Emacs WAY smoother!)
;;; -----

;;; -----
;;; Binds
;; XXX Why on the earth it's impossible to use a var as input for general-imap?
;; TODO make all of it only one function
(when (featurep! :editor evil)
  (general-imap "j" (general-key-dispatch '(lambda () (interactive) (insert "j"))
      :timeout 0.25
      "j" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-imap "ت" (general-key-dispatch '(lambda () (interactive) (insert "ت"))
      :timeout 0.25
      "ت" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-nmap "ت" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
  (general-nmap "ن" (lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  ;; (add-hook 'evil-mode-hook ; FIXME doesn't work
  ;;       (lambda () (local-unset-key (kbd "M-i"))))
  ;; (general-nmap :prefix "M"
  ;;   "i" '(lambda ()
  ;;          (set-lang "fa")
  ;;          (evil-insert))))

(map! :leader
    :desc "Open an external terminal here" "o x" #'xterm-here)

(map! :leader :when (featurep! :ui tabs)
      :desc "Move tab to left"            "{" #'centaur-tabs-move-current-tab-to-left
      :desc "Move tab to right"           "}" #'centaur-tabs-move-current-tab-to-right
      :desc "Select the first tab"        "1" #'centaur-tabs-select-beg-tab
      :desc "Select 2nd tab"              "2" #'centaur-tabs-select-visible-tab
      :desc "Select 3rd tab"              "3" #'centaur-tabs-select-visible-tab
      :desc "Select 4th tab"              "4" #'centaur-tabs-select-visible-tab
      :desc "Select 5th tab"              "5" #'centaur-tabs-select-visible-tab
      :desc "Select 6th tab"              "6" #'centaur-tabs-select-visible-tab
      :desc "Select 7th tab"              "7" #'centaur-tabs-select-visible-tab
      :desc "Select 8th tab"              "8" #'centaur-tabs-select-visible-tab
      :desc "Select the last tab"         "9" #'centaur-tabs-select-end-tab
      :desc "Backward cycle through tabs" "[" #'centaur-tabs-backward
      :desc "Forward cycle through tabs"  "]" #'centaur-tabs-forward)
        ;; (:map global-map "C-TAB" #'centaur-tabs-forward) ; FIXME

;; TODO Add -c flag to magit-log and bind it to Spc-g-l-a
;;; -----

;;; -----
;;; Hooks
(add-hook 'find-file-hooks 'auto-insert)
;;; -----
