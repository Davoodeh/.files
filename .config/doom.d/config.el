;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; -----
;;; Functions
(defun load-dir (dir) ; loads other dirs relative to DOOMDIR
  (setq dir (concat (file-name-directory (or load-file-name buffer-file-name)) dir))
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun set-lang (layout) ; changes keyboard layout
  (shell-command (concat "kblayout set " layout) nil nil))

(defun capsoff ()
  (shell-command "xset q | grep -q '.*Caps\sLock.*on' && xdotool key Caps_Lock" nil nil))

(defun set-bidi-env ()
  (interactive)
  (setq bidi-paragraph-direction 'nil))

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
;;; Modes and hooks
(add-hook 'find-file-hook 'auto-insert)
(general-evil-setup) ; enables General commands in this file
(auto-save-visited-mode)
(use-package! nyan-mode :config
              (nyan-mode)
              ;; (nyan-start-animation)
              (nyan-toggle-wavy-trail))
(use-package! tex-fold :defer t :config
              (dolist (item '(; Custom commands
                              ("𝕋" ("T"))
                              ("𝔽" ("F"))
                              ("[r]" ("reft"))
                              ;; Xepersian
                              ("[f]" ("LTRfootnote"))
                              ;; Default commands
                              ("∧" ("land"))
                              ("∨" ("lor"))
                              ("￢" ("lnot"))
                              ("―――" ("hline"))))
                      (add-to-list 'LaTeX-fold-math-spec-list item))
              (add-hook 'after-save-hook #'TeX-fold-buffer))
(set-ligatures! 'emacs-lisp-mode 'scheme-mode
                ;; Functional
                :lambda        "lambda"
                :def           "defun"
                ;; Types
                :null          "null"
                :true          "t"
                :true          "T"
                :false         "nil"
                ;; Flow
                :not           "not"
                :in            "in"
                :and           "and"
                :or            "or"
                :for           "for"
                ;; Other
                :dot           ".")
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
 '(doom-font (font-spec :family "Iosevka" :size 16 :inherit '(italic bold)))
 ;; '(line-spacing 8)
 '(lsp-log-io t) ; shows lsp-log in an accessible buffer
 '(lsp-keymap-prefix "M-l") ; for Window Managers that use the Super key
 '(display-line-number t)
 '(display-line-numbers-type 'relative)
 '(display-time-default-load-average 3) ; shows a simple clock (load time not included)
 '(display-time-mode t) ; adds a clock to the bar
 '(doom-theme 'doom-solarized-dark) ; default theme
 '(global-display-line-numbers-mode) ; enables line number mode
 '(global-prettify-symbols-mode) ; disable it already I know you all hate it (why?)
 '(treemacs-position (quote right)))

(setq auto-insert-alist '(; insers skeletons automatically
                          ("\\.sent\\'" . sent-skeleton)
                          ("\\.snt\\'"  . sent-skeleton)))
(setq auto-mode-alist (append '(; opens specific extensions with given modes
                                ("\\.sent\\'" . org-mode)
                                ("\\.snt\\'"  . org-mode))
                              auto-mode-alist))
;; (setq company-idle-delay nil) ; disables Company-Auto-Completion (makes Emacs WAY, WAY smoother!)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
(setq-default TeX-engine 'xetex)
(after! ob-php
        (defun org-babel-execute:php (body params)
          "Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'."
          (let* ((cmd (concat org-babel-php-command " " org-babel-php-command-options)))
                (org-babel-eval cmd body))))
(after! org
        (setq org-preview-latex-default-process 'xdvisvgm)
        (add-to-list 'org-preview-latex-process-alist '(xdvisvgm
                                                        :programs ("xelatex" "dvisvgm")
                                                        :description "xdv > svg"
                                                        :message "you need to install the programs: xelatex and dvisvgm."
                                                        :image-input-type "xdv"
                                                        :image-output-type "svg"
                                                        :image-size-adjust (1.7 . 1.5)
                                                        :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                                                        :image-converter ("dvisvgm %f -n -b min -c %S -o %O")))
        (after! ox-latex
                (add-to-list 'org-latex-classes '("xa4article"
                                                  "\\documentclass[a4paper]{article}\n"
                                                  ("\\section{%s}" . "\\section*{%s}")
                                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
                (setq
                 org-latex-bib-compiler "biber"
                 org-latex-caption-above '(table src-block)
                 org-latex-compiler "xelatex"
                 org-latex-default-class "xa4article"
                 org-latex-listings 'minted
                 org-latex-minted-options '(("linenos") ("mathescape"))
                 org-latex-toc-command "\\tableofcontents\n\\clearpage"
                 org-latex-pdf-process (mapcar (lambda (s)
                                                       (replace-regexp-in-string "%latex " "%latex -shell-escape " s))
                                               org-latex-pdf-process) ; -shell-escape needed for minted
                 org-latex-packages-alist '("\\usepackage[pass]{geometry}"
                                            "\\usepackage{fontspec, xpatch, fullpage, float, xcolor, titling}"
                                            "\\usepackage[newfloat]{minted}"))))
(setq +format-on-save-enabled-modes
      '(not sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode))
(set-formatter! 'shfmt (concat "shfmt -bn -ci -sr -i 4 -ln "
                               (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
                                        (bash "bash")
                                        (mksh "mksh")
                                        (t "posix")))
                :modes '(sh-mode))
(setq-hook! 'lisp-mode-hook +format-with 'cljfmt)
(setq-hook! 'emacs-lisp-mode-hook +format-with 'cljfmt)
(setq geiser-default-implementation 'racket)
(add-hook 'org-mode-hook 'set-bidi-env)
;;; -----

;;; -----
;;; Binds
;; XXX Why on the earth it's impossible to use a var as input for general-imap?
;; TODO make all of it only one function
(when (featurep! :editor evil)
  (general-imap "j" (general-key-dispatch 'self-insert-command :timeout 0.25 "j" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-imap "k" (general-key-dispatch 'self-insert-command :timeout 0.25 "k" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-imap "ت" (general-key-dispatch 'self-insert-command :timeout 0.25 "ت" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-imap "о" (general-key-dispatch 'self-insert-command :timeout 0.25 "о" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
  (general-imap "J" (general-key-dispatch 'self-insert-command :timeout 0.25 "J" '(lambda () (interactive) (evil-normal-state) (capsoff))))
  (general-imap "K" (general-key-dispatch 'self-insert-command :timeout 0.25 "K" '(lambda () (interactive) (evil-normal-state) (capsoff))))
  (general-nmap "о" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
  (general-nmap "л" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
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
