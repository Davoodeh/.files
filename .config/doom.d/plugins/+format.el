;;; plugins/+format.el -*- lexical-binding: t; -*-

(setq +format-on-save-enabled-modes
      '(not sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode))

(set-formatter! 'shfmt (concat "shfmt -ci -i 4 -ln "
                               (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
                                        (bash "bash")
                                        (mksh "mksh")
                                        (t "posix")))
                :modes '(sh-mode))

(setq-hook! 'lisp-mode-hook +format-with 'cljfmt)
(setq-hook! 'emacs-lisp-mode-hook +format-with 'cljfmt)
