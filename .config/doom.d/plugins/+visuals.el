;;; plugins/+visuals.el -*- lexical-binding: t; -*-

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

(when (featurep! :ui ligatures)
  (set-ligatures! 'emacs-lisp-mode 'scheme-mode
                ;; Functional
                  :lambda        "lambda"
                  :def           "defun"
                ;; Types
                  :null          "null"
                  :true          "t"
                  :true          "T"
                  :true          "#t"
                  :true          "#T"
                  :false         "nil"
                  :false         "#nil"
                  :false         "#f"
                  :list          "list"
                ;; Flow
                  :not           "not"
                  :in            "in"
                  :and           "and"
                  :or            "or"
                  :for           "for"
                ;; Other
                  :dot           "."))
