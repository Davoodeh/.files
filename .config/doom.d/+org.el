;;; +org.el -*- lexical-binding: t; -*-

(add-hook 'org-mode-hook 'set-bidi-env)

(setq auto-mode-alist (append '(; opens specific extensions with given modes
                                ("\\.sent\\'" . org-mode)
                                ("\\.snt\\'"  . org-mode))
                              auto-mode-alist))

;; Make Babel Php much like real Php files require <?php and ?>.
;; This helps with Minted and correct highlighting.
(after! ob-php
        (defun org-babel-execute:php (body params)
          "Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'."
          (let* ((cmd (concat org-babel-php-command " " org-babel-php-command-options)))
                (org-babel-eval cmd body))))

(after! ox-latex
        (add-to-list 'org-latex-classes '("a4article"
                                          "\\documentclass[a4paper]{article}\n"
                                          ("\\section{%s}" . "\\section*{%s}")
                                          ("\\subsection{%s}" . "\\subsection*{%s}")
                                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
        (add-to-list 'org-latex-caption-above 'src-block)
        (setq
         org-latex-bib-compiler "biber"
         org-latex-default-class "a4article"
         org-latex-minted-options '(("linenos") ("mathescape"))))
