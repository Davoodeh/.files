;;; plugins/+minted.el -*- lexical-binding: t; -*-

;; TODO add it to the command dynamically rather than overriding
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
(after! ox-latex
        (setq
         org-latex-listings 'minted
         org-latex-pdf-process (mapcar (lambda (s) (replace-regexp-in-string "%latex " "%latex -shell-escape " s)) org-latex-pdf-process))
                ;; latexmk -pdf -pdflatex='xelatex -shell-escape -interaction nonstopmode %O -output-directory . %S' lisp.tex
        (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))
