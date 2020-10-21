;;; skeletons/header-xelatex.el -*- lexical-binding: t; -*-

(define-skeleton header-xelatex-skeleton
  "Inserts a LaTeX template skeleton into current buffer."
  "Title: "

  "\\input{header" _ "}"
  "\\author{}\n"
  "\\title{}\n"
  "\\date{\\today}\n\n"

  "\\begin{document}\n"
  "\\begin{titlingpage}\n"
  "\\maketitle\n\n"

  "% فارسی\n"
  "% \\begin{abstract}\n"
  "% \\end{abstract}\n\n"

  "\\tableofcontents\n"
  "\\end{titlingpage}\n\n"

  "% فارسی\n"
  "\n\n"

  "\\end{document}\n")
