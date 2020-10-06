;;; ~/.config/doom.d/skeletons/mini-xelatex.el -*- lexical-binding: t; -*-
(define-skeleton mini-xelatex-skeleton
  "Inserts a Persian LaTeX skeleton into current buffer."
  "Title: "

  "\\documentclass[a4paper]{article}\n"
  "\\usepackage[pass]{geometry}\n\n"

  "\\usepackage{\n"
  "  fullpage,\n"
  "  hyperref,\n"
  "  titling,\n"
  "  amsmath, amssymb, amsthm,\n"
  "}\n"

  "\\usepackage{xepersian}\n"
  "\\settextfont{XB Roya}\n"
  "\\setlatintextfont{XB Roya}\n"
  "% \\setdigitfont{XB Yas}\n"
  "\\setmonofont{Iosevka}\n\n"

  "\\author{}\n"
  "\\title{}\n"
  "\\date{\\today}\n\n"

  "\\begin{document}\n"
  "\\begin{titlingpage}\n"
  "\\maketitle\n\n"

  "% فارسی\n"
  "% \\begin{abstract}\n"
  "\\end{abstract}\n\n"

  "\\tableofcontents\n"
  "\\end{titlingpage}\n\n"

  "% فارسی\n"
  _ "\n\n"

  "\\end{document}\n")
