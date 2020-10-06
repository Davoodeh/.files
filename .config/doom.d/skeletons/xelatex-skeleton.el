;;; ~/.config/doom.d/skeletons/xelatex-skeleton.el -*- lexical-binding: t; -*-
(define-skeleton xelatex-skeleton
  "Inserts a Farsi LaTeX skeleton into current buffer."
  "Title: "

  "\\documentclass[a4paper]{article}\n"
  "\\usepackage[pass]{geometry}\n\n"

  "\\usepackage[backend=biber, sorting=none]{biblatex}\n"
  "\\begin{filecontents}{\\jobname.bib.tmp}\n"
  "\\end{filecontents}\n"
  "\\addbibresource{\\jobname.bib.tmp}\n\n"

  "\\usepackage{\n"
  "  fullpage,\n"
  "  hyperref,\n"
  "  titling,\n"
  "  xcolor,\n"
  "  caption, subcaption,\n"
  "  float,\n"
  "  graphicx,\n"
  "  amsmath, amssymb, amsthm,\n"
  "  tikz,\n"
  "}\n"

  "\\usepackage[linesnumbered,ruled]{algorithm2e}\n"
  "\\newenvironment{algo}[1][H]{\\renewcommand{\\algorithmcfname}{الگوریتم}\\begin{algorithm}[#1]}{\\end{algorithm}}\n"

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

  "% \\pagebreak\\section*{مراجع}\\begin{latin}\\printbibliography[heading=none]\\end{latin}\n"
  "\\end{document}\n")
