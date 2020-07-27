;;; ~/.config/doom.d/skeletons/mini-xelatex.el -*- lexical-binding: t; -*-

(define-skeleton mini-xelatex-skeleton
  "Inserts a Persian Latex skeleton into current buffer."
  "Title: "
  "\\documentclass[professionalfont, a4paper, margin=1in]{article}\n"
  "\\usepackage{xepersian}\n"
  "\\settextfont{XB Roya}\n"
  "\\setlatintextfont{Vazir}\n"
  "\\setdigitfont{XB Yas}\n"
  "\\setmonofont{Iosevka}\n"
  "\\author{}\n"
  "\\title{}\n"
  "\\date{\\today}\n"
  "\\begin{document}\n"
  "\n"
  "% فارسی\n"
  _ "\n"
  "\n"
  "\\end{document}")
