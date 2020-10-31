;;; skeletons/sent-skeleton.el -*- lexical-binding: t; -*-

(define-skeleton sent-skeleton
  "Inserts a preSENT skeleton into current buffer."
  "Title: "

  "#+TITLE: \n"
  "#+AUTHOR: \n"
  "#+DATE: \n"
  "#+CREATOR: \n"
  "* << EndOfSlide\n"
  "title\n"
  "By \n"
  "\n"
  "* I. \n"
  "\n"
  "* II. \n"
  "\n"
  "* III. \n"
  "\n"
  "* IV. \n"
  "\n"
  "* EndOfSlide\n"
  "Thanks. 🙏\n"
  "Question? 🤔")

(add-to-list 'auto-insert-alist '(; insers skeletons automatically
                                  ("\\.sent\\'" . sent-skeleton)
                                  ("\\.snt\\'"  . sent-skeleton)))
