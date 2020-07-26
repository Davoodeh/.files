;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; Custom
(custom-set-variables
 '(auto-insert-query nil)               ; don't remember exactly xD
 '(bidi-paragraph-direction nil)        ; To ensure Emacs' default behaviour
 '(centaur-tabs-set-close-button nil)   ; Mouse cannot close tabs now
 '(centaur-tabs-set-icons t)
 '(centaur-tabs-gray-out-icons 'buffer) ; Gray out icons for the unselected tabs
 '(centaur-tabs-height 20)
 '(doom-font (font-spec :family "Iosevka" :size 16 :inherit 'italic))
 '(doom-unicode-font (font-spec :family "Material Design Icons" :size 16))
 ;; '(line-spacing 8)
 ;; '(lsp-log-io t)
 '(lsp-keymap-prefix "M-l")             ; For i3 and tiling window managers that use Super key
 '(display-line-number t)
 '(display-line-numbers-type 'relative) ; Doesn't work properly for LaTeX
 '(display-time-default-load-average 3) ; Just show simple clock not loadtime
 '(display-time-mode t)
 '(doom-theme 'doom-solarized-dark)
 '(global-display-line-numbers-mode)
 '(global-prettify-symbols-mode)
 '(treemacs-position (quote right)))

;;; Custom-setq
(setq company-idle-delay nil)           ; Disable Company-Auto-Completion (saves a lot of time!)

(use-package! tex-fold :defer t :config
              (add-hook 'after-save-hook #'TeX-fold-buffer))

;;; Modes
(general-evil-setup)
(auto-save-visited-mode)
;; (use-package! nyan-mode
;;   :config
;;   (nyan-mode)
;;   ;; (nyan-start-animation)
;;   (nyan-toggle-wavy-trail))

;;; Functions
;; (defun set-compile-command (CMD)
  ;; (set (make-local-variable 'compile-command)
       ;; (format "%s %s" CMD (file-name-nondirectory buffer-file-name))))

(defun set-lang (layout)
  (shell-command (concat "kblayoutmanager set " layout) nil nil))

;; Open an external terminal in the working directory
(defun xterm-here ()
  (interactive "@")
  (shell-command
   (concat "cd '" (file-name-directory (or (or load-file-name buffer-file-name) "")) "' ; detach terminal")
   nil nil))

;;; Binds
;; FIXME : why on the earth it's impossible to use a var as input for general-imap?
;; TODO make all of it only one function, geez
(when (featurep! :editor evil)
  (general-imap "j" (general-key-dispatch '(lambda ()
                                            (interactive)
                                            (insert "j"))
      :timeout 0.25
      "j" '(lambda ()
            (interactive)
            (evil-normal-state)
            (set-lang "us")))) ; FIXME suppress the "finished with no output"
  (general-imap "ت" (general-key-dispatch '(lambda ()
                                            (interactive)
                                            (insert "ت"))
      :timeout 0.25
      "ت" '(lambda ()
            (interactive)
            (evil-normal-state)
            (set-lang "us")))
  (general-nmap "ت" (general-key-dispatch '())
      :timeout 0.25
      "ت" '(lambda ()
            (interactive)
            (evil-normal-state)
            (set-lang "us")))))
  ;; (add-hook 'evil-mode-hook
  ;;       (lambda () (local-unset-key (kbd "M-i"))))
  ;; (general-nmap :prefix "M" ; FIXME doesn't work
  ;;   "i" '(lambda ()
  ;;          (set-lang "fa")
  ;;          (evil-insert))))

(map!
  :leader
    :desc "Open an external terminal here" "o x" #'xterm-here)

(map!
  :when (featurep! :ui tabs)
    :leader
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
      :desc "forward cycle through tabs"  "]" #'centaur-tabs-forward)
        ;; FIXME
        ;; (:map global-map "C-TAB" #'centaur-tabs-forward)

;; TODO Add -c flag to magit-log and bind it to Spc-g-l-a

;;; Hooks and on-load/event/hook functions
(add-hook 'find-file-hooks 'auto-insert)

;; (add-hook 'LaTeX-mode-hook 'auto-save-mode)
;; (add-hook 'org-mode-hook 'auto-save-mode)

;; (add-hook 'LaTeX-mode-hook '(set-compile-command "xelatex"))
;; (with-eval-after-load 'tex (add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open"))) ; or "Zathura"


;;; Boilerplates
;;; TODO move to another folder
(define-skeleton xelatex-skeleton
  "Inserts a Farsi Latex skeleton into current buffer."
  "Title: "
  "\\documentclass[professionalfont, a4paper, margin=1in]{article}\n"
  "\\linespread{1.25}\n"
  "\\usepackage[backend=biber, sorting=none]{biblatex} % for bib management\n"
  "\\addbibresource{~/Documents/bib/bib.bib}\n"
  "\\usepackage{booktabs} % for table rules and lines\n"
  "\\usepackage{graphicx} % for figures, floats and graphics\n"
  "\\usepackage{float} % for 'H' location\n"
  "\\usepackage{array,etoolbox} % for auto-count feature in tables\n"
  "\\preto\\tabular{\\setcounter{magicrownumbers}{0}}\n"
  "\\newcounter{magicrownumbers}\n"
  "\\newcommand\\rownumber{\\stepcounter{magicrownumbers}\\arabic{magicrownumbers}}\n"
  "% just add a @{\\makebox[3em][r/c/l]{\\rownumber\\space}} in any column\n"
  "\\usepackage[table]{xcolor} % for colouring tables\n"
  "\\usepackage{tikz} % for drawing graphs and stuff\n"
  "\\usetikzlibrary{shapes.geometric} % for shapes\n"
  "\\usepackage{fullpage} % for narrow margins, [cm] for more narrow margins\n"
  "\\usepackage{amsmath} % for more symbols\n"
  "\\usepackage[linesnumbered,ruled]{algorithm2e}\n"
  "\\newenvironment{algo}[1][H]{\\renewcommand{\\algorithmcfname}{الگوریتم}\\begin{algorithm}[#1]}{\\end{algorithm}}\n"
  "\\usepackage{enumitem,amssymb} % for todolists and advanced enuming\n"
  "\\newlist{todolist}{itemize}{2}\n"
  "\\setlist[todolist]{label=$\square$}\n"
  "\\usepackage{pifont}\n"
  "\\newcommand{\\cmark}{\\ding{51}}\n"
  "\\newcommand{\\xmark}{\\ding{55}}\n"
  "\\newcommand{\\done}{\\rlap{$\\square$}{\\raisebox{2pt}{\\large\\hspace{1pt}\\cmark}}\n"
  "\\hspace{-2.5pt}}\n"
  "\\newcommand{\\wontfix}{\\rlap{$\\square$}{\\large\\hspace{1pt}\\xmark}}\n"
  "\\usepackage{subcaption} % for side-by-side figures\n"
  "\\usepackage{hyperref} % for links\n"
  "\\usepackage{titling} % for titlingpage env\n"
  "\\usepackage{xepersian} % must be the last package\n"
  "\\settextfont{XB Roya}\n"
  "\\setlatintextfont{Vazir}\n"
  "\\setdigitfont{XB Yas}\n"
  "\\setmonofont{Iosevka}\n"
  "\\author{محمدیاسین داوده}\n"
  "\\title{" _ "}\n"
  "\\date{\\today}\n"
  "\\begin{document}\n"
  "\\begin{titlingpage}\n"
  "\\maketitle\n"
  "\n"
  "% فارسی\n"
  "\\begin{abstract}\n"
  "\n"
  "\\end{abstract}\n"
  "\\end{titlingpage}\n"
  "\\tableofcontents\\pagebreak\n"
  "\n"
  "% فارسی\n"
  "\n"
  "\\pagebreak\\section*{مراجع}\\begin{LTR}\\printbibliography[heading=none]\\end{LTR}\n"
  "\\end{document}\n")

(define-skeleton sent-skeleton
  "Inserts a preSENT skeleton into current buffer."
  "Title: "
  "#+TITLE: \n"
  "#+AUTHOR: M. Yas. Davoodeh\n"
  "#+DATE: <2020-03-02 Mon>\n"
  "#+CREATOR: M. Yas. Davoodeh\n"
  "* BOF\n"
  "title\n"
  "By M. Yas. Davoodeh\n"
  "\n"
  "* I. \n"
  "\n"
  "* II. \n"
  "\n"
  "* III. \n"
  "\n"
  "* IV. \n"
  "\n"
  "* EOF\n"
  "Thanks. 🙏\n"
  "Question? 🤔")

(setq auto-insert-alist '(
                          (latex-mode   . xelatex-skeleton)
                          ("\\.sent\\'" . sent-skeleton)
                          ("\\.snt\\'"  . sent-skeleton)))
(setq auto-mode-alist (append '(
                                ("\\.sent\\'" . org-mode)
                                ("\\.snt\\'"  . org-mode))
                              auto-mode-alist))
