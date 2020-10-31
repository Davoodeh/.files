;;; +misc.el -*- lexical-binding: t; -*-

(custom-set-variables
 '(centaur-tabs-set-close-button nil)
 '(centaur-tabs-set-icons t)
 '(centaur-tabs-gray-out-icons 'buffer) ; grays out icons for unselected tabs
 '(centaur-tabs-height 20)

 '(geiser-default-implementation 'racket)

 '(treemacs-position (quote right)))

;; (setq company-idle-delay nil) ; disables Company-Auto-Completion (makes Emacs WAY, WAY smoother!)

(use-package! eshell :config
              (defun eshell-load-shell-aliases ()
                "Read shell aliases and insert them into the list of eshell aliases."
                (interactive)
                (progn
                 (message "Parsing aliases")
                 (shell-command "alias | sed \"s/='\\?/ / ; s/'$//\"" "shell-aliases" "shell-errors")
                 (switch-to-buffer "shell-aliases")
                 (let ((alias-name) (command-string) (alias-list))
                   (while (not (eobp))
                     (while (not (char-equal (char-after) 32))
                       (forward-char 1))
                     (setq alias-name
                           (buffer-substring-no-properties (line-beginning-position) (point)))
                     (forward-char 1)
                     (setq command-string
                           (buffer-substring-no-properties (point) (line-end-position)))
                     (setq alias-list (cons (list alias-name command-string) alias-list))
                     (forward-line 1))
                   (setq eshell-command-aliases-list alias-list))
                 (if (get-buffer "shell-aliases") (kill-buffer "shell-aliases"))
                 (if (get-buffer "shell-errors") (kill-buffer "shell-errors"))
                 (evil-quit)))
              (after! eshell (eshell-load-shell-aliases)))
