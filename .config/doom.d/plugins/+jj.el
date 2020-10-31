;;; plugins/+jj.el -*- lexical-binding: t; -*-

;; TODO make all of it only one function
;; XXX Why it's impossible to use a var as input for general-imap?
(after! general
  (general-evil-setup)
        (general-imap "j" (general-key-dispatch 'self-insert-command :timeout 0.25 "j" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
        (general-imap "k" (general-key-dispatch 'self-insert-command :timeout 0.25 "k" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
        (general-imap "ت" (general-key-dispatch 'self-insert-command :timeout 0.25 "ت" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
        (general-imap "о" (general-key-dispatch 'self-insert-command :timeout 0.25 "о" '(lambda () (interactive) (evil-normal-state) (set-lang "us"))))
        (general-imap "J" (general-key-dispatch 'self-insert-command :timeout 0.25 "J" '(lambda () (interactive) (evil-normal-state) (capsoff))))
        (general-imap "K" (general-key-dispatch 'self-insert-command :timeout 0.25 "K" '(lambda () (interactive) (evil-normal-state) (capsoff))))
        (general-nmap "о" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
        (general-nmap "л" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
        (general-nmap "ت" (lambda () (interactive) (evil-normal-state) (set-lang "us")))
        (general-nmap "ن" (lambda () (interactive) (evil-normal-state) (set-lang "us"))))
