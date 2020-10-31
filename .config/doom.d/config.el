;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; This file loads configs and is the home to general Emacs configs.
;;; Other configs have their own files or are in +misc.el.
;;; Plugins are small configs that may not be needed from time to time.

(load! "+helpers")

(load-dir "skeletons")
(plugin "+visuals")
(when (featurep! :lang latex)
  (plugin "+xetex")
  (plugin "+minted"))
(when (featurep! :tools format)
  (plugin "+format"))
(when (featurep! :lang org)
  (load! "+org"))
(load! "+misc")

(custom-set-variables
 '(auto-insert-query nil)
 '(bidi-paragraph-direction nil) ; ensures Emacs' default behaviour
 '(doom-font (font-spec :family "Iosevka" :size 16 :inherit '(italic bold)))
 ;; '(line-spacing 8)
 '(display-line-number t)
 '(display-line-numbers-type 'relative)
 '(display-time-default-load-average 3) ; shows a simple clock (load time not included)
 '(display-time-mode t) ; adds a clock to the bar
 '(doom-theme 'doom-solarized-dark)
 '(global-display-line-numbers-mode) ; enables line number mode
 '(global-prettify-symbols-mode))

;;; -----
;;; Modes and hooks
(add-hook 'find-file-hook 'auto-insert)
(auto-save-visited-mode)
;;; -----

;;; -----
;;; Binds
;; Bind jj (and variations) to escape
(when (featurep! :editor evil)
  (plugin "+jj"))

(setq lsp-keymap-prefix "M-l") ; for Window Managers that use the Super key

(map! :leader
      :desc "Open an external terminal here" "o x" #'xterm-here)
(map! :leader :when (featurep! :ui tabs)
      :desc "Move tab to left"               "{" #'centaur-tabs-move-current-tab-to-left
      :desc "Move tab to right"              "}" #'centaur-tabs-move-current-tab-to-right
      :desc "Select the first tab"           "1" #'centaur-tabs-select-beg-tab
      :desc "Select 2nd tab"                 "2" #'centaur-tabs-select-visible-tab
      :desc "Select 3rd tab"                 "3" #'centaur-tabs-select-visible-tab
      :desc "Select 4th tab"                 "4" #'centaur-tabs-select-visible-tab
      :desc "Select 5th tab"                 "5" #'centaur-tabs-select-visible-tab
      :desc "Select 6th tab"                 "6" #'centaur-tabs-select-visible-tab
      :desc "Select 7th tab"                 "7" #'centaur-tabs-select-visible-tab
      :desc "Select 8th tab"                 "8" #'centaur-tabs-select-visible-tab
      :desc "Select the last tab"            "9" #'centaur-tabs-select-end-tab
      :desc "Backward cycle through tabs"    "[" #'centaur-tabs-backward
      :desc "Forward cycle through tabs"     "]" #'centaur-tabs-forward)
;; (:map global-map "C-TAB" #'centaur-tabs-forward) ; FIXME
;;; -----
