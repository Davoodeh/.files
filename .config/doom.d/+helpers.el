;;; +helpers.el -*- lexical-binding: t; -*-

(setq auto-insert-alist '())

(defun load-dir (dir)
  "Load other dirs relative to DOOMDIR."
  (setq dir (concat (file-name-directory (or load-file-name buffer-file-name)) dir))
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun plugin (name)
  "Load a plugin from plugins folder in DOOMDIR."
  (load! (concat "plugins/" name ".el")))

(defun set-lang (layout)
  "Change the keyboard layout."
  ;; TODO set a fallback if kblayout does not exist
  (shell-command (concat "kblayout set " layout) nil nil))

(defun capsoff ()
  "Turn off the caps lock."
  (shell-command "xset q | grep -q '.*Caps\sLock.*on' && xdotool key Caps_Lock" nil nil))

(defun set-bidi-env ()
  (interactive)
  (setq bidi-paragraph-direction 'nil))

(defun xterm-here ()
  "Open an external terminal in the working directory."
  (interactive "@")
  (shell-command
  ;; TODO set a fallback if terminal does not exist or it's win
   (concat "cd '" (file-name-directory (or (or load-file-name buffer-file-name) "")) "' ; exec nohup terminal >/dev/null 2>&1")
   nil nil))
