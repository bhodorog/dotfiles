;;  automatically switch to lisp-mode for given files
(setq auto-mode-alist (cons '("dotemacs" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("emacscustom" . lisp-mode) auto-mode-alist))


(when (>= emacs-major-version 24)
  (require 'package)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)

  ;; keep the installed packages in .emacs.d
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

  ;; update the package metadata is the local cache is missing
  (unless package-archive-contents
    (package-refresh-contents))
  (package-initialize)
)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst bhg-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p bhg-savefile-dir)
  (make-directory bhg-savefile-dir))

;; if tool-bar-mode is defined disable it
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
;; setting the variable directly doesn't work, needs to be customized,
;; and seems to be disable by default anyway
;; (blink-cursor-mode -1)

;; disable the annoying bell ring
;; something else which seems to be disabled by default
;; (setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)
