;;; package --- Summary
;;; Commentary:
;;; elpa like repositories
;;; elpa - official Emacs repository (copyright restrictions)
;;; melpa - automated build repository (packages auto built and versioned)
;;; marmelade - authors log in and upload their packges
;;; for details, see
;;; https://web.archive.org/web/20190312134118/http://blog.jorgenschaefer.de/2014/06/the-sorry-state-of-emacs-lisp-package.html
;;; Code:
(when (>= emacs-major-version 24)
  (require 'package)

  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)

  ;; keep the installed packages in .emacs.d
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
  ;; prefer melpa-stable. To override and use melpa with use-package use `:pin melpa`
  (setq package-archive-priorities
        '(("melpa-stable" . 20)
          ("org" . 20)
          ("gnu" . 10)
          ("melpa" . 5)))

  ;; this makes every single start very slow. Use it with care!
  ;; update the package metadata if the local cache is missing
  ;; (unless package-archive-contents
  ;;   (package-refresh-contents))
  (package-initialize)
)

(provide 'bhg-init-packages)
