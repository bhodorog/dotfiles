;; elpa like repositories
;; elpa - official emacs repository (copyright restrictions)
;; melpa - automated build repository (packages auto built and versioned)
;; marmelade - authors log in and upload their packges
;; for details, see
;; https://web.archive.org/web/20190312134118/http://blog.jorgenschaefer.de/2014/06/the-sorry-state-of-emacs-lisp-package.html
(when (>= emacs-major-version 24)
  (require 'package)

  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)

  ;; keep the installed packages in .emacs.d
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

  ;; this makes every single start very slow. Use it with care!
  ;; update the package metadata if the local cache is missing
  ;; (unless package-archive-contents
  ;;   (package-refresh-contents))
  (package-initialize)
)

(provide 'bhg-init-packages)
