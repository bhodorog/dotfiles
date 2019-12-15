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

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(load (expand-file-name "bhg_aliases_reminders.el" user-emacs-directory))

;; store all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq version-control t               ;; Use version numbers for backups
      kept-new-versions 16            ;; Number of newest versions to keep
      kept-old-versions 2             ;; Number of oldest versions to keep
      delete-old-versions t           ;; Ask to delete excess backup versions?
      backup-by-copying-when-linked t ;; Copy linked files, don't rename.
      backup-by-copying t             ;; Don't clobber symlinks
      backup-directory-alist
      `((".*" . "~/.emacs.d/backups")))          ;; Don't litter my fs tree


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; if tool-bar-mode is defined disable it
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable the menu-bar
(menu-bar-mode -1)

;; Use y-n instead of verbose yes-no
(defalias 'yes-or-no-p 'y-or-n-p)


;; disable the blinking cursor for non-term based
(blink-cursor-mode -1)

;; disable the annoying bell ring for non-term based
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; scroll settings
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

(line-number-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)		;; don't use tabs to indent
(setq-default tab-width 4)			;; if tabs are enabled make sure they're consistent
(setq tab-stop-list (number-sequence 4 120 4))  ;; Use 4, 8, 12, 16 ... as tab stops when everything fails


;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; hippie expand is dabbrev expand on steroids
;; while these functions are defined in hippie-exp.el, we're
;; overriding their order to firstly try expand-dabrev
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x a r") #'align-regexp)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; misc old bhg
(transient-mark-mode t)
(cua-selection-mode t)
(defvaralias 'c-basic-offset 'tab-width)        ;; Override the default
(defvaralias 'cperl-indent-level 'tab-width)    ;; Override the default
(visual-line-mode t)


;; enable narrow-to commands by default
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)

(require 'use-package)
(setq use-package-verbose t)

(show-paren-mode t)
;; vs
;; the following's default configs are not great to easily see the
;; matching parens => keep the above
;; (use-package rainbow-delimiters
;;   :ensure t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
  ;; leave special buffers alone
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

;; requires external ag
(use-package ag
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package iedit
  :ensure t)

;; requires external rg
(use-package rg
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "M-\"") 'ivy-avy) ;; the default C-' is not available under iTerm on OSX
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package ivy-hydra
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode +1)
  (add-to-list 'projectile-globally-ignored-file-suffixes "pyc")
  (add-to-list 'projectile-globally-ignored-directories ".mypy_cache")
  )

;; requires exernal  markdown processor: e.g. kramdown
(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-command "kramdown"))


(use-package expand-region
  :ensure t
  ;; this won't work under iTerm on OSX :(
  ;; to find a better suited shortcut
  :bind ("C-=" . er/expand-region))


(use-package dired
  :config
  (require 'dired-x))

;; display current match/total matches in mode-line
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


;; useful for GUI based emacs under OSX which start with a predefined
;; exec-path instead of $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 120) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package yaml-mode
  :ensure t)

;; CONSIDER this instead of AC ?
(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

;; (use-package flycheck-pycheckers
;;   :ensure t
;;   :config
;;   (setq flycheck-pycheckers-checkers (quote (flake8 pylint))))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-disable-checkers '(python-pylint))
)


;; flycheck's main feature is the ability to run all the checkers at
;; the same time vs sequentially or chained as flycheck
;; we use no-require since the hook needs to be added
;; (with-eval-after-load 'flycheck ...)
;; (use-package flycheck
;;   :no-require t
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))


;; (use-package window-number
;;   :ensure t
;;   :config
;;   (window-number-mode 1))


;; other-window replacement for quicker switch when >2
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-window)
  (global-set-key (kbd "s-w") 'ace-window))
;; disabled for now due to sometime determining there are 3 windows
;; when there are clearly just 2
;;  (global-set-key [remap other-window] 'ace-window))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c s") 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "C-c u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-c r") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (setq counsel-find-file-ignore-regexp "\(?:\.pyc\)\|\(?:\.mypy_cache\)")
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-global-mode))

;;; init.el ends here

(provide 'bhg-init-everything)
