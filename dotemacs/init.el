;;  automatically switch to lisp-mode for given files
(setq auto-mode-alist (cons '("dotemacs" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("emacscustom" . lisp-mode) auto-mode-alist))


;; elpa like repositories
;; elpa - official emacs repository (copyright restrictions)
;; melpa - automated build repository (packages auto built and versioned)
;; marmelade - authors log in and upload their packges
;; for details, see
;; http://blog.jorgenschaefer.de/2014/06/the-sorry-state-of-emacs-lisp-package.html
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
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-global-mode +1)
  (add-to-list 'projectile-globally-ignored-file-suffixes "pyc"))

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
  (setq whitespace-line-column 79) ;; limit line length
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

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines))


;; (use-package window-number
;;   :ensure t
;;   :config
;;   (window-number-mode 1))


;; other-window replacement for quicker switch when >2
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-window)
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

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
  (setq counsel-find-file-ignore-regexp ".*\.pyc$")
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-global-mode))

;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
;;       '("python2" "/path/to/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))
;; requires external jedi and epc (pip install)
(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  ;; (add-hook 'python-mode-hook 'jedi:ac-setup)
  )

;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (setq ac-auto-start 2
;;         ac-override-local-map nil
;;         ac-use-menu-map t
;;         ac-candidate-limit 20)
;;   (add-hook 'python-mode-hook 'auto-complete-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(defun my/python-mode-hook()
  (add-to-list 'company-backends 'company-jedi))

(use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package python
  :ensure t)

(add-hook
 'python-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c d") 'jedi:show-doc)
   (local-set-key (kbd "M-SPC") 'jedi:complete)
   (local-set-key (kbd "M-.") 'jedi:goto-definition)
   (setq show-trailing-whitespace t)
   (electric-pair-mode t)
   ;; (yas/minor-mode-on)
   (which-function-mode 1)
   (outline-minor-mode t)
   (setq coding-system-for-write 'utf-8)
   (setq fill-column 79)
   ;; (fci-mode)
   ))


;;; init.el ends here
