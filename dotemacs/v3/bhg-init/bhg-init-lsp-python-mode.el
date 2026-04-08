(use-package python
  :hook (python-mode . (lambda ()
           (electric-pair-mode t)
           (which-function-mode 1)
           (outline-minor-mode 1)
           (setq coding-system-for-write 'utf-8)
           (setq fill-column 100))))

(use-package python-black
  :ensure t
  :demand t
  :after python
  :bind (("C-c y b" . python-black-buffer)
         ("C-c y m" . python-black-region))  ;; requires `pip install black-machiatto` to work
  )

(use-package py-isort
  :ensure t
  :demand t
  :after python
  :bind ("C-c y s" . py-isort-buffer)
  )

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
    ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
    ;; (python-mode . lsp-deferred)  # already done in lsp-pyright
    ;; if you want which-key integration
    (lsp-mode . lsp-enable-which-key-integration)
  )
  :commands (lsp lsp-deferred)
  :config
  ;; ---- performance: ignore venvs in file watching ----
  (dolist (dir '("[/\\\\]\\.venv"
                 "[/\\\\]\\.venv.*"
                 "[/\\\\]venv"
                 "[/\\\\]__pycache__"
                 "[/\\\\]\\.mypy_cache"))
    (add-to-list 'lsp-file-watch-ignored-directories dir))
)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp

;; ;;optionally
(use-package lsp-ui
  :ensure t
  :commands
  lsp-ui-mode
)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; find out what this is for
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)


(provide 'bhg-init-lsp-python-mode)
;;; bhg-init-lsp-python-mode.el ends here
