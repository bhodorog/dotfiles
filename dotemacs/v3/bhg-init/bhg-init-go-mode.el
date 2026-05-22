(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save))
  :config
  ;; Use goimports instead of gofmt (recommended)
  (setq gofmt-command "goimports"))

(provide 'bhg-init-go-mode)
