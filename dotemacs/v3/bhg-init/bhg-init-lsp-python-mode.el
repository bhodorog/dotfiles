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
    (python-mode . lsp-deferred)
    ;; if you want which-key integration
    (lsp-mode . lsp-enable-which-key-integration)
  )
  :commands lsp-deferred)

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


;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config (which-key-mode) )


(provide 'bhg-init-lsp-python-mode)
