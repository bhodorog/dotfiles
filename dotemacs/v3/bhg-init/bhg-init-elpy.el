(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  ;; disable flymake since we're using flycheck
  ;; see https://github.com/jorgenschaefer/elpy/issues/137 why flycheck is not supported directly
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-rpc-virtualenv-path 'current)
)

(provide 'bhg-init-elpy)
