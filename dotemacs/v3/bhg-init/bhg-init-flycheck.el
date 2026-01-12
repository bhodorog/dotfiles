(use-package flycheck
  :ensure t
  ;; :custom
  ;; (flycheck-disabled-checkers '(python-pylint))

  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-flake8-maximum-line-length 120)

  (flycheck-define-checker
      python-mypy ""
      :command ("mypy"
                "--ignore-missing-imports"
                ;; "--python-version" "3.6"
                source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t)
  (flycheck-add-next-checker 'python-flake8 'python-mypy t)
)

;; flycheck's main feature is the ability to run all the checkers at
;; the same time vs sequentially or chained as flycheck
;; we use no-require since the hook needs to be added
;; (with-eval-after-load 'flycheck ...)
;; (use-package flycheck
;;   :no-require t
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(provide 'bhg-init-flycheck)
;;; bhg-init-flycheck.el ends here
