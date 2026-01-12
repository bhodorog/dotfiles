(provide 'bhg-init-cdesk)


(use-package flycheck
  :config
  (setq flycheck-flake8-maximum-line-length 120)
  (setq flycheck-highlighting-mode 'lines)
  )


(use-package python-black
  :init
  (setq python-black-extra-args '("--line-length=120")))


(use-package py-isort
  :init
  (setq py-isort-options '("--line-width=120")))


;; Let's have elpy around, for elpy-occur if not for anything else)
(use-package elpy
  :ensure t
  :bind ("C-c y d" . elpy-occur-definitions)
  )
