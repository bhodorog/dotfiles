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

(use-package python-black
  :ensure t
  :demand t
  :after python
  :bind ("C-c y b" . python-black-buffer)
  )

(use-package py-isort
  :ensure t
  :demand t
  :after python
  :bind ("C-c y s" . py-isort-buffer)
  )

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


(provide 'bhg-init-python-mode)
