(add-to-list 'load-path (expand-file-name "bhg-init" user-emacs-directory))

(require 'bhg-init-packages)
(require 'bhg-init-everything)
(require 'bhg-init-flycheck)
;; (require 'bhg-init-elpy)
;; (require 'bhg-init-python-mode)
(require 'bhg-init-lsp-python-mode)
(require 'bhg-init-cdesk)
