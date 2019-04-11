; Shortening of often used commands
(defalias 'tswh 'toggle-split-width-threshold)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'fgd 'find-grep-dired)
(defalias 'sh 'shell)

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'rof 'recentf-open-files)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'hr 'highlight-regexp)
(defalias 'ur 'unhighlight-regexp)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'los 'view-lossage)
(defalias 'dpb 'describe-prefix-bindings)
(defalias 'garm 'global-auto-revert-mode)
(defalias 'slm 'scroll-lock-mode)

;; Various reminders

;; Use it with E on marked buffers in ibuffer to revisit buffers to a new location
;; (set-visited-file-name (replace-regexp-in-string "adapter-api-bluebottle" "bluebottle-adapter-api" (buffer-file-name)))

;; Run shell commands on regions (useful to copy wrapped lines without the finge character)
;; (shell-command-on-region (region-beginning) (region-end) "pbcopy")

;; Automatically surround code with matching chars () [] ""
;; Go to the beginning of the text to be surrounded, Mark Set (C-Spc),
;; go to the end of the text to be surrounded, press one char of the
;; pair


;; set-column-goal C-x C-n
;; used under the hood by M-q (fill-paragraph) and by selecting a region

;; search/replace multiple files
;; 1. C-u M-x find-dired <text to be replaced> (edit `find | grep` command if you need to) (find-grep-dired prior to that can help pre-filling the command template)
;; 2. Q
; or
;; 1. M-x dired+
;; 2. Q
; or
;; 1. projectile-replace{,-regexp}

;; projectile settings
;; (setq projectile-completion-system 'ivy)

;; To highlight the line where the cursor is (globally)
;; (global-hl-line-mode +1)

;; fix go-mode go-vet command
;; (let ((govet (flycheck-checker-get 'go-vet 'command)))
;;   (when (equal (cadr govet) "tool")
;;     (setf (cdr govet) (cddr govet))))
