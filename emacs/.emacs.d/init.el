(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (ledger-mode hledger-mode flycheck-tip erlang projectile popwin which-key magit latex-preview-pane company-lsp lsp-ui lsp-mode eglot helm async company-auctex company rainbow-delimiters color-theme-sanityinc-tomorrow flycheck editorconfig auctex)))
 '(popwin-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; For scrolling with the trackpad. Pressing Shift makes it faster.
;; https://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Global nice things
(global-hl-line-mode 1); Highlight current row
(tool-bar-mode -1); Disable toolbar
(load-theme 'sanityinc-tomorrow-eighties t)
(setq-default buffer-file-coding-system 'utf-8-unix); Unix line endings and UTF-8 encoding
(desktop-save-mode 1); Automatic restoration on closing
;; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'find-file-hook 'rainbow-delimiters-mode)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; AUCTeX
(add-hook 'TeX-mode-hook 'reftex-mode); RefTeX mode by default
(set-default 'preview-scale-function 1.3); Preview size
;; Make AUCTeX aware of style files and multi-file documents right away..
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; Add tikz environment to preview-latex for AUCTeX
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)
  )
;; Tell RefTeX to fetch bibliography considering these commands
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

;; Erlang
(setq load-path (cons "/usr/lib/erlang/lib/tools-3.3/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Required to use hledger instead of ledger itself.
(setq ledger-mode-should-check-version nil
      ledger-report-links-in-register nil
      ledger-binary-path "hledger")

;; Nice theme for Emacs
(use-package modus-themes
    :ensure
    :init
    (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-region '(bg-only no-extend))
    (modus-themes-load-themes)
    :config
    (modus-themes-load-operandi)
)
