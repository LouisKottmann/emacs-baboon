;;; init-30-languages.el --- install languages and their completions

(use-package company
  :init (global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-idle-delay 0.0)
  :bind ("C-<tab>" . company-complete))

(use-package which-key
  :config
  (which-key-mode))

;; For adding per-language binaries, see https://github.com/emacs-lsp/lsp-mode/blob/master/README.org#supported-languages
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-S-l")
  :hook ((ruby-mode . lsp)
         (shell-script-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-prefer-capf t)
  (setq lsp-session-file (expand-file-name "lsp-session-v1" baboon-savefile-dir)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dockerfile-mode)
