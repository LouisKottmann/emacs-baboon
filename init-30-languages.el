;;; init-30-languages.el --- install languages

(gsetq flycheck-temp-prefix "savefile/flycheck")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dockerfile-mode)

(use-package yaml-mode
  :mode
  (("\\.\\(e?ya?\\|ra\\)ml\\.\\(erb\\|sh\\)\\'" . yaml-mode)))

(use-package toml-mode)

(use-package systemd)

(eval-after-load 'ruby-mode
  '(progn
     (defun baboon-ruby-mode-defaults ()
       (ruby-tools-mode +1)
       ;; CamelCase aware editing operations
       (subword-mode +1)
       (lsp))

     (setq baboon-ruby-mode-hook 'baboon-ruby-mode-defaults)

     (add-hook 'ruby-mode-hook (lambda ()
                                 (run-hooks 'baboon-ruby-mode-hook)))))

(use-package ruby-tools
  :bind (:map ruby-tools-mode-map
              ("C-;" . iedit-mode)))

(use-package rbenv
  ;; :commands global-rbenv-mode
  :init
  (gsetq rbenv-modeline-function 'rbenv--modeline-plain)
  :config
  (global-rbenv-mode))

(use-package ssh-config-mode)

(use-package dotenv-mode)

(use-package json-mode)

(use-package gitignore-mode)

(use-package lockfile-mode)

(add-to-list 'auto-mode-alist '(".*inventory.*\\'" . conf-mode))

(use-package ansible-doc)

(use-package jinja2-mode)

(use-package clojure-mode)

(use-package cider
  :config (gsetq cider-annotate-completion-candidates t
                 cider-prompt-for-symbol nil
                 cider-repl-history-file (expand-file-name ".cider-repl-history" baboon-savefile-dir)
                 nrepl-log-messages t)
  (flycheck-clojure-setup) ;; run setup *after* cider load
  :hook ((clojure-mode . cider-mode)
         (clojure-mode . eldoc-mode)
         (cider-repl-mode . subword-mode)))

(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))

(use-package nginx-mode
  :mode (("nginx\\.conf\\'" . nginx-mode)
         ("nginx\\.conf\\.j2\\'" . nginx-mode)))
