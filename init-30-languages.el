;;; init-30-languages.el --- install languages

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
       (subword-mode +1))

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
