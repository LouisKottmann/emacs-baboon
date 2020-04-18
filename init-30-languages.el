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
  :init
  (setq lsp-keymap-prefix "C-S-l")
  (gsetq lsp-log-io                       "*debug*"
         lsp-print-performance            "*debug*"
         lsp-inhibit-message              t
         lsp-report-if-no-buffer          "*debug*"
         lsp-enable-snippet               t
         lsp-restart                      'interactive
         lsp-document-sync-method         nil
         lsp-eldoc-render-all             t
         lsp-enable-xref                  t
         lsp-enable-indentation           t
         lsp-prefer-flymake               nil
         lsp-enable-on-type-formatting    t
         lsp-signature-auto-activate      t
         lsp-enable-semantic-highlighting t)
  :hook ((ruby-mode . lsp)
         (shell-script-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-session-file (expand-file-name "lsp-session-v1" baboon-savefile-dir)))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :after company
  :preface
  (defun company-lsp-init-h ()
    "Make sure that `company-capf' is disabled since it is incompatible with
`company-lsp' (see lsp-mode#884)."
    (if (not (bound-and-true-p company-mode))
        (add-hook 'company-mode-hook #'company-lsp-init-h t t)
      (setq-local company-backends
                  (cons 'company-lsp
                        (remq 'company-capf company-backends)))
      (remove-hook 'company-mode-hook #'company-lsp-init-h t)))
  :hook ((lsp-mode . company-lsp-init-h))
  :init
  (gsetq company-lsp-async               t
         company-lsp-enable-recompletion t
         company-lsp-enable-snippet      t
         company-lsp-cache-candidates    'auto))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package toml-mode)

(use-package systemd)

(use-package ruby-mode
  :mode ("\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "\\.cap\\'"
         "\\.thor\\'"
         "\\.rabl\\'"
         "Thorfile\\'"
         "Vagrantfile\\'"
         "\\.jbuilder\\'"
         "Podfile\\'"
         "\\.podspec\\'"
         "Puppetfile\\'"
         "Berksfile\\'"
         "Appraisals\\'")
  :interpreter "ruby"
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode)
  :bind (:map ruby-tools-mode-map
              ("C-;" . iedit-mode)))

(use-package rbenv
  :commands global-rbenv-mode
  :config
  (global-rbenv-mode)
  (rbenv-use-corresponding))
