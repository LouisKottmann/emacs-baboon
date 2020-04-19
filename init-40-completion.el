;;; init-40-completion.el --- install completions

(use-package company
  :demand t
  :preface (defun baboon-company-set-backends ()
             (interactive)
             (message "settings baboon backends again")
             (gsetq company-backends '((company-lsp company-elisp company-keywords company-yasnippet company-files)
                                       (company-tabnine company-dabbrev))))
  :init
  (gsetq company-minimum-prefix-length 1
         company-idle-delay 0.0
         company-require-match nil
         company-tooltip-align-annotations t)
  :config
  (baboon-company-set-backends)
  (global-company-mode t)
  :bind (("C-<tab>" . company-complete)
         (:map company-active-map
               ("C-o" . company-other-backend)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous))))

(use-package company-tabnine
  :custom
  (company-tabnine-auto-balance nil)
  (company-tabnine-auto-fallback nil)
  (company-tabnine-wait 1))

(use-package company-flx
  :config
  (company-flx-mode +1))

(use-package company-quickhelp
  :after company
  :init
  (gsetq company-quickhelp-delay 0.3
         company-quickhelp-color-background "#fdf6e3")
  :config (company-quickhelp-mode t))

;; For adding per-language binaries, see https://github.com/emacs-lsp/lsp-mode/blob/master/README.org#supported-languages
(use-package lsp-mode
  :init
  (gsetq lsp-keymap-prefix                "C-L"
         lsp-session-file                 (expand-file-name "lsp-session-v1" baboon-savefile-dir)
         lsp-log-io                       "*debug*"
         lsp-print-performance            "*debug*"
         lsp-report-if-no-buffer          "*debug*"
         lsp-enable-snippet               t
         lsp-restart                      'interactive
         lsp-document-sync-method         nil
         lsp-eldoc-render-all             t
         lsp-enable-xref                  t
         lsp-enable-indentation           t
         lsp-diagnostic-package           :flycheck
         lsp-enable-on-type-formatting    t
         lsp-signature-auto-activate      t
         lsp-auto-configure               t
         lsp-semantic-highlighting        :deferred)
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-after-open . baboon-company-set-backends))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :init
  (gsetq company-lsp-async               t
         company-lsp-enable-recompletion t
         company-lsp-enable-snippet      t
         company-lsp-cache-candidates    'auto))

(use-package yasnippet
  :preface
  (defvar baboon-snippets-dir (expand-file-name "snippets" baboon-dir)
    "This folder stores yasnippets")
  :init
  (unless (file-exists-p baboon-snippets-dir)
    (make-directory baboon-snippets-dir))
  :bind (:map yas-minor-mode-map
              ("C-'" . yas-expand))
  :config
  (add-to-list 'yas-snippet-dirs baboon-snippets-dir)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)
