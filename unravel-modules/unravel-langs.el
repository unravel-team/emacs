;;;; Tabs, indentation, and the TAB key
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setq-default tab-width 4
                indent-tabs-mode nil))

;;;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay)) ; Emacs 29

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; Eglot (built-in client for the language server protocol)
(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

;;; Markdown (markdown-mode)
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

;;; Flyspell
(use-package flyspell
  :ensure nil
  :bind
  ( :map flyspell-mode-map
    ("C-;" . nil)
    :map flyspell-mouse-map
    ("<mouse-3>" . flyspell-correct-word))
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB"))

;;; Flymake
(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! l" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! L" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

;;; Elisp packaging requirements
(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; General configurations for prose/writing

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-package outline
  :ensure nil
  :bind
  ("<f10>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `dictionary'
(use-package dictionary
  :ensure nil
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

(provide 'unravel-langs)
