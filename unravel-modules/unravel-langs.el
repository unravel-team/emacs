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
  :demand t ;; Not a mistake, we need to load Eglot elisp code before
            ;; we open any Python file.
  :functions (eglot-ensure)
  :commands (eglot)
  :bind
  ( :map eglot-mode-map
    ("C-c e r" . eglot-rename)
    ("C-c e o" . eglot-code-action-organize-imports)
    ("C-c e d" . eldoc)
    ("C-c e c" . eglot-code-actions)
    ("C-c e f" . eglot-format)
    ;; Since eglot plugs into flymake anyway
    ("C-c e l" . flymake-show-buffer-diagnostics))
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t))

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
  ( :map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! l" . flymake-show-buffer-diagnostics)  ; Emacs28
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

(use-package paredit
  :ensure t
  :bind ( :map paredit-mode-map
          ("C-o" . paredit-open-round)
          ("M-D" . paredit-splice-sexp)
          ("C-A-d" . paredit-forward-down)
          ("C-A-u" . paredit-backward-up)
          ;; Unbind things that I don't need
          ("M-s" . nil) ; used for search related keybindings
          ("M-?" . nil) ; `xref-find-references' uses it.
          ("RET" . nil)); `ielm-return' uses it.
  :hook ((lisp-data-mode lisp-mode clojure-mode clojure-ts-mode cider-repl-mode inferior-emacs-lisp-mode) . paredit-mode))

(use-package apheleia
  :ensure t
  :demand t
  :config
  (apheleia-global-mode +1)
  (with-eval-after-load 'apheleia-formatters
    (push '(zprint . ("zprint")) apheleia-formatters)))

;;;; Configuration for Python Programming

(use-package python
  :ensure nil
;;; Uncomment this if you want Eglot to start automatically. I prefer
;;; calling `M-x eglot' myself.
;;  :hook ((python-base-mode . eglot-ensure))
  :config
  (setq python-shell-dedicated 'project)
  ;; Apheleia is an Emacs package for formatting code as you save
  ;; it. Here we are asking Apheleia to use Ruff for formatting our
  ;; Python code.
  (with-eval-after-load 'apheleia
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(ruff-isort ruff))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          '(ruff-isort ruff))))

(use-package pet
  :ensure (:host github :repo "vedang/emacs-pet" :branch "fix-eglot-integration"
                 ;; :remotes (("upstream" :repo "wyuenho/emacs-pet" :branch "main"))
                 )
  :ensure-system-package (dasel sqlite3)
  :config
  ;; The -10 here is a way to define the priority of the function in
  ;; the list of hook functions. We want `pet-mode' to run before any
  ;; other configured hook function.
  (add-hook 'python-base-mode-hook #'pet-mode -10))

;;;; Configuration for Zig Programming

(use-package zig-mode
  :ensure t
;;; Uncomment this if you want Eglot to start automatically. I don't
;;; recommend it, but that's just me.
  ;; :hook ((zig-mode . eglot-ensure))
  )

;;; Configuration for Clojure programming
(use-package clojure-mode
  :ensure t)

;;; `clojure-ts-mode' is not stable enough right now. In particular,
;;; it clashes with `paredit-mode' sometimes, leading to Paredit
;;; throwing unbalanced-expression errorsand being unusable. So
;;; keeping this disabled and experimenting with how to fix it, for
;;; them moment.

(defvar enable-clojure-ts-mode nil)

(when (and (treesit-available-p) enable-clojure-ts-mode)
  (use-package clojure-ts-mode
    :ensure t))

(use-package cider
  :ensure t
  :after (:any clojure-mode clojure-ts-mode)
  :config
  (defun cider-repl-prompt-on-newline (ns)
    "Return a prompt string with newline.
NS is the namespace information passed into the function by cider."
    (concat ns ">\n"))
  (setq cider-repl-prompt-function #'cider-repl-prompt-on-newline))

(use-package clj-refactor
  :ensure t
  :after (:any clojure-mode clojure-ts-mode)
  :hook
  ((clojure-mode . clj-refactor-mode)
   (clojure-ts-mode . clj-refactor-mode))
  :config
  (cljr-add-keybindings-with-prefix "C-c m")
  ;; Don't magically add stuff to the namespace requires form (because
  ;; for big projects this operation is slow) it's easier to do this
  ;; by hand (=add-missing= operation) after you've typed out what you
  ;; wanted to.
  (setq cljr-magic-requires nil))

(use-package clojure-snippets
  :ensure t
  :after clojure-mode)

(use-package jet
  :ensure t
  :config
  (defun json->edn ()
    "Convert the selected region, or entire file, from JSON to EDN."
    (interactive)
    (let ((b (if mark-active (region-beginning) (point-min)))
          (e (if mark-active (region-end) (point-max)))
          (jet (when (executable-find "jet")
                 "jet --pretty --keywordize keyword --from json --to edn")))
      (if jet
          (let ((p (point)))
            (shell-command-on-region b e jet (current-buffer) t)
            (goto-char p))
        (user-error "Could not find jet installed")))))

;;; Settings for Interaction mode for Emacs-Lisp
(use-package ielm
  :ensure nil
  :bind
  ( :map ielm-map
    ("C-j" . newline-and-indent)))

(provide 'unravel-langs)
