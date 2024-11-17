;;; Essential configurations
(use-package emacs
  :ensure nil
  :demand t
  :config
;;;; General settings and common custom functions
  (setq help-window-select t)
  (setq next-error-recenter '(4)) ; center of the window
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq tramp-connection-timeout (* 60 10)) ; seconds
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline t)
  (setq-default truncate-partial-width-windows nil)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq scroll-error-top-bottom t)
  (setq echo-keystrokes-help t) ; Emacs 30
  (setq epa-keys-select-method 'minibuffer)) ; Emacs 30

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

;;;; Built-in bookmarking framework (bookmark.el)
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))

;;;; Registers (register.el)
(use-package register
  :ensure nil
  :defer t ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
(when (treesit-available-p)
  (use-package expreg
    :ensure t
    :functions (prot/expreg-expand prot/expreg-expand-dwim)
    :bind ("C-M-SPC" . prot/expreg-expand-dwim) ; overrides `mark-sexp'
    :config
    (defun prot/expreg-expand (n)
      "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
      (interactive "p")
      (dotimes (_ n)
        (expreg-expand)))

    (defun prot/expreg-expand-dwim ()
      "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
      (interactive)
      (let ((symbol (bounds-of-thing-at-point 'symbol)))
        (cond
         ((equal (bounds-of-thing-at-point 'word) symbol)
          (prot/expreg-expand 1))
         (symbol (prot/expreg-expand 2))
         (t (expreg-expand)))))))

;;;; Show battery status on the mode line (battery.el)
(use-package battery
  :ensure nil
  :hook (after-init . display-battery-mode)
  :config
  (setq battery-mode-line-format
        (cond
         ((eq battery-status-function #'battery-linux-proc-acpi)
          "⏻ %b%p%%,%d°C ")
         (battery-status-function
          "⏻ %b%p%% "))))

;;;; Configuration on Mac OS X machine
(when (eq system-type 'darwin)
  (use-package ns-win
    :ensure nil
    :config
    (defun copy-from-osx ()
      "Make cut and paste work with the OS X clipboard"
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      "Make cut and paste work with the OS X clipboard"
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'alt)
    (setq interprogram-cut-function #'paste-to-osx)
    (setq interprogram-paste-function #'copy-from-osx)
    ;; Work around a bug on OS X where system-name is a fully qualified
    ;; domain name
    (setq system-name (car (split-string system-name "\\.")))
;;; Binaries
    (setq vc-git-program (or (executable-find "git") "/usr/local/bin/git"))
    (setq epg-gpg-program (or (executable-find "gpg") "/usr/local/bin/gpg"))
;;; Source dirs
    ;; Note: These are hard-coded to my machine.
    (setq source-directory (expand-file-name "~/src/emacs/src/"))
    (setq find-function-C-source-directory (expand-file-name "~/src/emacs/src/"))))

(defun vedang/backward-kill-word-or-kill-region (&optional arg)
  "Rebind `C-w' to work differently based on whether a region is active.

If the region is selected, retain the original behaviour, otherwise call
`backward-kill-word' instead.  ARG is passed to `backward-kill-word'."
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(use-package simple
  :ensure nil
  :after vertico ; so that we can bind to vertico-map
  :bind
  ;; Rebind `C-w' to work differently based on whether a region is
  ;; active.
  ( :map global-map
    ("C-w" . vedang/backward-kill-word-or-kill-region)
    :map vertico-map
    ("C-l" . vedang/backward-kill-word-or-kill-region)))

(provide 'unravel-essentials)
