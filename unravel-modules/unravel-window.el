;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config
;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;; `window', `display-buffer-alist', and related
(use-package prot-window
  :ensure nil
  :demand t
  :config
  ;; NOTE 2023-03-17: Remember that I am using development versions of
  ;; Emacs.  Some of my `display-buffer-alist' contents are for Emacs
  ;; 29+.
  (setq display-buffer-alist
        `(;; no window
          ("\\`\\*Async Shell Command\\*\\'"
           (display-buffer-no-window))
          ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
           (display-buffer-no-window)
           (allow-no-window . t))
          ;; bottom side window
          ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((mode-line-format . none))))
          ;; bottom buffer (NOT side window)
          ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                  (derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . messages-buffer-mode)
                  (derived-mode . backtrace-mode)))
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom))
          ;; below current window
          ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
           (display-buffer-reuse-mode-window display-buffer-below-selected))
          ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.1)
           (dedicated . t)
           (preserve-size . (t . t)))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((or . ((derived-mode . occur-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . Buffer-menu-mode)
                  (derived-mode . log-view-mode)
                  (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
                  "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
                  prot-window-shell-or-term-p
                  ;; ,world-clock-buffer-name
                  ))
           (prot-window-display-buffer-below-or-pop)
           (body-function . prot-window-select-fit-size))
          ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (dedicated . t)
           (window-height . fit-window-to-buffer))
          ;; same window
          ;; NOTE 2023-02-17: `man' does not fully obey the
          ;; `display-buffer-alist'.  It works for new frames and for
          ;; `display-buffer-below-selected', but otherwise is
          ;; unpredictable.  See `Man-notify-method'.
          ((or . ((derived-mode . Man-mode)
                  (derived-mode . woman-mode)
                  "\\*\\(Man\\|woman\\).*"))
           (display-buffer-same-window)))))

(use-package prot-window
  :ensure nil
  :demand t
  :config
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 80)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30))

;;; Frame-isolated buffers
;; Another package of mine.  Read the manual:
;; <https://protesilaos.com/emacs/beframe>.
(use-package beframe
  :ensure t
  :hook (after-init . beframe-mode)
  :bind
  ("C-x f" . other-frame-prefix)
  ("C-c b" . beframe-prefix-map)
  ;; Replace the generic `buffer-menu'.  With a prefix argument, this
  ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
  ;; you absolutely need the global list of buffers.
  ("C-x C-b" . beframe-buffer-menu)
  ("C-x b" . beframe-switch-buffer)
  ("C-x B" . select-frame-by-name)
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun my-beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my-beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

;;; Frame history (undelete-frame-mode)
(use-package frame
  :ensure nil
  :bind ("C-x u" . undelete-frame) ; I use only C-/ for `undo'
  :hook (after-init . undelete-frame-mode))

;;; Window history (winner-mode)
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (("C-x <right>" . winner-redo)
   ("C-x <left>" . winner-undo)))

(provide 'unravel-window)
