;;; Isearch, occur, grep
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
  (add-hook 'occur-mode-hook #'hl-line-mode))

(use-package isearch
  :ensure nil
  :demand t
  :bind
  ( :map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel) ; instead of `isearch-abort'
    ("M-/" . isearch-complete)))

;;; grep and xref
(use-package re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-package xref
  :ensure nil
  :commands (xref-find-definitions xref-go-back)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :config
  (setq grep-save-buffers nil)
  (setq grep-use-headings t) ; Emacs 30

  (let ((executable (or (executable-find "rg") "grep"))
        (rgp (string-match-p "rg" grep-program)))
    (setq grep-program executable)
    (setq grep-template
          (if rgp
              "/usr/bin/rg -nH --null -e <R> <F>"
            "/usr/bin/grep <X> <C> -nH --null -e <R> <F>"))
    (setq xref-search-program (if rgp 'ripgrep 'grep))))

;;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.
(unless (>= emacs-major-version 31)
  (use-package wgrep
    :ensure t
    :after grep
    :bind
    ( :map grep-mode-map
      ("e" . wgrep-change-to-wgrep-mode)
      ("C-x C-q" . wgrep-change-to-wgrep-mode)
      ("C-c C-c" . wgrep-finish-edit))
    :config
    (setq wgrep-auto-save-buffer t)
    (setq wgrep-change-readonly-file t)))

(provide 'unravel-search)
