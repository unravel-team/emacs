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

;;; Denote (simple note-taking and file-naming)

;; Read the manual: <https://protesilaos.com/emacs/denote>.  This does
;; not include all the useful features of Denote.  I have a separate
;; private setup for those, as I need to test everything is in order.
(use-package denote
  :ensure t
  :hook
  ;; If you use Markdown or plain text files you want to fontify links
  ;; upon visiting the file (Org renders links as buttons right away).
  ((text-mode . denote-fontify-links-mode-maybe)

   ;; Highlight Denote file names in Dired buffers.  Below is the
   ;; generic approach, which is great if you rename files Denote-style
   ;; in lots of places as I do.
   ;;
   ;; If you only want the `denote-dired-mode' in select directories,
   ;; then modify the variable `denote-dired-directories' and use the
   ;; following instead:
   ;;
   ;;  (dired-mode . denote-dired-mode-in-directories)
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  Here I only have a subset of what Denote offers.
  ( :map global-map
    ("C-c d n" . denote-create-note)
    ("C-c d N" . denote-silo-extras-select-silo-then-command)
    ("C-c d o" . denote-open-or-create)
    ("C-c d O" . denote-silo-extras-open-or-create)
    ("C-c d l" . denote-link-or-create)
    ("C-c d L" . denote-link-after-creating-with-command)
    ;; Note that `denote-rename-file' can work from any context, not
    ;; just Dired buffers.  That is why we bind it here to the
    ;; `global-map'.
    ;;
    ;; Also see `denote-rename-file-using-front-matter' further below.
    ("C-c d r" . denote-rename-file)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for
    ;; `org-mode-map', `markdown-mode-map', and/or `text-mode-map'.
    ("C-c d j" . denote-journal-extras-new-entry)
    ("C-c d s" . denote-sort-dired)
    ;; Bindings to personal functions (defined below)
    ("C-c d p m" . vedang/denote-publishing-extras-new-microblog-entry)
    ("C-c d p b" . vedang/denote-publishing-extras-new-blog-entry)
    :map text-mode-map
    ("C-c d B" . denote-backlinks)
    ("C-c d b" . denote-find-backlink)
    ;; Also see `denote-rename-file' further above.
    ("C-c d R" . denote-rename-file-using-front-matter)
    ("C-c d k" . denote-rename-file-keywords)
    :map org-mode-map
    ("C-c d h" . denote-org-extras-link-to-heading)
    ("C-c d d l" . denote-org-extras-dblock-insert-links)
    ("C-c d d b" . denote-org-extras-dblock-insert-backlinks)
    ("C-c d d m" . denote-org-extras-dblock-insert-missing-links)
    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-marked-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-A" . denote-dired-rename-marked-files-add-keywords)
    ("C-c C-d C-K" . denote-dired-rename-marked-files-remove-keywords)
    ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (require 'denote-silo-extras)
  (require 'denote-journal-extras)
  (require 'denote-org-extras)

  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Tresors/Documents/diary/notes"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp "data") ; external data related to headings is stored in these directories (web archives)
  (setq denote-date-format nil) ; read its doc string
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-prompts '(title keywords subdirectory signature))

  (setq denote-rename-confirmations nil) ; CAREFUL with this if you are not familiar with Denote!
  (setq denote-save-buffers t)
  (setq denote-rename-buffer-format "[D] %s %t%b")
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-buffer-has-backlinks-string " (<--->)")
  (setq denote-backlinks-show-context t)
  (setq denote-org-store-link-to-heading t)

  ;; Journal settings
  (setq denote-journal-extras-keyword "")

  ;; I use Yasnippet to expand these into a better template.
  (add-to-list 'denote-templates '(reference-note . "reference"))
  (add-to-list 'denote-templates '(morning . "morningpage"))
  (add-to-list 'denote-templates '(emotion . "emotion"))
  (add-to-list 'denote-templates '(insight . "insight"))
  (add-to-list 'denote-templates '(weekly_intentions . "weekint"))
  (add-to-list 'denote-templates '(weekly_report . "weekrpt"))
  (add-to-list 'denote-templates '(checkin . "checkin"))

  ;; Front-matter for Org files
  (setq denote-org-front-matter
        ":PROPERTIES:
:ID: %4$s
:CREATED: %2$s
:END:
#+title:      %1$s
#+filetags:   %3$s
#+date:       %2$s
#+identifier: %4$s
\n")

  (defun vedang/denote-publishing-extras-new-microblog-entry (&optional date)
    "Create a new microblog entry.
Set the title of the new entry according to the value of the user option
`denote-journal-extras-title-format'.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function.  It
is internally processed by `denote-parse-date'."
    (interactive (list (when current-prefix-arg (denote-date-prompt))))
    (let ((internal-date (denote-parse-date date))
          (denote-directory (file-name-as-directory (expand-file-name "published" denote-directory))))
      (denote
       (denote-journal-extras-daily--title-format internal-date)
       '("draft" "microblog")
       nil nil date
       ;; See YASnippet
       "microblog")))

(defun vedang/denote-publishing-extras-new-blog-entry (&optional date)
  "Create a new blog entry.

With optional DATE as a prefix argument, prompt for a date.  If
`denote-date-prompt-use-org-read-date' is non-nil, use the Org
date selection module.

When called from Lisp DATE is a string and has the same format as
that covered in the documentation of the `denote' function.  It
is internally processed by `denote-parse-date'."
  (interactive (list (when current-prefix-arg (denote-date-prompt))))
  (let ((internal-date (denote-parse-date date))
        (denote-directory (file-name-as-directory (expand-file-name "published" denote-directory))))
    (denote
     (denote-title-prompt)
     '("draft")
     nil nil date
     ;; See YASnippet
     "fullblog")))

(defun vedang/denote-link-ol-get-id ()
  "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one."
  (interactive)
  (let* ((pos (point))
         (id (org-entry-get pos "CUSTOM_ID")))
    (if (and (stringp id) (string-match-p "\\S-" id))
        id
      (setq id (org-id-new "h"))
      (org-entry-put pos "CUSTOM_ID" id)
      id)))

(defun vedang/denote--split-luhman-sig (signature)
  "Split numbers and letters in Luhmann-style SIGNATURE string."
  (replace-regexp-in-string
   "\\([a-zA-Z]+?\\)\\([0-9]\\)" "\\1=\\2"
   (replace-regexp-in-string
    "\\([0-9]+?\\)\\([a-zA-Z]\\)" "\\1=\\2"
    signature)))

(defun vedang/denote--pad-sig (signature)
  "Create a new signature with padded spaces for all components"
  (combine-and-quote-strings
   (mapcar
    (lambda (x)
      (string-pad x 5 32 t))
    (split-string (vedang/denote--split-luhman-sig signature) "=" t))
   "="))

(defun vedang/denote-sort-for-signatures (sig1 sig2)
  "Return non-nil if SIG1 is smaller that SIG2.
Perform the comparison with `string<'."
  (string< (vedang/denote--pad-sig sig1) (vedang/denote--pad-sig sig2)))

(setq denote-sort-signature-comparison-function
      #'vedang/denote-sort-for-signatures))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c d f" . consult-denote-find)
   ("C-c d g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package paredit
  :ensure t
  :bind ( :map paredit-mode-map
          ("C-o" . paredit-open-round)
          ("M-D" . paredit-splice-sexp)
          ("C-A-d" . paredit-forward-down)
          ("C-A-u" . paredit-backward-up)
          ;; Unbind things that I don't need
          ("M-s" . nil) ; used for search related keybindings
          ("M-?" . nil)) ; `xref-find-references` uses it.
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
  :hook ((python-base-mode . eglot-ensure))
  :config
  (setq python-shell-dedicated 'project))

(use-package pyvenv
  :ensure t
  :after python
  :commands (pyvenv-create pyvenv-workon pyvenv-activate pyvenv-deactivate)
  :config
  (setenv "WORKON_HOME" "~/.cache/venvs/")
  (pyvenv-tracking-mode 1))

(provide 'unravel-langs)
