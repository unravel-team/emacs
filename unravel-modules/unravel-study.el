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
  (let ((dir (string-trim (shell-command-to-string "echo $DENOTE_DIRECTORY"))))
    (when (not (string-empty-p dir))
      (setq denote-directory (expand-file-name dir))
      (setq denote-journal-extras-directory (expand-file-name "journal" denote-directory))))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp "data") ; external data related to headings is stored in these directories (web archives)
  (setq denote-date-format nil)         ; read its doc string
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
  (add-to-list 'denote-templates '(sketch . "sketch"))

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

  (defun vedang/denote-link-ol-get-id ()
    "Get the CUSTOM_ID of the current entry.

If the entry already has a CUSTOM_ID, return it as-is, else create a new
one.

If we are creating a new ID, add a CREATED property with the current
timestamp as well.

This function is based on `denote-link-ol-get-id', with minor
modifications."
    (interactive)
    (let* ((pos (point))
           (id (org-entry-get pos "CUSTOM_ID"))
           (created (org-entry-get pos "CREATED")))
      (if (and (stringp id) (string-match-p "\\S-" id))
          id
        (setq id (org-id-new "h"))
        (org-entry-put pos "CUSTOM_ID" id))
      (when (not created)
        (setq created (format-time-string (org-time-stamp-format t t) (current-time)))
        (org-entry-put pos "CREATED" created))
      id))

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

  (setq denote-sort-signature-comparison-function #'vedang/denote-sort-for-signatures))

(use-package consult-denote
  :ensure t
  :bind
  (("C-c d f" . consult-denote-find)
   ("C-c d g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

;;; PDF Tools for reading and annotating PDF files
(use-package pdf-tools
  :ensure (:host github :repo "vedang/pdf-tools" :branch "master")
  :config
  (pdf-tools-install))

;;; org-remark for annotating org and eww files
(use-package org-remark
  :ensure t
  :init
  (setq org-remark-create-default-pen-set nil)
  :bind
  ( :map global-map
    ("C-c r m" . org-remark-mark)
    :map org-remark-mode-map
    ("C-c r e" . org-remark-mark-review)
    ("C-c r i" . org-remark-mark-important)
    ("C-c r o" . org-remark-open)
    ("C-c r n" . org-remark-view-next)
    ("C-c r p" . org-remark-view-prev)
    ("C-c r r" . org-remark-remove)
    ("C-c r d" . org-remark-delete)
    ("C-c r s" . org-remark-save)
    ("C-c r t" . org-remark-toggle)
    ("C-c r v" . org-remark-view))
  :config
  (defun vm/org-remark-notes ()
    (expand-file-name "brain/marginalia.org" org-directory))
  (setq org-remark-notes-file-name #'vm/org-remark-notes)
  ;; Create a pen set for specific kinds of highlights. NOTE: This
  ;; pen-set has been made for dark themes.

  ;; Creates `org-remark-mark-review'
  (org-remark-create "review"
                     ;; face: `dired-flagged'
                     '(:underline (:color "dark red" :style wave) :foreground "#f7143a")
                     '(CATEGORY "review" help-echo "Review this"))

  ;; Creates `org-remark-mark-important'
  (org-remark-create "important"
                     ;; face: `dired-broken-symlink'
                     '(:underline "gold" :background "red1" :foreground "yellow1" :weight bold)
                     '(CATEGORY "important"))

  (set-face-bold 'org-remark-highlighter t)

  (with-eval-after-load 'eww
    (org-remark-eww-mode +1))
  (with-eval-after-load 'info
    (org-remark-info-mode +1))
  (with-eval-after-load 'nov
    (org-remark-nov-mode +1)))

;;; org-fc for flashcards and spaced repetition
(use-package org-fc
  :ensure (:host github :repo "l3kn/org-fc" :branch "main")
  :ensure-system-package (gawk)
  :config
  (setq org-fc-directories `(,(concat org-directory "/notes/"))))

;;; toc-org for automatic Table of Contents
(use-package toc-org
  :ensure t)

;;; Downloading and archiving webpages
(use-package org-board
  :ensure t
  :bind-keymap
  ("C-c o" . org-board-keymap))

;;; Publishing org-mode content
(use-package ox-gfm
  :ensure (:host github :repo "vedang/ox-gfm" :branch "master"))

(provide 'unravel-study)
