;;; Denote (simple note-taking and file-naming)

;; Read the manual: <https://protesilaos.com/emacs/denote>.
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
    ("C-c d o" . denote-open-or-create)
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
    ("C-c d S" . denote-sort-dired)
    ("C-c d g" . denote-grep)
    :map text-mode-map
    ("C-c d B" . denote-backlinks)
    ("C-c d b" . denote-find-backlink)
    ;; Also see `denote-rename-file' further above.
    ("C-c d R" . denote-rename-file-using-front-matter)
    ("C-c d k" . denote-rename-file-keywords)
    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-marked-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-A" . denote-dired-rename-marked-files-add-keywords)
    ("C-c C-d C-K" . denote-dired-rename-marked-files-remove-keywords)
    ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :config

  ;; Remember to check the doc strings of those variables.
  (let ((dir (getenv "DENOTE_DIRECTORY")))
    (when (and dir (not (string-empty-p dir)))
      (setq denote-directory (expand-file-name dir))))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-excluded-directories-regexp "data") ; external data related to headings is stored in these directories (web archives)
  (setq denote-date-format nil)         ; read its doc string
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-prompts '(title keywords subdirectory signature))

  (setq denote-rename-confirmations nil) ; CAREFUL with this if you are not familiar with Denote!
  (setq denote-save-buffers t)
  (setq denote-rename-buffer-format "%s %t%b")
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have a literal "[D]"
  ;; followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  (setq denote-buffer-has-backlinks-string " (<--->)")

  ;; I use Yasnippet to expand these into a better template.
  (add-to-list 'denote-templates '(reference-note . "reference"))
  (add-to-list 'denote-templates '(morning . "morningpage"))
  (add-to-list 'denote-templates '(emotion . "emotion"))
  (add-to-list 'denote-templates '(insight . "insight"))
  (add-to-list 'denote-templates '(weekly_intentions . "weekint"))
  (add-to-list 'denote-templates '(weekly_report . "weekrpt"))
  (add-to-list 'denote-templates '(sketch . "sketch"))
  (add-to-list 'denote-templates '(dayplan . "dayplan"))

  (defun denote--link-ol-get-id ()
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
      id)))

(use-package denote-org
  :ensure t
  :bind
  ( :map org-mode-map
    ("C-c d h" . denote-org-link-to-heading)
    ("C-c d d l" . denote-org-dblock-insert-links)
    ("C-c d d b" . denote-org-dblock-insert-backlinks)
    ("C-c d d m" . denote-org-dblock-insert-missing-links))
  :config
  (setq denote-org-store-link-to-heading 'id)
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
#+signature:  %5$s
\n"))

(use-package denote-silo
  :ensure t
  :bind
  ( :map global-map
    ("C-c d N" . denote-silo-select-silo-then-command)
    ("C-c d O" . denote-silo-open-or-create))
  :commands ( denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/personal/"
              "~/Documents/work/")))

(use-package denote-journal
  :ensure t
  :commands
  ( denote-journal-new-entry
    denote-journal-new-or-existing-entry
    denote-journal-link-or-create-entry
    denote-plan-new-or-existing-entry
    denote-personal-new-microblog-entry )
  :bind
  ( :map global-map
    ("C-c d j" . denote-journal-new-or-existing-entry)
    ;; Bindings to personal functions (defined below)
    ("C-c d p m" . denote-personal-new-microblog-entry)
    ("C-c d p b" . denote-personal-new-blog-entry)
    ("C-c d p l" . denote-personal-new-linklog-entry))
  :config
  (let ((dir (getenv "DENOTE_DIRECTORY")))
    (when (and dir (not (string-empty-p dir)))
      (setq denote-journal-directory (expand-file-name "journal" denote-directory))))
  ;; Journal settings
  (setq denote-journal-keyword "journal")

  (defun denote-personal-new-blog-entry (&optional date)
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

  (defun denote-personal-new-microblog-entry (&optional date)
    "Create a new microblog entry.
  Set the title of the new entry according to the value of the user option
  `denote-journal-title-format'.

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
       (denote-journal-daily--title-format internal-date)
       '("draft" "microblog")
       nil nil date
       ;; See YASnippet
       "microblog")))

  (defun denote-personal-new-linklog-entry (date)
    "Create a new microblog entry.
  Set the title of the new entry according to the value of the user option
  `denote-journal-title-format'.

  Prompt for a DATE. If `denote-date-prompt-use-org-read-date' is
  non-nil, use the Org date selection module.

  When called from Lisp DATE is a string and has the same format as
  that covered in the documentation of the `denote' function.  It
  is internally processed by `denote-parse-date'."
    (interactive (list (denote-date-prompt)))
    (let ((internal-date (denote-parse-date date))
          (denote-directory (file-name-as-directory (expand-file-name "published" denote-directory))))
      (denote
       (denote-journal-daily--title-format internal-date)
       '("draft" "linklog")
       nil nil date
       ;; See YASnippet
       "linklog")))

  (defvar denote-plan-keyword "standup")
  (defvar denote-plan-directory (expand-file-name "plan" denote-directory))
  (defun denote-new-plan-note-sequence ()
    ;; [tag: plan-notes-are-under-51-signature]
    (denote-sequence-get-new 'child "51"))
  (defun denote-plan-entry-title (date)
    (format-time-string "Plan for the day, %A, %e %B, %Y" date))

  (defun denote-plan-new-entry (&optional date)
    "Create a new plan entry. See `denote-journal-new-entry'."
    (interactive (list (when current-prefix-arg (denote-date-prompt))))
    (let* ((internal-date (or (denote-valid-date-p date) (current-time)))
           (denote-journal-directory denote-plan-directory)
           (denote-directory (denote-journal-directory))
           (denote-journal-signature #'denote-new-plan-note-sequence))
      (denote
       (denote-plan-entry-title internal-date)
       `(,denote-plan-keyword)
       nil nil date
       ;; See YASnippet
       "dayplan"
       (denote-journal-signature))
      (run-hooks 'denote-journal-hook)))

  (defun denote-plan-new-or-existing-entry (&optional date)
    "Exactly like `denote-journal-new-or-existing-entry', but for plan files"
    (interactive (list (when current-prefix-arg (denote-date-prompt))))
    (let ((denote-journal-keyword denote-plan-keyword)
          (denote-journal-directory denote-plan-directory))
      (if-let* ((date-to-use (or (denote-valid-date-p date) (current-time)))
                (internal-date (denote-journal--date-in-interval-p date-to-use denote-journal-interval))
                (denote-journal-directory denote-plan-directory)
                (files (denote-journal--get-entry internal-date denote-journal-interval))
                (file (denote-journal-select-file-prompt files)))
          (find-file file)
        (denote-plan-new-entry date)))))

(use-package tmr
  :ensure t)

(use-package denote-sequence
  :ensure t
  :bind
  ( :map global-map
    ("C-c d s n" . denote-sequence)
    ("C-c d s p" . denote-sequence-new-parent)
    ("C-c d s c" . denote-sequence-new-child-of-current)
    ("C-c d s s" . denote-sequence-new-sibling-of-current)
    ("C-c d s d" . denote-sequence-dired)
    ("C-c d s f" . denote-sequence-find-dired)
    ("C-c d s r" . denote-sequence-reparent)
    ("C-c d s C" . denote-sequence-convert))
  :commands
  ( denote-sequence-get-new )
  :config
  (setq denote-sequence-scheme 'numeric))

(use-package denote-regexp
  :ensure t)

(use-package denote-explore
  :ensure t
  :after (denote denote-regexp)
  :config
  (setq denote-explore-network-directory (expand-file-name "denote-explore-network" denote-directory))
  (setq denote-explore-network-filename "denote-network")
  (setq denote-explore-network-format 'd3.js)
  (setq denote-explore-network-d3-colours 'SchemeObservable10)
  (setq denote-explore-network-d3-js "https://d3js.org/d3.v7.min.js")
  (setq denote-explore-network-graphviz-filetype 'svg)
  :bind
  (;; Statistics
   ("C-c d e s n" . denote-explore-count-notes)
   ("C-c d e s k" . denote-explore-count-keywords)
   ("C-c d e s e" . denote-explore-barchart-filetypes)
   ("C-c d e s w" . denote-explore-barchart-keywords)
   ("C-c d e s t" . denote-explore-barchart-timeline)
   ;; Random walks
   ("C-c d e w n" . denote-explore-random-note)
   ("C-c d e w r" . denote-explore-random-regex)
   ("C-c d e w l" . denote-explore-random-link)
   ("C-c d e w k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c d e j d" . denote-explore-duplicate-notes)
   ("C-c d e j D" . denote-explore-duplicate-notes-dired)
   ("C-c d e j l" . denote-explore-dead-links)
   ("C-c d e j z" . denote-explore-zero-keywords)
   ("C-c d e j s" . denote-explore-single-keywords)
   ("C-c d e j r" . denote-explore-rename-keywords)
   ("C-c d e j y" . denote-explore-sync-metadata)
   ("C-c d e j i" . denote-explore-isolated-files)
   ;; Visualise denote
   ("C-c d e n" . denote-explore-network)
   ("C-c d e r" . denote-explore-network-regenerate)
   ("C-c d e d" . denote-explore-barchart-degree)
   ("C-c d e b" . denote-explore-barchart-backlinks)))

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
    (expand-file-name "marginalia.org" org-directory))
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

;;; Pomodoro
(use-package org-pomodoro-third-time
  :ensure (:host github :repo "telotortium/org-pomodoro-third-time")
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t
        org-pomodoro-clock-break t
        org-pomodoro-length 45))

(use-package third-time
  :ensure t)

(provide 'unravel-study)
