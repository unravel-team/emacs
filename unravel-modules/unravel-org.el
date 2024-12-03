;;; Calendar
(use-package calendar
  :ensure nil
  :commands (calendar)
  :config
  (setq calendar-mark-diary-entries-flag nil)
  (setq calendar-mark-holidays-flag t)
  (setq calendar-mode-line-format nil)
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-time-zone-style 'numeric) ; Emacs 28.1

  (require 'solar)
  (setq calendar-latitude 35.17         ; Not my actual coordinates
        calendar-longitude 33.36)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0200")
  (setq calendar-daylight-time-zone-name "+0300"))

;;; Appt (appointment reminders which also integrate with Org agenda)
(use-package appt
  :ensure nil
  :commands (appt-activate)
  :config
  (setq appt-display-diary nil
        appt-display-format nil
        appt-display-mode-line t
        appt-display-interval 3
        appt-audible nil ; TODO 2023-01-25: t does nothing because I disable `ring-bell-function'?
        appt-warning-time-regexp "appt \\([0-9]+\\)" ; This is for the diary
        appt-message-warning-time 6)

  (with-eval-after-load 'org-agenda
    (appt-activate 1)

    ;; Create reminders for tasks with a due date when this file is read.
    (org-agenda-to-appt)))

(use-package emacs
  :ensure nil
  :config
  (setq sentence-end-double-space nil))

;;; Org-mode (personal information manager)
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Tresors/Documents/diary"))
  (setq org-default-notes-file (expand-file-name "brain/daily.org" org-directory))
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  ( :map global-map
    ("C-c l" . org-store-link)
    ("C-c o" . org-open-at-point-global)
    :map org-mode-map
    ;; I don't like that Org binds one zillion keys, so if I want one
    ;; for something more important, I disable it from here.
    ("C-'" . nil)
    ("C-," . nil)
    ("M-;" . nil)
    ("C-c M-l" . org-insert-last-stored-link)
    ("C-c C-M-l" . org-toggle-link-display)
    ("M-." . org-edit-special) ; alias for C-c ' (mnenomic is global M-. that goes to source)
    :map org-src-mode-map
    ("M-," . org-edit-src-exit) ; see M-. above
    )
  :config
;;;; general settings
  (setq org-ellipsis "...")
  (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
  (setq org-special-ctrl-a/e 'reversed)
  (setq org-special-ctrl-k nil)
  (setq org-use-speed-commands t)
  (setq org-M-RET-may-split-line '((headline . nil)
                                   (item . t)
                                   (table . nil)
                                   (default . t)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-track-ordered-property-with-tag t))

;;;; archival, org-archive
(use-package org
  :ensure nil
  :config
  ;; Setup directory and file paths for org
  (defvar org-archive-directory (concat org-directory "/archive")
    "Directory under which all archived content is stored.")
  (setq org-archive-location (concat org-archive-directory "/%s_archive::")))

;;; Narrowing and Folding
(use-package org
  :ensure nil
  :bind
  ( :map narrow-map
    ("b" . org-narrow-to-block)
    ("e" . org-narrow-to-element)
    ("s" . org-narrow-to-subtree)))

(use-package org-fold
  :ensure nil
  :config
  (setq org-fold-catch-invisible-edits 'show-and-error))

;;;; refile, todo
(use-package org
  :ensure nil
  :bind
  ( :map org-mode-map
    ("C-c t" . org-todo))
  :config
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-reverse-note-order nil)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WORKING(w!)" "|" "CANCEL(c@)" "DONE(d!)")
          (sequence "PROJECT(p!)" "FOLLOWUP(f!)" "WAITING(a@/!)" "DELEGATED(e@/!)")))

  (defface prot/org-bold-done
    '((t :inherit (bold org-done)))
    "Face for bold DONE-type Org keywords.")

  (setq org-todo-keyword-faces
        '(("CANCEL" . prot/org-bold-done)))
  (setq org-use-fast-todo-selection t)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t))

;;;; tags
(use-package org
  :ensure nil
  :config
  (setq org-tag-alist
        '((:startgrouptag)
          ("TaskStatus" . ?S)
          (:grouptags)
          ("next" . ?x)
          ("waiting" . ?w)
          ("delegated" . ?d)
          (:endgrouptag)
          (:startgrouptag)
          ("Writing" . ?W)
          (:grouptags)
          ("notes" . ?n)
          ("sketch" . ?s)
          ("feedback" . ?f)
          ("actionitems" . ?a)
          (:endgrouptag)
          (:startgrouptag)
          ("TaskType" . ?T)
          (:startgrouptag)
          ("joy" . ?j)
          ("errand" . ?e)
          ("bug" . ?b)
          ("habit" . ?h)
          ("goal" . ?g)
          (:endgrouptag)
          ("important" . ?i)
          ("refile" . ?r)
          ("future" . ?F)))
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0))

;;; org-priorities
(use-package org
  :ensure nil
  :config
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil))

;;;; log
(use-package org
  :ensure nil
  :config
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  ;; Persist clock across Emacs sessions
  (org-clock-persistence-insinuate))

;;; org-clock
(use-package org-clock
  :ensure nil
  :bind
  ("C-<f11>" . org-clock-goto)
  :config
  (setq org-clock-history-length 20)
  (setq org-clock-in-resume t)
  (setq org-clock-into-drawer "CLOCK")
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-persist t)
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (setq org-clock-report-include-clocking-task t)

  ;; List of TODO states to clock-in
  (setq vm/todo-list '("TODO" "WAITING"))

  ;; Change task state to WORKING when clocking in
  (defun bh/clock-in-to-working (kw)
    "Switch task from TODO to WORKING when clocking in.
Skips capture tasks and tasks with subtasks"
    (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
               (member kw vm/todo-list))
      "WORKING"))

  (setq org-clock-in-switch-to-state 'bh/clock-in-to-working))

;;;; links
(use-package org
  :ensure nil
  :config
  (setq org-link-context-for-files t)
  (setq org-link-keep-stored-after-insertion nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;;; org-list
(use-package org-list
  :ensure nil
  :config
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+"))))

;; From @suvrat to keep touch-typing when working with lists.
(defun suv/org-move-item-or-tree ()
  (interactive)
  (message "Use f, b, n, p to move individual items. Use C-{f,b,n,p} for point movement.")
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'org-shiftmetaright)
    (define-key map (kbd "b") 'org-shiftmetaleft)
    (define-key map (kbd "n") 'org-metadown)
    (define-key map (kbd "p") 'org-metaup)
    (define-key map (kbd "C-f") 'forward-char)
    (define-key map (kbd "C-b") 'backward-char)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (set-transient-map map t)))

(use-package org
  :ensure nil
  :config
  (define-key org-mode-map (kbd "C-c j") 'suv/org-move-item-or-tree))

;;;; code blocks
(use-package org
  :ensure nil
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation nil)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2))

;;;; export
(use-package org
  :ensure nil
  :init
  ;; NOTE 2023-05-20: Must be evaluated before Org is loaded,
  ;; otherwise we have to use the Custom UI.  No thanks!
  (setq org-export-backends '(html texinfo md beamer))
  :config
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-dispatch-use-expert-ui nil)
  (setq org-html-htmlize-output-type nil)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (setq org-use-sub-superscripts '{}))

(use-package ox-latex
  :ensure nil
  :config
  (setq org-latex-packages-alist
        '(("capitalize" "cleveref" nil)
          ("" "booktabs" nil)
          ("" "svg" nil)
          ("" "fontspec" nil)))
  (when (executable-find "pygmentize")
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted" nil))
    (setq org-latex-src-block-backend 'minted))
  (setq org-latex-reference-command "\\cref{%s}")
  (setq org-latex-tables-booktabs t)
  (setq org-latex-compiler "lualatex")
  (setq org-latex-hyperref-template
        "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 linktoc=all,
 colorlinks=true,
 linkcolor=blue,
 urlcolor=blue,
 citecolor=blue,
 pdfborder={0 0 1}
 }
")
  (when (executable-find "latexmk")
    (setq org-latex-pdf-process
          '("latexmk -f -pdf -%latex --jobname=%b  -file-line-error --synctex=1 -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
  (setq org-image-actual-width nil))

;;;; org-capture
(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;; agenda
(use-package org-agenda
  :ensure nil
  :bind
  ("C-c a" . org-agenda)
  :config

;;;;; Basic agenda setup
  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-span 'day)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  ;; Show me only the Agenda window when I ask for Agenda
  (setq org-agenda-window-setup 'other-frame)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  ;; Don't recalculate agenda unless I explicitly say so.
  (setq org-agenda-sticky t)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

;;;;; General agenda view options
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)

;;;;; Agenda diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary nil)
  ;; I do not want the diary, but there is no way to disable it
  ;; altogether.  This creates a diary file in the /tmp directory.
  (setq diary-file (make-temp-file "emacs-diary-"))
  (setq org-agenda-diary-file 'diary-file) ; TODO 2023-05-20: review Org diary substitute

;;;;; Agenda follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

;;;;; Agenda multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

;;;;; Agenda filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

;;;;; Agenda items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time nil)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string (concat "Now " (make-string 70 ?.)))
  (setq org-agenda-time-grid
        '((daily today require-timed)
          ( 0500 0600 0700 0800 0900 1000
            1100 1200 1300 1400 1500 1600
            1700 1800 1900 2000 2100 2200)
          "" ""))
  (setq org-agenda-default-appointment-duration nil)

;;;;; Agenda global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

;;;;; Agenda tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -100)

;;;;; Agenda entry
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")
  (setq org-agenda-text-search-extra-files '(agenda-archives))

;;;;; Agenda logging and clocking
  (setq org-agenda-log-mode-items '(clock))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3 :fileskip0 t :compact t :narrow 80))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-agenda-sort-notime-is-late t)   ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

;;;;; Agenda column view
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

  ;; Note: This is a column format that's been useful a few times.
  ;; Noting it here so that I can use it when needed.
  ;; (setq org-columns-default-format
  ;;     "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20CLOSED %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS")
;;;;; Custom views for Agenda
  (setq org-agenda-custom-commands
        '(("g" "GTD Agenda"
           ((agenda ""
                    ((org-agenda-overriding-header
                      "Your Meetings today")
                     (org-agenda-entry-types '(:timestamp :sexp))
                     (org-agenda-repeating-timestamp-show-all t)
                     (org-agenda-time-grid
                      '((daily today require-timed)
                        (800 1000 1200 1400 1600 1800 2000 2200)
                        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
                     (org-agenda-current-time-string
                      "◀── now ─────────────────────────────────────────────────")))
            (tags-todo "+important-notoday"
                       ((org-agenda-overriding-header
                         "These are your IMPORTANT Tasks")
                        (org-agenda-dim-blocked-tasks)
                        ;; Sorting is *really* slowing it down.

                        ;; @TODO: Figure out a way to speed this up,
                        ;; maybe by specifying certain files here and
                        ;; creating a separate custom agenda for all
                        ;; important tasks.
                        ;; (org-agenda-sorting-strategy
                        ;;  '(timestamp-down effort-up))
                        ))
            (agenda ""
                    ((org-agenda-overriding-header
                      "These are your URGENT Tasks")
                     (org-agenda-entry-types '(:deadline))
                     (org-deadline-warning-days 2)
                     (org-agenda-sorting-strategy
                      '(habit-down priority-down timestamp-down))))
            (tags-todo "+joy-notoday"
                       ((org-agenda-overriding-header
                         "These tasks bring JOY")
                        (org-agenda-dim-blocked-tasks)))
            (tags-todo "+notoday"
                       ((org-agenda-overriding-header
                         "I will NOT DO these today")
                        (org-agenda-dim-blocked-tasks)))))
          ("i" "Your IMPORTANT Tasks"
           ((tags-todo "+important-notoday"
                       ((org-agenda-overriding-header
                         "These are your IMPORTANT Tasks")
                        (org-agenda-dim-blocked-tasks)))))
          ("u" "Your URGENT Tasks"
           ((agenda ""
                    ((org-agenda-overriding-header
                      "These are your URGENT Tasks")
                     (org-agenda-entry-types '(:deadline))
                     (org-deadline-warning-days 2)
                     (org-agenda-sorting-strategy
                      '(habit-down priority-down timestamp-down))))))
          ("n" "Your NEXT Tasks" tags-todo "+next")
          ("h" "Your Habits" tags-todo "STYLE=\"habit\"")
          ("r" "Refile" tags "+refile"))))

;;;;; Agenda habits
(use-package org-habit
  :ensure nil
  :config
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 9)
  (setq org-habit-show-all-today t))

;;;; org-capture
(use-package org-capture
  :ensure nil
  :config
;;; Default definitions for variables used in capture templates
  (when (not (boundp 'org-blogpost-file))
    (defvar org-blogpost-file org-default-notes-file
      "File in which blogposts and microblogposts are stored."))
  (when (not (boundp 'org-company-file))
    (defvar org-company-file org-default-notes-file
      "File in which company documentation is stored."))

;;; *CRITICAL NOTE* Read before modifying the push stack below:
  ;; Pushing to capture templates is a stack. What goes in first shows
  ;; up at the bottom of the capture templates list.

;;; Templates for thinking tools
  (push '("T" "Templates for Helping Me Think") org-capture-templates)
  ;; Capture a decision that you've taken, for review and reflection later.
  (push `("Td" "Decision Journal" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.decision.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Current Reality Tree for a problem
  (push `("Tc" "Current Reality Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.crt.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create an Evaporating Cloud for a problem
  (push `("Te" "Evaporating Cloud" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.ec.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Future Reality Tree for a problem
  (push `("Tf" "Future Reality Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.frt.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Prerequisite Tree for a problem
  (push `("Tp" "Prerequisite Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.prt.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Transition Tree for a problem
  (push `("Tt" "Transition Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.trt.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture a new Business idea for sketching out / thinking through
  (push `("Tb" "Business Canvas" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/business.canvas.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture a customer persona, note that this is always captured in
  ;; the current clocking task, and is something I should do under the
  ;; business canvas.
  (push `("TP" "Customer Persona (under Business Canvas)" entry
          (clock)
          (file ,(expand-file-name "capture-templates/business.customer.persona.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture a customer journey through your product, note that this is
  ;; always captured in the current clocking task
  (push `("Tj" "Customer Journey (under Business Canvas)" entry
          (clock)
          (file ,(expand-file-name "capture-templates/business.customer.journey.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

;;; Templates for capturing data about myself on a day-to-day basis
  (push '("d" "Templates for Capturing Data (personal)") org-capture-templates)

  ;; Capture weight / food. This seems hard to get into a laptop habit.
  ;; This is the kind of quantitative life that a mobile solution would
  ;; have helped with.

  (push `("dw" "Weight Tracking" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/bodylog.weight.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t
          :empty-lines 1)
        org-capture-templates)

  (push `("df" "Food Tracking" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/bodylog.food.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t
          :empty-lines 1)
        org-capture-templates)

  (push `("dd" "Downtime Tracking" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/bodylog.dt.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t
          :empty-lines 1)
        org-capture-templates)

;;; Templates for capturing build in public ideas
  (push '("b" "Templates for Capturing Build in Public") org-capture-templates)

  ;; Capture Micro-blogging
  (push `("bm" "New Microblogging entry" entry
          (file+olp+datetree org-blogpost-file "Microblogging")
          (file ,(expand-file-name "capture-templates/microblog.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; New blogpost idea
  (push `("bb" "New Blogpost entry" entry
          (file+headline org-blogpost-file "Meta: Blogposts to write")
          (file ,(expand-file-name "capture-templates/todo.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

;;; Templates for when I want to capture specific feedback about something
  (push '("f" "Templates for Feedback, Reflection, Journaling") org-capture-templates)

  ;; Capture feedback for people I am working with
  (push `("fp" "Feedback for People I'm working with" item
          (file+headline org-company-file "Feedback")
          (file ,(expand-file-name "capture-templates/feedback.others.org"))
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; The monthly newsletter to send to investors, friends and mentors
  (push `("fn" "Company Newsletters" entry
          (file+headline org-company-file "Company Newsletters")
          (file ,(expand-file-name "capture-templates/business.updates.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture suggestions / ideas from other people, which can be
  ;; expanded into actual projects later.
  (push `("fs" "Ideas and Suggestions" entry
          (file+headline org-company-file "Ideas and Suggestions")
          (file ,(expand-file-name "capture-templates/suggestion.org"))
          :prepend t
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

;;; Templates for planning on a day-to-day basis
  (push '("p" "Templates for Planning") org-capture-templates)

  ;; Deliberately plan out and make a routine out of start of day and
  ;; end of day activities.

  (push `("ps" "The Start of Day Planning Routine" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/workday.start.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  (push `("pe" "The End of Day Reflection Routine" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/workday.end.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  (push `("pn" "The Next Day Intentions Routine" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/workday.next.org"))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

;;; Templates for capturing meetings, events, something happening at this time
  (push '("m" "Templates for Capturing Meetings or Events") org-capture-templates)

  ;; Capture an upcoming meeting or one that has already happened
  (push `("mp" "Meeting some other day" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/meeting.org"))
          :prepend t
          :clock-in t
          :clock-resume t
          :time-prompt t)
        org-capture-templates)

  ;; Capture notes for an ongoing meeting or a meeting that's already
  ;; happened.
  (push `("mn" "Meeting today" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/meeting.org"))
          :prepend t
          :clock-in t
          :clock-resume t)
        org-capture-templates)

;;; Templates for Capturing Tasks
  (push '("t" "Templates for Capturing Tasks") org-capture-templates)

  ;; Set up a new habit for tracking. This should be refiled to the
  ;; correct location later.
  (push `("th" "Habit" entry
          (file+headline org-default-notes-file "My Habit Tracker")
          (file ,(expand-file-name "capture-templates/habit.org")))
        org-capture-templates)

  ;; One-click Capture for replying to emails from notmuch. Creates a
  ;; task to remind you that you need to reply to this email.
  (push `("tr" "Respond to email" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/reply.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates)

  ;; One-click capture of links from the clipboard. Used in conjunction
  ;; with `org-protocol', or as a stand-alone to capture links.
  (push `("tw" "Website Link Immediate Capture" entry
          (file+olp org-default-notes-file "Links Captured from the Browser")
          (file ,(expand-file-name "capture-templates/website.org"))
          :immediate-finish t)
        org-capture-templates)

  ;; A more nuanced capture for browser links, which I use for cleaning
  ;; out my browser 2/3 times a week.
  (push `("tl" "Website Link Pinboard Capture" entry
          (file+olp org-default-notes-file "Links Captured from the Browser")
          (file ,(expand-file-name "capture-templates/pinboard.capture.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates)

  ;; Capture a task where someone expects me to communicate when it's done
  (push `("tj" "Jira or External-facing Task" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/jira.org"))
          :clock-in t
          :clock-resume t)
        org-capture-templates)

  ;; One-click Capture for Tasks. Captures the task immediately and gets
  ;; out of your way.
  (push `("ti" "Simple Task Immediate Finish" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/todo.org"))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates))

(provide 'unravel-org)
