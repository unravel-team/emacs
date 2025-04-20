;;; Personal Basic settings
(use-package emacs
  :ensure nil
  :config
  (setq user-mail-address "vedang@unravel.tech"))

(use-package org
  :ensure nil
  :config
  (setq org-default-notes-file
        (expand-file-name "brain/daily.org" org-directory))
  (setq org-blogpost-file
        (expand-file-name "brain/projects/blogposts.org" org-directory))
  (setq org-crm-file
        (expand-file-name "brain/projects/crm.org" org-directory))
  (setq org-sales-file
        (expand-file-name "brain/projects/sales.org" org-directory))
  (setq org-company-file
        (expand-file-name "brain/daily.org" org-directory))
  (setq org-agenda-files
        (append (list org-default-notes-file
                      (expand-file-name "brain/projects" org-directory)
                      (expand-file-name "brain/areas/prm.org" org-directory)))))

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
          (file ,(expand-file-name "capture-templates/thinking.decision.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Current Reality Tree for a problem
  (push `("Tc" "Current Reality Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.crt.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create an Evaporating Cloud for a problem
  (push `("Te" "Evaporating Cloud" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.ec.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Future Reality Tree for a problem
  (push `("Tf" "Future Reality Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.frt.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Prerequisite Tree for a problem
  (push `("Tp" "Prerequisite Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.prt.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Create a Transition Tree for a problem
  (push `("Tt" "Transition Tree" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/thinking.trt.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture a new Business idea for sketching out / thinking through
  (push `("Tb" "Business Canvas" entry
          (file+headline org-default-notes-file "Helping Me Think")
          (file ,(expand-file-name "capture-templates/business.canvas.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/business.customer.persona.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture a customer journey through your product, note that this is
  ;; always captured in the current clocking task
  (push `("Tj" "Customer Journey (under Business Canvas)" entry
          (clock)
          (file ,(expand-file-name "capture-templates/business.customer.journey.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/bodylog.weight.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :immediate-finish t
          :empty-lines 1)
        org-capture-templates)

  (push `("df" "Food Tracking" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/bodylog.food.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :immediate-finish t
          :empty-lines 1)
        org-capture-templates)

  (push `("dd" "Downtime Tracking" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/bodylog.dt.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/microblog.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; New blogpost idea
  (push `("bb" "New Blogpost entry" entry
          (file+headline org-blogpost-file "Meta: Blogposts to write")
          (file ,(expand-file-name "capture-templates/todo.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/feedback.others.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; The monthly newsletter to send to investors, friends and mentors
  (push `("fn" "Company Newsletters" entry
          (file+headline org-company-file "Company Newsletters")
          (file ,(expand-file-name "capture-templates/business.updates.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  ;; Capture suggestions / ideas from other people, which can be
  ;; expanded into actual projects later.
  (push `("fs" "Ideas and Suggestions" entry
          (file+headline org-company-file "Ideas and Suggestions")
          (file ,(expand-file-name "capture-templates/suggestion.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/workday.start.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  (push `("pe" "The End of Day Reflection Routine" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/workday.end.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)

  (push `("pn" "The Next Day Intentions Routine" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/workday.next.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/meeting.org" user-emacs-directory))
          :prepend t
          :clock-in t
          :clock-resume t
          :time-prompt t)
        org-capture-templates)

  ;; Capture notes for an ongoing meeting or a meeting that's already
  ;; happened.
  (push `("mn" "Meeting today" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/meeting.org" user-emacs-directory))
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
          (file ,(expand-file-name "capture-templates/habit.org" user-emacs-directory)))
        org-capture-templates)

  ;; One-click Capture for replying to emails from notmuch. Creates a
  ;; task to remind you that you need to reply to this email.
  (push `("tr" "Respond to email" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/reply.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates)

  ;; One-click capture of links from the clipboard. Used in conjunction
  ;; with `org-protocol', or as a stand-alone to capture links.
  (push `("tw" "Website Link Immediate Capture" entry
          (file+olp org-default-notes-file "Links Captured from the Browser")
          (file ,(expand-file-name "capture-templates/website.org" user-emacs-directory))
          :immediate-finish t)
        org-capture-templates)

  ;; A more nuanced capture for browser links, which I use for cleaning
  ;; out my browser 2/3 times a week.
  (push `("tl" "Website Link Pinboard Capture" entry
          (file+olp org-default-notes-file "Links Captured from the Browser")
          (file ,(expand-file-name "capture-templates/pinboard.capture.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates)

  ;; Capture a task where someone expects me to communicate when it's done
  (push `("tj" "Jira or External-facing Task" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/jira.org" user-emacs-directory))
          :clock-in t
          :clock-resume t)
        org-capture-templates)

  ;; One-click Capture for Tasks. Captures the task immediately and gets
  ;; out of your way.
  (push `("ti" "Simple Task Immediate Finish" entry
          (file+olp+datetree org-default-notes-file)
          (file ,(expand-file-name "capture-templates/todo.org" user-emacs-directory))
          :clock-in t
          :clock-resume t
          :immediate-finish t)
        org-capture-templates))

;;;; Sales and CRM capture templates
(use-package org-capture
  :ensure nil
  :config
;;; Default definitions for variables used in capture templates
  (when (not (boundp 'org-crm-file))
    (defvar org-crm-file org-default-notes-file
      "File in which CRM data is stored."))
  (when (not (boundp 'org-sales-file))
    (defvar org-sales-file org-default-notes-file
      "File in which Sales Deal data is stored."))

;;; *CRITICAL NOTE* Read before modifying the push stack below:
  ;; Pushing to capture templates is a stack. What goes in first shows
  ;; up at the bottom of the capture templates list.

  (push '("s" "Sales and CRM templates") org-capture-templates)
  (push `("sl" "New Sales Lead" entry
          (file+headline org-sales-file "Leads to be Refiled")
          (file ,(expand-file-name "capture-templates/sales.newlead.org" user-emacs-directory))
          :prepend t
          :kill-buffer t
          :empty-lines-after 1
          :clock-in t
          :clock-resume t)
        org-capture-templates)
  (push `("sc" "New Company in CRM" entry
          (file org-crm-file)
          (file ,(expand-file-name "capture-templates/crm.newcompany.org" user-emacs-directory))
          :prepend t
          :clock-in t
          :clock-resume t
          :kill-buffer t
          :empty-lines-after 1)
        org-capture-templates)
  (push `("sp" "New Person in CRM" entry
          (file+headline org-crm-file "CRM to be Refiled")
          (file ,(expand-file-name "capture-templates/crm.newperson.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates)
  (push `("sm" "Sales Related Meeting" entry
          (file+headline org-sales-file "Sales to be Refiled")
          (file ,(expand-file-name "capture-templates/sales.newmeeting.org" user-emacs-directory))
          :prepend nil
          :clock-in t
          :clock-resume t
          :empty-lines 1)
        org-capture-templates))

;;; Ledger for personal finance management, plain-text accounting
(use-package ledger-mode
  :ensure t)

;;; Publishing org-mode content

;;;; Explicit dependency on ox-gfm, because I use my fork of it.
(use-package ox-gfm
  :ensure (:host github :repo "vedang/ox-gfm" :branch "master"))

;;;; From Denote to Markdown, with front-matter intact
(use-package denote-publish
  :ensure (:host github :repo "vedang/denote-publish" :branch "main")
  :config
  ;; ## Project-specific directories
  (defvar vm-base-dir)
  (defvar vm-publishing-dir)

  ;; ## Convert the Front-Matter from org to md format.

  (setq vm-base-dir (expand-file-name "~/Tresors/Documents/diary/notes/published"))
  (setq vm-publishing-dir (expand-file-name "~/src/prototypes/vedang.me/v7/components/content/resources/content"))

  (setq org-publish-project-alist
        `(("vedangme" .
           (:base-directory ,vm-base-dir
                            :publishing-directory ,vm-publishing-dir
                            :publishing-function denote-publish-to-md
                            :recursive nil
                            :exclude-tags ("noexport" "draft" "private")
                            :section-numbers nil
                            :with-creator nil
                            :with-toc nil
                            :auto-sitemap t
                            :makeindex t)))))

(use-package org-contrib
  :ensure t
  :after org
  :config
  (require 'org-checklist))

(defun vedang-personal--org-list-top-level-to-subtree (list &optional params)
  "Convert LIST into an Org subtree.
LIST is as returned by `org-list-to-lisp'.  PARAMS is a property
list with overruling parameters for `org-list-to-generic'."
  (let* ((blank (pcase (cdr (assq 'heading org-blank-before-new-entry))
                  (`t t)
                  (`auto (save-excursion
                           (org-with-limited-levels (outline-previous-heading))
                           (org-previous-line-empty-p)))))
         (level (org-reduced-level (or (org-current-level) 0)))
         (make-heading-list-prefix
          (lambda (_type depth &optional _count)
            ;; Return the string for the heading, depending on DEPTH
            ;; of current sub-list.
            (if (= 1 depth)
                (concat (make-string (if org-odd-levels-only
                                         (1- (* 2 (+ level 1)))
                                       (+ level 1))
                                     ?*)
                        " ")
              (if (= 2 depth)
                  "- "
                (concat (make-string (* 2 (1- (1- depth))) ? )
                        (if (cl-oddp depth)
                            "+ "
                          "- ")))))))
    (org-list-to-generic
     list
     (org-combine-plists
      (list :splice t
            :istart make-heading-list-prefix
            :icount make-heading-list-prefix
            :isep (if blank "\n\n" "\n")
            :iend "\n"
            :dtstart " "
            :dtend " "
            :cbon "[X] "
            :cboff "[ ] "
            :cbtrans "[/] ")
      params))))

(defun vedang-personal--org-list-make-top-level-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (if (not (ignore-errors (goto-char (org-in-item-p))))
      (error "Not in a list")
    (let ((list (org-list-to-lisp t)))
      (save-excursion (insert (vedang-personal--org-list-top-level-to-subtree list))))))

(define-key org-mode-map (kbd "C-c C-*")
            'vedang-personal--org-list-make-top-level-subtree)

(provide 'vedang-personal)
