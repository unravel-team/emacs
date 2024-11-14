;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable custom.el by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable these commands which have been disabled by default
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; Disable these commands which have been enabled by default
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("unravel-modules" "custom-lisp"))

;;;; Packages

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Let `package-install' suggest upgrades for built-in packages too.
(setq package-install-upgrade-built-in t)

(defmacro prot-emacs-comment (&rest body)
  "Determine what to do with BODY.

If BODY contains an unquoted plist of the form (:eval t) then
return BODY inside a `progn'.

Otherwise, do nothing with BODY and return nil, with no side
effects."
  (declare (indent defun))
  (let ((eval))
    (dolist (element body)
      (when-let* (((plistp element))
                  (key (car element))
                  ((eq key :eval))
                  (val (cadr element)))
        (setq eval val
              body (delq element body))))
    (when eval `(progn ,@body))))

(defmacro prot-emacs-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of (i) string pairs mapping the
abbreviation to its expansion or (ii) a string and symbol pair
making an abbreviation to a function."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
                (if (stringp expansion)
                    `(define-abbrev ,table ,abbrev ,expansion)
                  `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split definitions 2)))
     (error "%s is not an abbrev table" ,table)))

(require 'unravel-theme)
(require 'unravel-essentials)
;; (require 'unravel-modeline)
(require 'unravel-completion)
;; (require 'unravel-search)
;; (require 'unravel-dired)
;; (require 'unravel-window)
;; (require 'unravel-git)
;; (require 'unravel-org)
(require 'unravel-langs)
