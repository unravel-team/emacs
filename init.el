;; init.el -*- lexical-binding: t; -*-

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

;;; Install Elpaca

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support for Elpaca
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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

(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :ensure (:wait t)
  :demand t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("UNRAVEL_EMACS_VEDANG" "DENOTE_DIRECTORY" "ORG_DIRECTORY" "VEDANG_PRIVATE_CONFIG_FILE" "ANTHROPIC_API_KEY" "CEREBRAS_API_KEY" "DEEPSEEK_API_KEY" "GEMINI_API_KEY" "GROQ_API_KEY" "MOONSHOT_API_KEY" "OPENAI_API_KEY" "OPENROUTER_API_KEY")))

(require 'unravel-theme)
(require 'unravel-essentials)
(require 'unravel-completion)
(require 'unravel-search)
(require 'unravel-dired)
(require 'unravel-window)
(require 'unravel-git)
(require 'unravel-org)
(require 'unravel-shell)
(require 'unravel-langs)
(require 'unravel-study)
;;; Define this ENV var to true if you want to use my personal settings
;;; (like specific directories or org variables). These are not
;;; guaranteed to work without understanding the configuration.
(when (getenv "UNRAVEL_EMACS_VEDANG")
  (require 'vedang-personal))

;; Name the default frame
;; You can select a frame with M-x select-frame-by-name
(add-hook 'elpaca-after-init-hook (lambda () (set-frame-name "unravel/emacs")))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
