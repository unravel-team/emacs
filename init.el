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
 '("unravel-modules"))

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

(setq package-install-upgrade-built-in t)

(require 'unravel-theme)
(require 'unravel-essentials)
;; (require 'unravel-modeline)
;; (require 'unravel-completion)
;; (require 'unravel-search)
;; (require 'unravel-dired)
;; (require 'unravel-window)
;; (require 'unravel-git)
;; (require 'unravel-org)
(require 'unravel-langs)
