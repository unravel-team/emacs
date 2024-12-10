;; early-init.el -*- lexical-binding: t; -*-

;;;; No GUI
;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; A big contributor to startup times is garbage collection.

;; We up the gc threshold to temporarily prevent it from running, then
;; reset it later after startup is complete. Not resetting it will
;; cause stuttering/freezes.

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8))
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist prot-emacs--file-name-handler-alist)
            (setq vc-handled-backends prot-emacs--vc-handled-backends)))

;; When both .el and .elc / .eln files are available,
;; load the latest one.

(setq load-prefer-newer t)

;; Ensure that `describe-package' does not require a
;; `package-refresh-contents'.
(setq package-enable-at-startup t)

;; Name the default frame
;; You can select a frame with M-x select-frame-by-name
(add-hook 'after-init-hook (lambda () (set-frame-name "unravel/emacs")))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
