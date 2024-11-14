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
;; reset it later by enabling `gcmh-mode'. Not resetting it will cause
;; stuttering/freezes.

(setq gc-cons-threshold most-positive-fixnum)

;; When both .el and .elc / .eln files are available,
;; load the latest one.

(setq load-prefer-newer t)

;; Ensure that `describe-package' does not require a
;; `package-refresh-contents'.
(setq package-enable-at-startup t)

;; Name the default frame
;; You can select a frame with M-x select-frame-by-name
(add-hook 'after-init-hook (lambda () (set-frame-name "unravel")))
