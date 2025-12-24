;;; Everything related to the look of Emacs

;;; The Ef (εὖ) themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/ef-themes>.
(use-package ef-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . ef-themes-rotate)
   ("C-<f5>" . ef-themes-select))
  :config
  (setq ef-themes-to-toggle '(ef-elea-light ef-elea-dark))
  (setq ef-themes-variable-pitch-ui t)
  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-rotate ef-themes-items)
  (setq ef-themes-headings      ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4))  ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1))))
  (setq ef-themes-disable-other-themes t)
  (mapc #'disable-theme custom-enabled-themes))

;;; Alabaster Themes!
(use-package alabaster-themes
  :ensure (:host github :repo "vedang/alabaster-themes")
  :commands (alabaster-themes-select))

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (elpaca-after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  (setopt lin-face 'lin-cyan))

;;;; Increase padding of windows/frames
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (elpaca-after-init . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '(:internal-border-width 30
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8
          :left-fringe-width 20
          :right-fringe-width 20))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line t))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind ( :map ctl-x-x-map
          ("c" . rainbow-mode)) ; C-x x c
  :hook (emacs-lisp-mode . prot/rainbow-mode-in-themes))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.2)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :cursor-type (bar . 2)
           :blink-cursor-mode -1)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50)
          (underscore-no-other-window
           :inherit underscore
           :cursor-in-non-selected-windows nil)
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  (cursory-mode 1))

;;;; Theme buffet
;; <https://git.sr.ht/~bboal/theme-buffet>
(use-package theme-buffet
  :ensure t
  :after (:any modus-themes ef-themes alabaster-themes)
  :config
  (let ((modus-themes-p (featurep 'modus-themes))
        (ef-themes-p (featurep 'ef-themes))
        (alabaster-themes-p (featurep 'alabaster-themes)))
    (setq theme-buffet-menu 'end-user)
    (setq theme-buffet-time-offset 0)
    ;; My list of themes when I want to use ef-themes
    ;; (setq theme-buffet-end-user
    ;;       '( :night     (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis ef-owl)
    ;;          :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light ef-eagle)
    ;;          :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
    ;;          :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
    ;; My list of themes when I want to use alabaster
    (setq theme-buffet-end-user
          '( :night     (alabaster-themes-dark alabaster-themes-dark-mono)
             :morning   (alabaster-themes-light-bg alabaster-themes-light)
             :afternoon (alabaster-themes-light-bg alabaster-themes-light)
             :evening   (alabaster-themes-dark)))

    (when (or alabaster-themes-p modus-themes-p ef-themes-p)
      (theme-buffet-timer-hours 2)
      (theme-buffet-a-la-carte))))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-height 130)
          (regular
           :default-height 150)
          (medium
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 190)
          (presentation
           :inherit medium
           :default-height 250)
          (jumbo
           :inherit medium
           :default-height 330)
          (t
           ;; See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka"
           :default-weight normal
           :variable-pitch-family "Iosevka"
           :variable-pitch-height 1.05)))

  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package show-font
  :ensure t
  :commands (show-font-select-preview show-font-list)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setq show-font-pangram 'prot)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"))

;;;;; `variable-pitch-mode' setup
(use-package face-remap
  :ensure nil
  :functions prot/enable-variable-pitch
  :bind ( :map ctl-x-x-map
          ("v" . variable-pitch-mode))
  :hook ((text-mode notmuch-show-mode elfeed-show-mode) . prot/enable-variable-pitch)
  :config
  ;; NOTE 2022-11-20: This may not cover every case, though it works
  ;; fine in my workflow.  I am still undecided by EWW.
  (defun prot/enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
;;;;; Resize keys with global effect
  :bind
  ;; Emacs 29 introduces commands that resize the font across all
  ;; buffers (including the minibuffer), which is what I want, as
  ;; opposed to doing it only in the current buffer.  The keys are the
  ;; same as the defaults.
  (("C-x C-=" . global-text-scale-adjust)
   ("C-x C-+" . global-text-scale-adjust)
   ("C-x C-0" . global-text-scale-adjust)))

(provide 'unravel-theme)
