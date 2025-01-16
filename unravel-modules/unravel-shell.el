;;;; Vterm
(use-package vterm
  :ensure t
  :bind
  ("C-x m" . vterm)
  :config
  (setq vterm-shell (executable-find "fish")))

;;; Enrich zoxide db based on everything I open in Emacs
(when (executable-find "zoxide")
  (use-package zoxide
    :ensure t
    :hook
    (consult-after-jump . zoxide-add)
    (find-file . zoxide-add)
    (dired-mode . zoxide-add)))

;; Mode to read/write fish functions and files
(use-package fish-mode
  :ensure t)

(use-package dwim-shell-command
  :ensure t
  :bind
  ( :map global-map
    ([remap shell-command] . dwim-shell-command)
    :map dired-mode-map
    ([remap dired-do-async-shell-command] . dwim-shell-command)
    ([remap dired-do-shell-command] . dwim-shell-command)
    ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  ;; Also make available all the utility functions provided by Xenodium
  (require 'dwim-shell-commands))

(provide 'unravel-shell)
