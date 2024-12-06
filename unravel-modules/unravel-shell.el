;;;; Vterm
(use-package vterm
  :ensure t
  :bind
  ("C-x m" . vterm)
  :config
  (setq vterm-shell (or (executable-find "fish") "/opt/homebrew/bin/fish")))

;;; Enrich zoxide db based on everything I open in Emacs
(when (executable-find "zoxide")
  (use-package zoxide
    :ensure t
    :config
    (add-hook 'find-file-hook #'zoxide-add)
    (add-hook 'dired-mode-hook #'zoxide-add)))

;; Mode to read/write fish functions and files
(use-package fish-mode
  :ensure t)

(provide 'unravel-shell)
