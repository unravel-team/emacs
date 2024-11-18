;;;; Vterm
(use-package vterm
  :ensure t
  :bind
  ("C-x m" . vterm)
  :config
  (setq vterm-shell (or (executable-find "fish") "/opt/homebrew/bin/fish")))

(provide 'unravel-shell)
