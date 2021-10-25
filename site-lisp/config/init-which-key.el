
(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)

  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)

  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  ;; (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  ;; (which-key-add-key-based-replacements doom-leader-key "<leader>")
  ;; (which-key-add-key-based-replacements doom-localleader-key "<localleader>"))
(provide 'init-which-key)
