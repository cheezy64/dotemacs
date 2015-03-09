(add-to-list 'auto-mode-alist '("\\.ipp\\'"  . c++-mode))

(defun my-c-mode-hook ()
  (setq c-style-variables-are-local-p nil)
  (setq c-auto-newline nil)
  (setq c-default-style "ellemtel"
    c-basic-offset 3)
  (c-set-style "ellemtel")
)

(add-hook 'c-mode-common-hook
  (lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      ;; indent
      ;; gtags (GNU global) stuff
      (setq gtags-suggested-key-mapping t)
      (helm-gtags-mode 1)
    )
    (when (derived-mode-p 'c-mode 'c++-mode)
      (my-c-mode-hook)
    )
  )
)

(provide 'init-cc-mode)
