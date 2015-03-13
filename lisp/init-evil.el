(require 'evil)
(evil-mode t)

(evil-define-command cofi/evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?k)
        (exit-key ?j))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

;; Move character back when exiting inser mode like VIM
(setq evil-move-cursor-back t)

;; (evil-set-initial-state 'org-mode 'emacs)
;; Remap org-mode meta keys for convenience
(evil-declare-key 'normal org-mode-map
    "gh" 'outline-up-heading
    "gl" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    (kbd "TAB") 'org-cycle
    )

(cl-loop for (mode . state) in '((minibuffer-inactive-mode . emacs)
				 (ggtags-global-mode	. emacs)
				 (grep-mode		. emacs)
				 (Info-mode		. emacs)
				 (term-mode		. emacs)
				 (sdcv-mode		. emacs)
				 (anaconda-nav-mode	. emacs)
				 (log-edit-mode		. emacs)
				 (inf-ruby-mode		. emacs)
				 (direx:direx-mode	. emacs)
				 (yari-mode		. emacs)
				 (erc-mode		. emacs)
				 (gud-mode		. emacs)
				 (help-mode		. emacs)
				 (eshell-mode		. emacs)
				 (shell-mode		. emacs)
				 (magit-log-edit-mode	. insert)
				 (weibo-timeline-mode	. emacs)
				 (weibo-post-mode	. emacs)
				 (sr-mode		. emacs)
				 (dired-mode		. emacs)
				 (compilation-mode	. emacs)
				 (speedbar-mode		. emacs)
				 (magit-commit-mode	. normal)
				 (magit-diff-mode	. normal)
				 (js2-error-buffer-mode . emacs))
         do (evil-set-initial-state mode state))

(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

(define-key evil-normal-state-map "Y" (kbd "y$"))

;; Smart navigation of visual line if line is wrapped
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; @see http://stackoverflow.com/questions/10569165/how-to-map-jj-to-esc-in-emacs-evil-mode
;; @see http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
(define-key evil-insert-state-map "k" #'cofi/evil-maybe-exit)

(define-key evil-insert-state-map (kbd "M-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "M-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "M-k") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "M-k") 'evil-exit-visual-state)
(define-key minibuffer-local-map (kbd "M-k") 'abort-recursive-edit)
(define-key evil-insert-state-map (kbd "M-j") 'my-yas-expand)
(define-key evil-emacs-state-map (kbd "M-j") 'my-yas-expand)
(global-set-key (kbd "M-k") 'keyboard-quit)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;;; change mode-line color by evil state
;;(lexical-let ((default-color (cons (face-background 'mode-line)
;;                                   (face-foreground 'mode-line))))
;;  (add-hook 'post-command-hook
;;            (lambda ()
;;              (let ((color (cond ((minibufferp) default-color)
;;                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;;                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;                                 (t default-color))))
;;                (set-face-background 'mode-line (car color))
;;                (set-face-foreground 'mode-line (cdr color))))))

;; change the cursor color depending on the mode
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(provide 'init-evil)
