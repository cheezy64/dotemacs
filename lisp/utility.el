(defun evilcvn--change-symbol(fn)
  (let ((old (thing-at-point 'symbol)))
    (funcall fn)
    (unless (evil-visual-state-p)
      (kill-new old)
      (evil-visual-state))
    (evil-ex (concat "'<,'>s/" (if (= 0 (length old)) "" "\\<\\(") old (if (= 0 (length old)) "" "\\)\\>/"))))
  )

(defun evilcvn-change-symbol-in-whole-buffer()
  "mark the region in whole buffer and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive)
  (evilcvn--change-symbol 'mark-whole-buffer)
  )

(defun evilcvn-change-symbol-in-defun ()
  "mark the region in defun (definition of function) and use string replacing UI in evil-mode
to replace the symbol under cursor"
  (interactive)
  (evilcvn--change-symbol 'mark-defun)
  )

(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (cond
   ;; display-graphic-p need windows 23.3.1
   ((and (display-graphic-p) x-select-enable-clipboard)
    (x-set-selection 'CLIPBOARD msg))
   (t (with-temp-buffer
        (insert msg)
        (shell-command-on-region (point-min) (point-max)
                                 (cond
                                  ((eq system-type 'cygwin) "putclip")
                                  ((eq system-type 'darwin) "pbcopy")
                                  (t "xsel -ib")
                                  )))
    )))

(defun cp-filename-of-current-buffer ()
  "copy file name (NOT full path) into the yank ring and OS clipboard"
  (interactive)
  (let (filename)
    (when buffer-file-name
      (setq filename (file-name-nondirectory buffer-file-name))
      (copy-yank-str filename)
      (message "filename %s => clipboard & yank ring" filename)
      )))

(defun cp-filename-line-number-of-current-buffer ()
  "copy file:line into the yank ring and clipboard"
  (interactive)
  (let (filename linenum rlt)
    (when buffer-file-name
      (setq filename (file-name-nondirectory buffer-file-name))
      (setq linenum (save-restriction
                      (widen)
                      (save-excursion
                        (beginning-of-line)
                        (1+ (count-lines 1 (point))))))
      (setq rlt (format "%s:%d" filename linenum))
      (copy-yank-str rlt)
      (message "%s => clipboard & yank ring" rlt)
      )))

(defun cp-fullpath-of-current-buffer ()
  "copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (let (name)
      (setq name (file-truename buffer-file-name))
      ;; For Windows, replace slashes with appropriate slash
      (if (eq system-type 'windows-nt)
          (setq name (replace-regexp-in-string "/" "\\\\" name))
        )
      (copy-yank-str name)
      (message "file full path => clipboard & yank ring"))
    ))

;; http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html
(defun replace-slash-toggle ()
  "Replace forward slash with back slash and vice versa
  in the current region or line.
  If there's a text selection, work on the selected text."
  (interactive)
  (let (li bds)
    (setq bds
          (if (use-region-p)
              (cons (region-beginning) (region-end))
              (bounds-of-thing-at-point 'line)))
    (setq li (buffer-substring-no-properties (car bds) (cdr bds)))
    (if (> (cl-count 47 li) (cl-count 92 li))
        (replace-string "/" "\\" nil (car bds) (cdr bds))
        (replace-string "\\" "/" nil (car bds) (cdr bds)))))

(defun move-buf-to-new-frame ()
  "Moves current buffer to new frame and current window to previous buffer"
  (interactive)
  (let ((target (current-buffer)))
    (previous-buffer)
    (switch-to-buffer-other-frame target)))

(defun get-parent-dir (dir)
  "Returns the parent directory of DIR, or an empty string if one
isn't found."
  (let ((parent-dir (replace-regexp-in-string "[A-Za-z0-9._-]+/?$" "" dir)))
    (if (equal dir
               parent-dir)
        ""
      parent-dir)))

(defun visit-next-file-with-base-name (&optional otherWindow)
  "Cycles between files with the same basename as the given file.
Useful for cycling between header .h/.cpp/.ipp files etc."
  (interactive)
  (let* ((currentBufferFileName (replace-regexp-in-string "^.*/" "" (buffer-file-name)))
         (currentDirectory (replace-regexp-in-string "[a-zA-Z0-9._-]+$" "" (buffer-file-name)))
         (dotAtBeginning (equal ?\056 (aref currentBufferFileName 0)))
         (dotAnywhere (find ?\056 currentBufferFileName)))
    (unless (or dotAtBeginning (not dotAnywhere))
      (let* ((fileBaseName (replace-regexp-in-string "\\..*" "" currentBufferFileName))
             (matchFileList (directory-files currentDirectory t (concat "^" fileBaseName "\\.")))
             (identifiedFileList (member (buffer-file-name)
                                         matchFileList)))
        (unless (<= (length matchFileList) 1)
          (if (eq (length identifiedFileList) 1)
              (if otherWindow
                  (find-file-other-window (car matchFileList))
                (find-file (car matchFileList)))
            (if otherWindow
                (find-file-other-window (cadr identifiedFileList))
              (find-file (cadr identifiedFileList)))))))))

(defun documentation-std ()
  "Looks up a given function/keyword on cplusplus.com"
  (interactive)
  (browse-url (concat "http://www.cplusplus.com/search.do?q="
                      (documentation-common))))

(defun documentation-common ()
  "Infrastructure function that other documentation commands use."
  (let ((wordToLookup (read-string (concat "Keyword to lookup"
                                           (if (not (string-equal (thing-at-point 'word) ""))
                                               (concat " (" (thing-at-point 'word) "): ")
                                             ": ")))))
    (if (string-equal wordToLookup "")
        (setq wordToLookup (thing-at-point 'word)))
    wordToLookup))


(provide 'utility)
