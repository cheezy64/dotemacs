;; FILE  : package-mode.el
;; AUTHOR: Justin Davis (justin.davis@ni.com)
;; DESC  : A major mode for NI package files

(defvar package-mode-hook
  nil
  "Hook run after entering package file mode.")

(defvar package-mode-map
  (let ((package-mode-map (make-keymap)))
    (define-key package-mode-map [mouse-1] 'package-mode-clickable-dispatch)
    (define-key package-mode-map "\C-cd"   'package-mode-jump-to-dep)
    package-mode-map)
  "Keymap for package file major mode")

(add-to-list 'auto-mode-alist '("package$" . package-mode))

(defun package-mode-clickable-dispatch (event)
  "This function is run when a user click the mouse somewhere in
a package file."
  (interactive "e")
  (package-mode-jump-to-dep (posn-point (event-end event))))

(defun package-mode-jump-to-dep (point-loc)
  "Jump to the dependency entry at point.  If called
interactively and point is not on a dependency, ask the user for
a dependency name."
  (interactive "d")
  ;; Get the text properties @ point
  (let ((text-props (text-properties-at point-loc)))
    ;; If the use clicked on a highlighted dependency
    (if (eq (plist-get text-props 'mouse-face)
            'highlight)
        (package-mode-jump-to-dependency (current-word t nil))
      ;; Otherwise, if this function was called interactively
      (if (called-interactively-p)
          (call-interactively 'package-mode-jump-to-dependency)))))

(defconst package-mode-font-lock-keywords
  '(
    ("\\<\\(component\\|dependency\\|subcomponent\\)\\>"
     (0 font-lock-builtin-face))
    ("\\(?:\\<\\|[a-zA-Z0-9_]+\\)\\(appendM\\(?:\\(?:ajorVersion\\|odeSuffix\\)ToTarget\\)\\|currentVersion\\|[dD]\\(?:ependencies\\|ocumentationURL\\)\\|forwardNamespaces\\|name\\(?:space\\)?\\|oldestCompatibleVersion\\|p\\(?:erforcePath\\|urpose\\)\\|t\\(?:\\(?:argetBaseNam\\|yp\\)e\\)\\|userDefinedMainNamespaces\\|version\\)\\>"
     (0 font-lock-keyword-face))
    ("\\<\\(?:dependency\\|\\(?:sub\\)?component\\) +\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-constant-face nil t))
    ("\\(current\\|dynamiclib\\|inherit\\|program\\|staticlib\\|t\\(?:est\\|ool\\)\\)"
     (0 font-lock-constant-face))
  )
  "Highlighting expressions for package mode")

(defvar package-mode-syntax-table
  (let ((package-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\n ">" package-mode-syntax-table)  ;; Newline ends package file comments
    (modify-syntax-entry ?#  "<" package-mode-syntax-table)  ;; '#' starts package file comments
    package-mode-syntax-table)
  "Syntax table for package-mode")

(defun package-mode-unhighlight-all ()
  "Remove all of the \"clicky\" highlighting in the buffer."
  ;; Check if the buffer is currently in a modified state
  (let ((buffer-modified (buffer-modified-p)))
    (remove-text-properties (point-min)
                            (point-max)
                            '(mouse-face highlight
                                         help-echo "Click to jump to dependency"))
    ;; If buffer wasn't modified to begin with, call
    ;; set-buffer-modified-p to make sure buffer doesn't get confused.
    (if (not buffer-modified)
        (set-buffer-modified-p nil))))

(defun package-mode-highlight-deps ()
  "Highlight all component dependencies with the \"mouse clicky\" face."
  (let ((regexp-count)
        (read-only-disabled nil)
        (buffer-modified nil)
        (quote-pt nil))
    ;; If package mode buffer is read only, make it temporarily writable.
    (when buffer-read-only
      (toggle-read-only nil)
      (setq read-only-disabled t))
    ;; Before messing with the text properties, keep track of the
    ;; buffer modification state.
    (setq buffer-modified (buffer-modified-p))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^ *\\(?:[a-zA-Z0-9]+\\)?[dD]ependencies\\> *=" nil t)
        ;; Save the location of the closing quote
        (setq quote-pt
              (save-excursion
                (search-forward-regexp "\"" nil nil 2)
                (point)))
        ;; Find all listed dependencies between the quotes
        (while (re-search-forward "\\([a-zA-Z0-9_]+\\)"
                                  quote-pt
                                  t)
          (add-text-properties
           (- (point) (length (match-string 1))) ; Start of dep
           (point) ; End of dep
           '(mouse-face highlight
                        help-echo "Click to jump to dependency")))))
    ;; If the package file was read-only to begin with, make it read-only again
    (if read-only-disabled
        (toggle-read-only t))
    ;; If the buffer was not-modified to begin with, set the buffer
    ;; state to a "not-modified" state
    (if (not buffer-modified)
        (set-buffer-modified-p nil))))

(defun package-mode-update-highlighting ()
  "Update the dependency highlighting in the buffer.  This
function just unhighlights everything and then highlights
everything again."
  (package-mode-unhighlight-all)
  (package-mode-highlight-deps))

(defun package-mode-jump-to-dependency (dependency)
  "Move point to the dependency entry for DEPENDENCY."
  (interactive "MDependency Name: \n")
  (let ((found-pt nil))
    (save-excursion
      (goto-char (point-min))
      (setq found-pt (re-search-forward (concat "dependency \\<" dependency "\\>") nil t))
      (if (not found-pt)
          (setq found-pt (re-search-forward (concat "component \\<" dependency "\\>") nil t)))
      (if (not found-pt)
          (setq found-pt (re-search-forward (concat "subcomponent \\<" dependency "\\>") nil t))))
    (if found-pt
        ;; Jump to found point
        (progn
          (goto-char
           (save-excursion
             (goto-char found-pt)
             (point-at-bol))))
      (progn
        ;; no point found, try trimming #'s off..eg. "nictrl160" -> "nictrl"
        (let ((newstring (replace-regexp-in-string "[0-9]+$" "" dependency )))
          (if (string-equal dependency newstring)
              (message "No \"%s\" component or dependency found" dependency)
            (package-mode-jump-to-dependency newstring)))))))

;;;###autoload
(defun package-mode ()
  "Major mode for editing package files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table package-mode-syntax-table)
  (use-local-map package-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(package-mode-font-lock-keywords))
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (setq major-mode 'package-mode)
  (setq mode-name "package")
  (package-mode-highlight-deps)
  ;; Re-highlight after save
  (add-hook
   'after-save-hook
   '(lambda ()
      (package-mode-update-highlighting))
   nil
   t ;; Buffer-local hook
   )
  (run-hooks 'package-mode-hook))

(provide 'package-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regex Sandbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If we need to add new package-mode keywords, use the following
;; commented-out commands to generate new optimized regular
;; expressions.

;; Top level
;; (regexp-opt '(
;;               "component"
;;               "subcomponent"
;;               "dependency"
;;               ) t)

;; (regexp-opt '(
;;               "name"
;;               "type"
;;               "version"
;;               "purpose"
;;               "namespace"
;;               "dependencies"
;;               "perforcePath"
;;               "oldestCompatibleVersion"
;;               "targetBaseName"
;;               "appendMajorVersionToTarget"
;;               "appendModeSuffixToTarget"
;;               "userDefinedMainNamespaces"
;;               "currentVersion"
;;               "forwardNamespaces"
;;               "documentationURL"
;;               ) t)

;; (regexp-opt '(
;;               "dynamiclib"
;;               "staticlib"
;;               "program"
;;               "tool"
;;               "inherit"
;;               "test"
;;               "current"
;;               ) t)
