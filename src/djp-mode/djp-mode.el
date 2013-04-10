;;; The current implementation is a playground. We'll get back to this later.

(eval-and-compile
  (require 'cc-mode))     ; (only) for `c-populate-syntax-table'

;; TODO: remove hardcoded paths
(defvar djp-executable "~/dev/djp/src/out/Debug/bin/djp")
(defvar djp-tmp-buffer "/tmp/djp-mode-buffer")

;; Indentation
(defvar djp-indentation-char ?\s)
(defvar djp-indentation-count 4)
(defvar djp-indentation-linewrap 8)

;; Font faces
(defvar djp-face-annotation-tok-at 'nil)
(defvar djp-face-comment 'font-lock-comment-face)
(defvar djp-face-identifier 'nil)
(defvar djp-face-keyword 'font-lock-keyword-face)
(defvar djp-face-qualified-id 'nil)
(defvar djp-face-reference-type-id 'font-lock-type-face)
(defvar djp-face-string-literal 'font-lock-string-face)
(defvar djp-face-op nil)
(defvar djp-face-literal-number nil)
(defvar djp-face-literal-char nil)

(defface djp-error-face
  `((((class color) (background light))
     (:underline  "orange"))
    (((class color) (background dark))
     (:underline "orange"))
    (t (:underline t)))
  "Face for Java errors."
  :group 'djp-mode)

;; timer
(defvar djp-mode-parser-timer nil)

(defvar djp-idle-timer-delay 0.2
  "Delay in seconds before re-parsing after user makes changes.")

(defvar djp-mode-parsing nil)

(defvar djp-parse-tree nil
  "The output generated by the parser.")

(defvar djp-symbol-table []
  "The symbol table generated by the parser.")

(defvar djp-indentation-table nil
  "The indentation hash table generated by the parser.")

(defun djp-reset-parser ()
  (setq djp-parse-tree nil))

;; indentation
(defun djp-indent-line ()
  (let* ((line (line-number-at-pos))
         (indent (gethash (- line 1) djp-indentation-table))
         (indent-value 0)
         ini end indent-str)
    (if indent
        (save-excursion
          (beginning-of-line)
          (setq ini (point))
          (skip-chars-forward " \t")
          (setq end (point))
          (setq indent-value
                (+ (* (car indent) djp-indentation-count)
                   (* (cadr indent) djp-indentation-linewrap)
                   (car (cddr indent))))
          (setq indent-str (make-string indent-value djp-indentation-char))
          (if (not (string= (buffer-substring-no-properties ini end) indent-str))
              (progn
                (delete-horizontal-space)
                (insert indent-str)))))))


;; djp-mode
(defvar djp-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in djp-mode buffers.")

(defvar djp-mode-map
  (let ((map (make-sparse-keymap))
	keys)
    ;(define-key map (kbd "C-m") #'djp-enter-key)
    ;(define-key map (kbd "C-a") #'djp-beginning-of-line)
    map)
  "Keymap used in `djp-mode' buffers.")

(defvar djp-mode-abbrev-table nil
  "Abbrev table in use in `djp-mode' buffers.")
(define-abbrev-table 'djp-mode-abbrev-table ())

;;;###autoload
(defun djp-mode ()
  "Major mode for Java"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table djp-mode-syntax-table)
  (use-local-map djp-mode-map)
  (setq major-mode 'djp-mode
	mode-name "DJP"
	comment-start "//"
	comment-end "")
  (setq local-abbrev-table djp-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) #'djp-indent-line)
  (set (make-local-variable 'indent-region-function) nil)

  ;; We do our own syntax highlighting based on the parse tree.
  ;; However, we want minor modes that add keywords to highlight properly
  ;; We do this by not letting font-lock unfontify anything, and telling it to
  ;; fontify after we re-parse and re-highlight the buffer.  (We currently don't
  ;; do any work with regions other than the whole buffer.)
  (dolist (var '(font-lock-unfontify-buffer-function
                 font-lock-unfontify-region-function))
    (set (make-local-variable var) (lambda (&rest args) t)))

  ;; Don't let font-lock do syntactic (string/comment) fontification.
  (set (make-local-variable #'font-lock-syntactic-face-function)
       (lambda (state) nil))

  (add-hook 'after-change-functions #'djp-mode-edit nil t)

  (djp-reparse))

(defun djp-mode-reset-timer ()
  (if djp-mode-parser-timer
      (cancel-timer djp-mode-parser-timer))
  (setq djp-mode-parsing nil)
  (setq djp-mode-parser-timer
	(run-with-idle-timer djp-idle-timer-delay nil #'djp-reparse)))

(defun djp-mode-edit (beg end len)
  "Schedule a new parse after buffer is edited."
  (djp-mode-reset-timer))

(defun djp-reparse ()
  (let (interrupted-p)
    (unless djp-mode-parsing
      (setq djp-mode-parsing t)
      (djp-reset-parser)
      (unwind-protect
	  (progn
	    (djp-with-unmodifying-text-property-changes
	      (remove-text-properties (point-min) (point-max) '(syntax-table))
	      (remove-text-properties (point-min) (point-max) '(face nil))
	      (djp-remove-overlays)
	      (djp-clear-face (point-min) (point-max))
	      (setq interrupted-p
		    (catch 'interrupted
		      (djp-parse)
		      (djp-do-syntax-highlight)
		      nil))
	      (if interrupted-p
		  (djp-mode-reset-timer))
	      (setq djp-mode-parsing nil)
	      (unless interrupted-p
		(setq djp-mode-parse-timer nil))))))))

(defun djp-parse ()
  "We save the buffer content into a temporary file and trigger the compiler.
The output of the compiler is used to build djp-parse-tree,
djp-symbol-table and djp-indentation-table."
  (let (cmd compiler-output tmp)
    (write-region (point-min) (point-max) djp-tmp-buffer)
    (setq cmd (concat djp-executable " " djp-tmp-buffer))
    (setq compiler-output (shell-command-to-string cmd))
    (setq tmp (read-from-string compiler-output))
    (setq djp-parse-tree (car tmp))
    (setq tmp (read-from-string compiler-output (cdr tmp)))
    (setq djp-symbol-table (eval (car tmp)))
    (setq tmp (read-from-string compiler-output (cdr tmp)))
    (setq djp-indentation-table (eval (car tmp)))))

;;; TODO: this is fragile and error prone
(defun djp-do-syntax-highlight ()
  "Traverse djp-parse-tree applying font-lock face for each node)"
  (loop for node in djp-parse-tree do (eval node)))

;;; Functions from EmacsOutput that are ignored
(defun djp-package-declaration (&rest ignore) nil)
(defun djp-import-declarations (&rest ignore) nil)
(defun djp-import-declaration (&rest ignore) nil)
(defun djp-class-or-interface-declaration (&rest ignore) nil)
(defun djp-comments (&rest ignore) nil)
(defun djp-constructor-declarator-rest (&rest ignore) nil)
(defun djp-normal-class-declaration (&rest ignore) nil)
(defun djp-member-decl-modifier-member-decl (&rest ignore) nil)
(defun djp-member-decl-modifier-static-block (&rest ignore) nil)

(defun djp-comment (ini end)
  (if djp-face-comment
      (put-text-property ini end 'face djp-face-comment)))

(defun djp-node-op (ini end)
  (if djp-face-op (put-text-property ini end 'face djp-face-op)))

(defun djp-node-annotation-tok-at (pos-tok-at)
  (if djp-face-annotation-tok-at
      (put-text-property pos-tok-at
			 (+ pos-tok-at 1) 'face djp-face-annotation-tok-at)))

(defun djp-node-literal-char (ini end)
  (if djp-face-literal-char
      (put-text-property ini end 'face djp-face-literal-char)))

(defun djp-node-identifier (ini end)
  (if djp-face-identifier
      (put-text-property ini end 'face djp-face-identifier)))

(defun djp-node-keyword (ini end)
  (if djp-face-keyword
      (put-text-property ini end 'face djp-face-keyword)))

(defun djp-node-literal-number (ini end)
  (if djp-face-literal-number
      (put-text-property ini end 'face djp-face-literal-number)))

(defun djp-node-qualified-id (ini end)
  (if djp-face-qualified-id
      (put-text-property ini end 'face djp-face-qualified-id)))

(defun djp-node-reference-type-id (ini end)
  (if djp-face-reference-type-id
      (put-text-property ini end 'face djp-face-reference-type-id)))

(defun djp-node-string-literal (ini end)
  (if djp-face-string-literal
      (put-text-property ini end 'face djp-face-string-literal)))

(defun djp-error (ini end msg)
  (let ((ovl (make-overlay ini end)))
    ;; Add identifier so we can delete this overlay when reparsing
    (overlay-put ovl 'djp-error t)
    (overlay-put ovl 'face 'djp-error-face)
    (put-text-property ini end 'help-echo msg)
    (put-text-property ini end 'point-entered #'djp-echo-error)))

(defun djp-echo-error (old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (if msg
        (message msg))))

(defun djp-clear-face (beg end)
  (remove-text-properties beg end '(face nil
                                    help-echo nil
                                    point-entered nil)))

(defun djp-remove-overlays ()
  "Remove overlays from buffer that have a `djp-error' property."
  (let ((beg (point-min))
	(end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
	(when (overlay-get o 'djp-error)
	  (delete-overlay o))))))

;; Stolen from js2-mode where you can read:
;; `Stolen shamelessly from James Clark's nxml-mode.'
(defmacro djp-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
	   (progn ,@body)
	 (unless ,modified
	   (restore-buffer-modified-p nil))))))

(provide 'djp-mode)

;;; djp-mode.el ends here
