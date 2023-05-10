;; -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.2") (compat "29.1.4.0") (dash "2.18.0"))

;;; Commentary:
;;

;;; Code:

(require 'cl-macs)
(require 'subr-x)

(require 'dash)
(require 'compat)


(defcustom starhugger-api-token nil
  "Hugging Face user access tokens.
Generate yours at https://huggingface.co/settings/tokens."
  :group 'starhugger)

(defcustom starhugger-model-endpoint-api-url
  "https://api-inference.huggingface.co/models/bigcode/starcoder"
  "End point URL to make HTTP request."
  :group 'starhugger)

(defun starhugger--get-all-generated-texts (str)
  (-let* ((parsed (json-parse-string str :object-type 'alist))
          ((_ . err-msg) (and (listp parsed) (assoc 'error parsed))))
    (cond
     (err-msg
      (user-error "%s" err-msg))
     (t
      (-map (lambda (elem) (alist-get 'generated_text elem)) parsed)))))

(defun starhugger--get-first-generated-text (str)
  (seq-elt (starhugger--get-all-generated-texts str) 0))

(defcustom starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log parsed responses."
  :group 'starhugger)

(defcustom starhugger-log-buffer (format " *%s-log*" 'starhugger)
  "Buffer name to log things, hidden by default."
  :group 'starhugger)

(defcustom starhugger-log-time "(%F %T) "
  "Whether to insert this time format before each entry in the log buffer."
  :group 'starhugger)

(defun starhugger--log (&rest args)
  (with-current-buffer (get-buffer-create starhugger-log-buffer)
    (dlet ((inhibit-read-only t))
      (goto-char (point-max))
      (when starhugger-log-time
        (insert (format-time-string starhugger-log-time)))
      (dolist (obj (-interpose " " args))
        (insert (format "%s" obj)))
      (insert "\n"))))

;; WHY isn't this documented?!
(defvar url-http-end-of-headers)

(defvar starhugger--last-returned nil)

(cl-defun starhugger-request (prompt callback &key data headers method)
  "CALLBACK's arguments: the response's content."
  (dlet ((url-request-method (or method "POST"))
         (url-request-data (or data (json-serialize `((inputs . ,prompt)))))
         (url-request-extra-headers
          (or headers
              `(("Content-Type" . "application/json")
                ,@
                (and (< 0 (length starhugger-api-token))
                     `(("Authorization" .
                        ,(format "Bearer %s" starhugger-api-token))))))))
    (url-retrieve
     starhugger-model-endpoint-api-url
     (lambda (status)
       (-let* ((content (buffer-substring url-http-end-of-headers (point-max))))
         (setq starhugger--last-returned (list :content content))
         (when (or init-file-debug debug-on-error debug-on-quit)
           (starhugger--log
            starhugger--last-returned
            :status status
            :header
            (buffer-substring (point-min) url-http-end-of-headers)))
         (funcall callback content)))
     nil t)))

(defun starhugger--record-generated (contents &optional display)
  (with-current-buffer (get-buffer-create starhugger-generated-buffer)
    (goto-char (point-max))
    (insert "\n\n" contents))
  (when display
    (save-selected-window
      (pop-to-buffer starhugger-generated-buffer))))

(defcustom starhugger-strip-prompt-before-insert t
  "Whether to remove the prompt in the parsed response before inserting."
  :group 'starhugger)

;;;###autoload
(defun starhugger-query (query-str &optional insert-pos display)
  "Interactive send QUERY-STR to the model.
Non-nil INSERT-POS (interactively when prefix arg: active
region's end of current point): insert the parsed response there.
Non-nil DISPLAY: displays the parsed response."
  (interactive (list
                (read-string "Prompt: "
                             (and (use-region-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))))
                (and current-prefix-arg
                     (if (use-region-p)
                         (region-end)
                       (point)))
                t))
  (-let* ((pt0 (or insert-pos (point)))
          (buf0 (current-buffer))
          (modtick (buffer-modified-tick)))
    (starhugger-request
     query-str
     (lambda (returned)
       (with-current-buffer buf0
         (-let* ((gen-text (starhugger--get-first-generated-text returned))
                 (insert-flag
                  (and insert-pos (equal modtick (buffer-modified-tick)))))
           (when insert-flag
             (deactivate-mark)
             (goto-char pt0)
             (insert
              (if starhugger-strip-prompt-before-insert
                  (string-remove-prefix query-str gen-text)
                gen-text)))
           (starhugger--record-generated gen-text
                                         (and display
                                              (not insert-pos)))))))))

(defun starhugger--complete-default-beg-position ()
  (-->
   (list
    (bounds-of-thing-at-point 'line)
    (bounds-of-thing-at-point 'paragraph)
    (bounds-of-thing-at-point 'defun))
   (-map #'car it)
   (remove nil it)
   (-min it)))

;;;###autoload
(defun starhugger-complete (&optional beginning)
  "Perform completion using the prompt from BEGINNING to current point.
BEGINNING defaults to start of current line, paragraph or defun,
whichever is the furthest."
  (interactive)
  (-let* ((beg (or beginning (starhugger--complete-default-beg-position)))
          (prompt (buffer-substring-no-properties beg (point))))
    (starhugger-query prompt (point))))

;;;###autoload
(defun starhugger-complete-using-from-buffer-begininng ()
  "Perform completion by sending all texts before current point as prompt."
  (interactive)
  (starhugger-complete (point-min)))

(defcustom starhugger-complete-commands
  '(starhugger-complete starhugger-complete-using-from-buffer-begininng)
  "Commands that by default, pressing TAB after those will continue it.
Enable `starhugger-global-mode' first."
  :group 'starhugger)

;;;###autoload
(defun starhugger--continue-complete-menu-item-filter (_)
  (and (member last-command starhugger-complete-commands) last-command))

;;;###autoload
(progn
  (define-minor-mode starhugger-global-mode
    "Currently doesn't do very much."
    :global t
    :keymap
    (let* ((tab-item `(menu-item "" nil :filter
                       starhugger--continue-complete-menu-item-filter)))
      (list
       (cons (kbd "TAB") tab-item) ;
       (cons (kbd "<tab>") tab-item) ;
       ))
    (if starhugger-global-mode
        (progn)
      (progn))))

;;; starhugger.el ends here

(provide 'starhugger)
