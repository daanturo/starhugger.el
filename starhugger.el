;; -*- lexical-binding: t; -*-

;; Version: 0.1.11
;; Package-Requires: ((emacs "28.2") (compat "29.1.4.0") (dash "2.18.0") (spinner "1.7.4"))

;;; Commentary:

;; Client for AI-powered code completion(s).

;;; Code:

(require 'cl-macs)
(require 'subr-x)

(require 'dash)
(require 'compat)

(defcustom starhugger-api-token nil
  "Hugging Face user access tokens.
Generate yours at https://huggingface.co/settings/tokens."
  :group 'starhugger)

(defcustom starhugger-model-api-endpoint-url
  "https://api-inference.huggingface.co/models/bigcode/starcoder"
  "End point URL to make HTTP request."
  :group 'starhugger)

;;;; Making requests

(defun starhugger--get-all-generated-texts (str)
  (-let* ((parsed (json-parse-string str :object-type 'alist))
          ((_ . err-msg) (and (listp parsed) (assoc 'error parsed))))
    (cond
     (err-msg
      (user-error "%s" err-msg))
     (t
      (-map (lambda (elem) (alist-get 'generated_text elem)) parsed)))))

(defcustom starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log parsed responses."
  :group 'starhugger)

(defcustom starhugger-log-buffer (format " *%s-log*" 'starhugger)
  "Buffer name to log things, hidden by default."
  :group 'starhugger)

(defcustom starhugger-log-time "(%F %T) "
  "Whether to insert this time format before each entry in the log buffer."
  :group 'starhugger)

(defcustom starhugger-max-prompt-length (* 1024 20)
  "Max length of the prompt to send.
\"`inputs` tokens + `max_new_tokens` must be <= 8192\". The
default value was determined using trial and errors and is
model-dependant."
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

(declare-function spinner-start "spinner")
(defvar spinner-types)
(defvar starhugger--spinner-added-type nil)
(defun starhugger--spinner-start ()
  (unless starhugger--spinner-added-type
    (require 'spinner)
    (push '(starhugger . ["🤗" "⭐" "🌟" "🌠" "💫"]) spinner-types)
    (setq starhugger--spinner-added-type t))
  (spinner-start 'starhugger 3))

(defcustom starhugger-enable-spinner t
  "Show spinner when fetching."
  :group 'starhugger)

;; WHY isn't this documented?!
(defvar url-http-end-of-headers)

(defcustom starhugger-additional-data-alist
  '((parameters
     (return_full_text . :false) ; don't re-include the prompt
     )
    (options)
    ;;
    )
  "Detailed parameter list.
An association list to be converted by `json-serialize'. See
https://huggingface.co/docs/api-inference/detailed_parameters#text-generation-task
for parameters."
  :group 'starhugger)

(defcustom starhugger-max-new-tokens nil
  "max_new_tokens when a number.
See also `starhugger-additional-data-alist'."
  :group 'starhugger)

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
But prevent errors about multi-byte characters."
  (encode-coding-string (apply #'json-serialize object args) 'utf-8))

(defvar starhugger--last-request nil)
(defvar starhugger-debug nil)

(defvar starhugger-before-request-hook '()
  "Hook run before making an HTTP request.")

(defvar-local starhugger--current-request-buffer-list '())

(cl-defun starhugger--request (prompt callback &key data headers method +parameters +options)
  "CALLBACK's arguments: the response's content."
  (run-hooks 'starhugger-before-request-hook)
  (-let* ((data
           (or data
               (starhugger--json-serialize
                `((parameters ,@+parameters)
                  (options ,@+options)
                  (inputs . ,prompt))))))
    (dlet ((url-request-method (or method "POST"))
           (url-request-data data)
           (url-request-extra-headers
            (or headers
                `(("Content-Type" . "application/json")
                  ,@(and (< 0 (length starhugger-api-token))
                         `(("Authorization" .
                            ,(format "Bearer %s" starhugger-api-token))))
                  ("Connection" . "keep-alive") ; not sure about this yet
                  ))))
      (url-retrieve
       starhugger-model-api-endpoint-url
       (lambda (status)
         (-let* ((content
                  (and url-http-end-of-headers
                       (buffer-substring url-http-end-of-headers (point-max)))))
           (setq starhugger--last-request
                 (list
                  :response-content content
                  :send-data data
                  :response-status status))
           (when (or starhugger-debug (not url-http-end-of-headers))
             (starhugger--log
              starhugger--last-request
              :header (and url-http-end-of-headers
                           (buffer-substring (point-min) url-http-end-of-headers))))
           (funcall callback content)))
       nil t))))

(defun starhugger--record-generated
    (prompt parsed-response-list &optional display)
  (with-current-buffer (get-buffer-create starhugger-generated-buffer)
    (when (zerop (buffer-size))
      (insert "Initial model: " starhugger-model-api-endpoint-url "\n"))
    (goto-char (point-max))
    (apply #'insert
           "\n> "
           prompt
           "\n\n"
           (-interpose "\n\n" parsed-response-list))
    (insert "\n"))
  (when display
    (save-selected-window
      (pop-to-buffer starhugger-generated-buffer))))

(defcustom starhugger-strip-prompt-before-insert nil
  "Whether to remove the prompt in the parsed response before inserting.
Enable this when the return_full_text parameter isn't honored."
  :group 'starhugger)

(defcustom starhugger-end-token "<|endoftext|>"
  "End of sentence token."
  :group 'starhugger)

(defcustom starhugger-strip-end-token t
  "Whether to remove `starhugger-end-token' before inserting."
  :group 'starhugger)

(defun starhugger--post-process-content
    (generated-text &optional notify-end prompt)
  (-->
   generated-text
   (if starhugger-strip-prompt-before-insert
       (string-remove-prefix prompt it)
     it)
   (if starhugger-strip-end-token
       (-let* ((end-flag (string-suffix-p starhugger-end-token it)))
         (when (and end-flag notify-end)
           (message "%s received from %s !"
                    starhugger-end-token
                    starhugger-model-api-endpoint-url))
         (if end-flag
             (string-remove-suffix starhugger-end-token it)
           it))
     it)))

(defvar-local starhugger--last-prompt-beg-pos nil)

(defun starhugger--record-last-prompt-beg-pos (pos)
  "Save POS into `starhugger--last-prompt-beg-pos' locally.
The said variable is used to continue completion from there to
the point. Because only some certain keys are used for
continuation, after the next command, set it back to nil (1).
Since inserting is asynchronous, the time next command's
insertion happen will happen after (1) so the variable won't stay
nil (if inserting happens)."
  (setq starhugger--last-prompt-beg-pos pos)
  (letrec ((fn
            (lambda ()
              (setq starhugger--last-prompt-beg-pos nil)
              (remove-hook 'post-command-hook fn t))))
    (add-hook 'post-command-hook fn nil t)))

(defun starhugger-turn-off-completion-in-region-mode ()
  "Use this when inserting parsed response.
To prevent key binding conflicts such as TAB."
  (completion-in-region-mode 0))

(defvar starhugger-post-insert-hook
  '(starhugger-turn-off-completion-in-region-mode)
  "Hook run after inserting the parsed response.")

(defcustom starhugger-retry-temperature-range '(0.0 1.0)
  "The lower and upper bound of random temperature when retrying.
A single number means just use it without generating. nil means
don't set temperature at all. Set this when the model doesn't
honor use_cache = false."
  :group 'starhugger)

(defvar-local starhugger-query--last-prompt nil)

(defun starhugger--data-for-different-response ()
  `((options (use_cache . :false))
    (parameters
     ,@(and starhugger-retry-temperature-range
            `((temperature .
               ,(cond
                 ((numberp starhugger-retry-temperature-range)
                  starhugger-retry-temperature-range)
                 (t
                  (-let* (((lo hi) starhugger-retry-temperature-range))
                    (--> (cl-random 1.0) (* it (- hi lo)) (+ lo it)))))))))))

(cl-defun starhugger--query-internal (prompt callback &key display spin force-new &allow-other-keys)
  "CALLBACK is called with the generated text list."
  (-let* ((call-buf (current-buffer))
          (spin-obj
           (and spin starhugger-enable-spinner (starhugger--spinner-start)))
          ((&alist 'options +options 'parameters +parameters)
           (and force-new (starhugger--data-for-different-response)))
          ((&alist 'parameters dynm-parameters 'options dynm-options)
           starhugger-additional-data-alist)
          (prompt*
           (substring prompt
                      (max 0 (- (length prompt) starhugger-max-prompt-length)))))
    (letrec ((request-buf
              (starhugger--request
               prompt*
               (lambda (returned)
                 (with-current-buffer call-buf
                   (setq starhugger--current-request-buffer-list
                         (delete
                          request-buf starhugger--current-request-buffer-list)))
                 (when spin
                   (funcall spin-obj))
                 (when returned
                   (-let* ((gen-texts
                            (starhugger--get-all-generated-texts returned)))
                     (funcall callback gen-texts)
                     (when display
                       (starhugger--record-generated prompt* gen-texts
                                                     display)))))
               :+options `(,@+options ,@dynm-options)
               :+parameters `(,@(and starhugger-max-new-tokens
                                     `((max_new_tokens . ,starhugger-max-new-tokens)))
                              ,@+parameters ,@dynm-parameters))))
      (push request-buf starhugger--current-request-buffer-list)
      (set-process-query-on-exit-flag (get-buffer-process request-buf) nil)
      request-buf)))

;;;###autoload
(cl-defun starhugger-query (prompt &rest args &key beg-pos end-pos display force-new)
  "Interactive send PROMPT to the model.
Non-nil END-POS (interactively when prefix arg: active region's
end of current point): insert the parsed response there.
BEG-POS:the beginning of the prompt area. Non-nil DISPLAY:
displays the parsed response. FORCE-NEW: disable caching (options
wait_for_model), interactively: PROMPT equals the old one. ARGS
is optional arguments."
  (interactive (-let* ((prompt
                        (read-string "Prompt: "
                                     (and (use-region-p)
                                          (buffer-substring-no-properties
                                           (region-beginning) (region-end))))))
                 (list
                  prompt
                  :end-pos (and current-prefix-arg
                                (if (use-region-p)
                                    (region-end)
                                  (point)))
                  :display t
                  :force-new (equal starhugger-query--last-prompt prompt))))
  (-let* ((pt0 (or end-pos (point)))
          (call-buf (current-buffer))
          (modftick (buffer-modified-tick))
          (callback
           (lambda (gen-texts)
             (with-current-buffer call-buf
               (setq starhugger-query--last-prompt prompt)
               (-let* ((pt1 (point))
                       (first-gen-text (cl-first gen-texts))
                       (insert-action
                        (and end-pos
                             (equal modftick (buffer-modified-tick))
                             (lambda ()
                               (starhugger--record-last-prompt-beg-pos beg-pos)
                               (insert
                                (starhugger--post-process-content
                                 first-gen-text
                                 (or end-pos display) prompt))
                               (run-hooks 'starhugger-post-insert-hook)))))
                 (when insert-action
                   (if (= pt0 pt1)
                       (progn
                         (deactivate-mark)
                         (funcall insert-action))
                     (save-excursion
                       (goto-char pt0)
                       (funcall insert-action)))))))))
    (apply #'starhugger--query-internal prompt callback :spin t args)))

;;;; Overlay suggestion

(defvar-local starhugger--overlay nil)

(defface starhugger-suggestion-face '((t :foreground "gray" :italic t))
  "Face for suggestion overlays."
  :group 'starhugger)

;; We may as well use a ring (`make-ring'), but it doesn't have a built-in way
;; to modify elements in-place
(defvar-local starhugger--suggestion-list '())

(defcustom starhugger-suggestion-list-size 32
  "Maximum number of saved suggestions in current buffer.
Note that this includes all recently fetched suggestions so not
all of them are relevant all the time."
  :group 'starhugger)

(defun starhugger--post-self-insert-h ()
  "Keep the overlay iff the typed character is its prefix."
  (-let* ((suggt (starhugger--current-overlay-suggestion))
          (suggt-char (elt suggt 0)))
    (if (equal suggt-char last-command-event)
        (progn
          (starhugger--show-overlay (substring suggt 1)))
      (progn
        ;; keep the suggestion, except when auto which is just annoying
        (when starhugger-auto-mode
          (starhugger-dismiss-suggestion))))))

(defvar starhugger-active-suggestion-mode-hook '()
  nil)

(define-minor-mode starhugger-active-suggestion-mode
  "Not meant to be called normally."
  :global nil
  :lighter " 🌟"
  :keymap `( ;
            (,(kbd "<remap> <keyboard-quit>") . starhugger-dismiss-suggestion)
            ;;
            )
  (if starhugger-active-suggestion-mode
      (progn
        (add-hook 'post-self-insert-hook #'starhugger--post-self-insert-h nil t))
    (progn
      (remove-hook 'post-self-insert-hook #'starhugger--post-self-insert-h t)
      (when (overlayp starhugger--overlay)
        (delete-overlay starhugger--overlay)))))

(defcustom starhugger-high-number-of-suggestions-to-fetch 3
  "The number of suggestions to fetch interactively at once.
Allow quickly previewing to a different one. Use for commands
such as `starhugger-trigger-suggestion'.

Note that the model may return the same response repeatedly."
  :group 'starhugger)

(defcustom starhugger-low-number-of-suggestions-to-fetch 2
  "The number of suggestions to fetch at once when automatically.
Use for `starhugger-show-next-suggestion' and for auto mode."
  :group 'starhugger)

(defun starhugger--current-overlay-suggestion (&optional no-end-token)
  (-->
   (overlay-get starhugger--overlay 'starhugger-suggestion)
   (if no-end-token
       (string-remove-suffix starhugger-end-token it)
     it)))

(defun starhugger--show-overlay (suggt &optional orig-pt)
  (-let* ((beg-pt (or orig-pt (point)))
          (suggt*
           (propertize suggt
                       'face 'starhugger-suggestion-face
                       ;; allow placing the cursor before the overlay when
                       ;; 'before-string
                       'cursor t)))
    (overlay-put starhugger--overlay 'starhugger-suggestion suggt)
    (when orig-pt
      (overlay-put starhugger--overlay 'starhugger-original-position orig-pt))
    ;; at end of buffer, 'display doesn't show anything because
    ;; `overlay-starr'=`overlay-end'
    (if (eobp)
        (overlay-put starhugger--overlay 'before-string suggt*)
      ;; currently I can't find a way to to achieve this:

      ;; 〈before〉|〈overlay〉〈after〉

      ;; so the workaround is too concatenate "overlay" and "after" and and put
      ;; the overlay on "after"
      (overlay-put starhugger--overlay 'display
                   (concat suggt* (buffer-substring beg-pt (+ beg-pt 1)))))))

(defun starhugger--suggestion-state ()
  (vector (point) (thing-at-point 'line t)))

(defun starhugger--relevant-fetched-suggestions (&optional all)
  (-let* ((cur-state (starhugger--suggestion-state)))
    (-keep
     (-lambda ((state suggt)) (and (or all (equal cur-state state)) suggt))
     starhugger--suggestion-list)))

(defun starhugger--add-to-suggestion-list (suggestions state)
  (dolist (suggt suggestions)
    (-let* ((elem (list state suggt)))
      (add-to-list 'starhugger--suggestion-list elem t)))
  (-let* ((leng1 (length starhugger--suggestion-list)))
    (when (< starhugger-suggestion-list-size leng1)
      (setq starhugger--suggestion-list
            (nthcdr
             (- leng1 starhugger-suggestion-list-size)
             starhugger--suggestion-list)))))

(defun starhugger--ensure-suggestion-list-syntax-highlighed (&optional force)
  "Ensure that fetched suggestions are syntax highlighted in current `major-mode'.
Normally only apply for unhighlighted suggestions, but FORCE
will (re-)apply for all."
  (-let* ((mjmode major-mode)
          (suggt-list starhugger--suggestion-list)
          (ptmin (point-min))
          ;; to minimize parsing, only try to take current top-level if possible
          ((fnbeg . fnend) (bounds-of-thing-at-point 'defun))
          (beg (or fnbeg (point-min)))
          (end (or fnend (point-max))))
    (if (or force
            ;; some suggestions are not fontified?
            (-some
             (-lambda ((_state str)) (null (object-intervals str)))
             starhugger--suggestion-list))
        (-let* ((bufstr (buffer-substring beg end))
                (fontified-lst
                 (with-temp-buffer
                   (delay-mode-hooks
                     (funcall mjmode))
                   (-map
                    (-lambda ((state suggt))
                      (-->
                       (cond
                        ((and (not force) (object-intervals suggt))
                         suggt)
                        (t
                         (erase-buffer)
                         (insert bufstr)
                         (-let* (([pt] state)
                                 ;; adjust relative to top-level
                                 (pt* (+ pt (- beg) ptmin)))
                           (goto-char pt*)
                           (insert suggt)
                           (font-lock-ensure)
                           (buffer-substring pt* (+ pt* (length suggt))))))
                       (list state it)))
                    suggt-list))))
          (setq starhugger--suggestion-list fontified-lst))
      suggt-list)))

(defun starhugger--init-overlay (suggt &optional pt)
  (-let* ((pt (or pt (point))))
    (when (and starhugger--overlay (overlay-buffer starhugger--overlay))
      (delete-overlay starhugger--overlay))
    (setq starhugger--overlay
          (make-overlay pt (+ pt 1)
                        nil
                        ;; allow inserting before the overlay
                        t t))
    (starhugger--show-overlay suggt pt)))

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key force-new num spin)
  "Show AI-powered code suggestions as overlays.
NUM: number of suggestions to fetch at once (actually
sequentially, the newly fetched ones are appended silently).
FORCE-NEW: try to fetch different responses. SPIN: show the
spinner while fetching."
  (interactive (list :force-new starhugger-active-suggestion-mode :spin t))
  (-let* ((buf (current-buffer))
          (pt0 (point))
          (state (starhugger--suggestion-state))
          (prompt (buffer-substring (point-min) (point))))
    (starhugger-active-suggestion-mode)
    (letrec ((func
              (lambda (fetch-time)
                (starhugger--query-internal
                 prompt
                 (lambda (suggestions)
                   (-let* ((suggt-1st (cl-first suggestions)))
                     (with-current-buffer buf
                       (starhugger--add-to-suggestion-list suggestions state)
                       (when (= 0 fetch-time)
                         (starhugger--init-overlay suggt-1st pt0))
                       (when (< fetch-time
                                (or
                                 num
                                 starhugger-high-number-of-suggestions-to-fetch))
                         (funcall func (+ fetch-time 1))))))
                 :spin spin
                 :force-new (or force-new (< 0 fetch-time))))))
      (funcall func 0))))

(defun starhugger-dismiss-suggestion ()
  "Clear current suggestion and stop running requests."
  (interactive)
  (dlet ((kill-buffer-query-functions '()))
    (dolist (request-buf starhugger--current-request-buffer-list)
      (delete-process (get-buffer-process request-buf))
      (kill-buffer request-buf)))
  (starhugger-active-suggestion-mode 0))

(defcustom starhugger-trigger-suggestion-after-accepting t
  "Whether to continue triggering suggestion after accepting."
  :group 'starhugger)

(defun starhugger--accept-suggestion-partially (by &optional args)
  "Insert a part of active suggestion by the function BY.
Accept the part that is before the point after applying BY on
ARGS. Note that BY should be `major-mode' dependant."
  (-when-let* ((pos (overlay-start starhugger--overlay)))
    (goto-char pos)
    (-let* ((suggt (starhugger--current-overlay-suggestion))
            (suggt* (string-remove-suffix starhugger-end-token suggt))
            (text-to-insert
             (with-temp-buffer
               (insert suggt*)
               (goto-char (point-min))
               (apply by args)
               (buffer-substring (point-min) (point)))))
      (insert text-to-insert)
      (if (equal suggt* text-to-insert)
          (progn
            (starhugger-dismiss-suggestion)
            (and starhugger-trigger-suggestion-after-accepting
                 (starhugger-trigger-suggestion :spin t)))
        (starhugger--show-overlay (string-remove-prefix text-to-insert suggt)))
      ;; maybe put parentheses balancer here?
      (run-hooks 'starhugger-post-insert-hook)
      text-to-insert)))


(defun starhugger-accept-suggestion ()
  "Insert the whole suggestion."
  (interactive)
  (starhugger--accept-suggestion-partially (lambda () (goto-char (point-max)))))

(defun starhugger-accept-suggestion-by-character (n)
  "Insert N characters from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-char (list n)))
(defun starhugger-accept-suggestion-by-word (n)
  "Insert N words from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-word (list n)))
(defun starhugger-accept-suggestion-by-line (n)
  "Insert N lines from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-line (list n)))
(defun starhugger-accept-suggestion-by-paragraph (n)
  "Insert N lines from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-paragraph (list n)))


(defun starhugger-undo-accept-suggestion-partially ()
  "Undo all partial acceptances and go back."
  (interactive)
  (-let* ((orig-point
           (overlay-get starhugger--overlay 'starhugger-original-position))
          (str (buffer-substring orig-point (point))))
    (delete-char (- (length str)))
    (starhugger--show-overlay
     (concat str (starhugger--current-overlay-suggestion)))))


(defun starhugger--get-prev-suggestion-index (delta suggestions)
  (-let* ((leng (length suggestions))
          (cur-idx
           (-elem-index (starhugger--current-overlay-suggestion) suggestions)))
    (-->
     (- cur-idx delta)
     ;; disable wrapping
     (min it (- leng 1)) (max it 0))))

(defun starhugger-show-prev-suggestion (delta)
  "Show the previous DELTA away suggestion."
  (interactive "p")
  (-let* ((suggestions (starhugger--relevant-fetched-suggestions))
          (prev-idx (starhugger--get-prev-suggestion-index delta suggestions))
          (suggt (elt suggestions prev-idx)))
    (starhugger--show-overlay suggt)))

(defun starhugger-show-next-suggestion (delta)
  "Show the next DELTA away suggestion."
  (interactive "p")
  (starhugger-show-prev-suggestion (- delta)))

;;;###autoload
(defun starhugger-show-fetched-suggestions (&optional all)
  "Display fetched suggestions at point, or ALL positions.
Note that the number of suggestions are limited by
`starhugger-suggestion-list-size'."
  (interactive "P")
  (starhugger--ensure-suggestion-list-syntax-highlighed all)
  (-let* ((cur-pt (point))
          (bufname (format "*%s %s*" 'starhugger-suggestions (buffer-name)))
          (suggestions*
           (-keep
            (-lambda (([pt] suggt))
              (and (or all (= cur-pt pt))
                   (save-excursion
                     (goto-char pt)
                     (concat (buffer-substring (pos-bol) pt) suggt))))
            starhugger--suggestion-list)))
    (pop-to-buffer bufname)
    (read-only-mode 0)
    (erase-buffer)
    (insert (string-join suggestions* "\n\n\n\n"))
    (read-only-mode 1)))

;;;; Auto-mode

(defcustom starhugger-auto-idle-time 0.5
  "Seconds to wait after typing, before fetching."
  :group 'starhugger)

(defvar-local starhugger--auto-timer nil)
(defvar-local starhugger--last-buffer-size 0)

;;;###autoload
(defun starhugger--auto-trigger ()
  (when (timerp starhugger--auto-timer)
    (cancel-timer starhugger--auto-timer))
  (run-with-idle-timer
   starhugger-auto-idle-time
   nil
   #'starhugger-trigger-suggestion
   :num starhugger-low-number-of-suggestions-to-fetch))

;;;###autoload
(defun starhugger--pre-command-h ()
  (setq starhugger--last-buffer-size (buffer-size)))

;;;###autoload
(defun starhugger--post-command-h ()
  "Clear suggestion when deleting."
  (when (< (buffer-size) starhugger--last-buffer-size)
    (starhugger-dismiss-suggestion)))

;;;###autoload
(progn
  (define-minor-mode starhugger-auto-mode
    "Automatic `starhugger-trigger-suggestion'."
    :lighter " 💫"
    :global nil
    (if starhugger-auto-mode
        (progn
          (add-hook 'pre-command-hook 'starhugger--pre-command-h nil t)
          (add-hook 'post-command-hook 'starhugger--post-command-h nil t)
          (add-hook 'post-self-insert-hook #'starhugger--auto-trigger nil t))
      (progn
        (remove-hook 'pre-command-hook 'starhugger--pre-command-h t)
        (remove-hook 'post-command-hook 'starhugger--post-command-h t)
        (remove-hook 'post-self-insert-hook #'starhugger--auto-trigger t)))))

;;; starhugger.el ends here

(provide 'starhugger)
