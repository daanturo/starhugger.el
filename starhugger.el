;; -*- lexical-binding: t; -*-

;; Version: 0.1.14
;; Package-Requires: ((emacs "28.2") (compat "29.1.4.0") (dash "2.18.0") (spinner "1.7.4"))

;;; Commentary:

;; Client for AI-powered code completion(s).

;;; Code:

(require 'cl-macs)
(require 'subr-x)

(require 'dash)
(require 'compat)

;;;; Making requests

(defcustom starhugger-api-token nil
  "Hugging Face user access tokens.
Generate yours at https://huggingface.co/settings/tokens."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-model-api-endpoint-url
  "https://api-inference.huggingface.co/models/bigcode/starcoder"
  "End point URL to make HTTP request."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--get-all-generated-texts (str)
  (-let* ((parsed (json-parse-string str :object-type 'alist))
          ((_ . err-msg) (and (listp parsed) (assoc 'error parsed))))
    (cond
     (err-msg
      (user-error "`starhugger' response error: %s" err-msg))
     (t
      (-map (lambda (elem) (alist-get 'generated_text elem)) parsed)))))

(defcustom starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log parsed responses."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-log-buffer (format " *%s-log*" 'starhugger)
  "Buffer name to log things, hidden by default."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-log-time "(%F %T) "
  "Whether to insert this time format before each entry in the log buffer."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-max-prompt-length (* 1024 8)
  "Max length of the prompt to send.
\"`inputs` tokens + `max_new_tokens` must be <= 8192\"."
  :group 'starhugger
  :type 'sexp)

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
    (push '(starhugger . ["⭐" "🌟"]) spinner-types)
    (setq starhugger--spinner-added-type t))
  (spinner-start 'starhugger 3))

(defcustom starhugger-enable-spinner t
  "Show spinner when fetching."
  :group 'starhugger
  :type 'sexp)

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
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-max-new-tokens nil
  "When a number, set it to max_new_tokens.
See also `starhugger-additional-data-alist'."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (encode-coding-string (apply #'json-serialize object args) 'utf-8))

(defvar starhugger--last-request nil)
(defvar starhugger-debug nil)

;;;###autoload
(defun starhugger-toggle-debugging ()
  "More verbose logging (and maybe indicators)."
  (interactive)
  (setq starhugger-debug (not starhugger-debug))
  (message "`starhugger-debug' := %s" starhugger-debug))

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
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-stop-token "<|endoftext|>"
  "End of sentence token."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-chop-stop-token t
  "Whether to remove `starhugger-stop-token' before inserting."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-fill-tokens
  '("<fim_prefix>" "<fim_suffix>" "<fim_middle>")
  "A list 3 token to use for `starhugger-fill-in-the-middle'."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-fill-in-the-middle t
  "Enable using code from both before and after point as prompt.
https://github.com/huggingface/huggingface-vscode/blob/73818334f4939c2f19480a404f74944a47933a12/src/runCompletion.ts#L66"
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-prompt-after-point-fraction (/ 1.0 4)
  "The length fraction that code after point should take in the prompt."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--post-process-content
    (generated-text &optional notify-end prompt)
  (-->
   generated-text
   (if starhugger-strip-prompt-before-insert
       (string-remove-prefix prompt it)
     it)
   (if starhugger-chop-stop-token
       (-let* ((end-flag (string-suffix-p starhugger-stop-token it)))
         (when (and end-flag notify-end)
           (message "%s received from %s !"
                    starhugger-stop-token
                    starhugger-model-api-endpoint-url))
         (if end-flag
             (string-remove-suffix starhugger-stop-token it)
           it))
     it)))

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
  :group 'starhugger
  :type 'sexp)

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

(cl-defun starhugger--query-internal (prompt callback &rest args &key display spin force-new &allow-other-keys)
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
      (-let* ((proc (get-buffer-process request-buf)))
        (set-process-query-on-exit-flag proc nil)
        (when starhugger-debug
          (set-process-plist
           proc `(:prompt ,prompt :args ,args ,@(process-plist proc))))
        request-buf))))

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
(defvar-local starhugger--suggestion-list '()
  "List of recently fetched suggestions along with internal state.
Recent suggestions are added to the beginning.")

(defcustom starhugger-suggestion-list-size 32
  "Maximum number of saved suggestions in current buffer.
Note that this includes all recently fetched suggestions so not
all of them are relevant all the time."
  :group 'starhugger
  :type 'sexp)


(defcustom starhugger-dismiss-suggestion-after-change t
  "Whether to clear the overlay when text changes.
And when no partially accepted suggestions."
  :group 'starhugger
  :type 'sexp)

(defun starhugger-active-suggestion--after-change-h
    (&optional _beg _end _old-len)
  (when (and this-command
             starhugger-dismiss-suggestion-after-change
             (not (starhugger--suggestion-accepted-partially)))
    (starhugger-dismiss-suggestion)))

(define-minor-mode starhugger-active-suggestion-mode
  "Not meant to be called normally.
When this minor mode is off, the overlay must not be shown."
  :global nil
  :lighter " 🌠"
  :keymap `( ;
            (,(kbd "<remap> <keyboard-quit>") . starhugger-dismiss-suggestion)
            ;;
            )
  (if starhugger-active-suggestion-mode
      (progn
        (add-hook 'after-change-functions 'starhugger-active-suggestion--after-change-h nil t)
        )
    (progn
      (remove-hook 'after-change-functions 'starhugger-active-suggestion--after-change-h t)
      (when (overlayp starhugger--overlay)
        (delete-overlay starhugger--overlay)))))

(defun starhugger--ensure-active-suggestion-mode ()
  (unless starhugger-active-suggestion-mode
    (starhugger-active-suggestion-mode)))

(defcustom starhugger-high-number-of-suggestions-to-fetch 3
  "The number of suggestions to fetch (sequentially) interactively.
Allow quickly previewing to a different one. Use for commands
such as `starhugger-trigger-suggestion'.

Note that the model may return the same response repeatedly."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-low-number-of-suggestions-to-fetch 2
  "The number of suggestions to fetch (sequentially) when automatically."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--current-overlay-suggestion (&optional chop-stop-token)
  (-->
   (overlay-get starhugger--overlay 'starhugger-ovlp-current-suggestion)
   (if chop-stop-token
       (string-remove-suffix starhugger-stop-token it)
     it)))

(defvar starhugger-suggestion-beg-map (make-sparse-keymap)
  "Keymap used when at the beginning of suggestion overlay.")

(defun starhugger--update-overlay (suggt &optional orig-pt)
  "Update overlay to displayer SUGGT after ORIG-PT.
ORIG-PT defaults to current point, when supplying it with a
non-nil (numeric) value, mark SUGGT and ORIG-PT as the original
ones."
  (-let* ((beg-pt (or orig-pt (point)))
          (suggt*
           (propertize suggt
                       'face 'starhugger-suggestion-face
                       ;; allow placing the cursor before the overlay when
                       ;; 'before-string
                       'cursor t)))
    (overlay-put starhugger--overlay 'starhugger-ovlp-current-suggestion suggt)
    (when orig-pt
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-suggestion suggt)
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-position orig-pt))
    ;; at end of buffer, 'display doesn't show anything because
    ;; `overlay-starr'=`overlay-end'
    (if (eobp)
        (progn
          (overlay-put starhugger--overlay 'display nil)
          (overlay-put starhugger--overlay 'before-string suggt*))
      ;; currently I can't find a way to to achieve this:

      ;; 〈before〉|〈overlay〉〈after〉

      ;; so the workaround is too concatenate "overlay" and "after" and and put
      ;; the overlay on "after"
      (progn
        (overlay-put starhugger--overlay 'before-string nil)
        (overlay-put
         starhugger--overlay
         'display
         (concat suggt* (buffer-substring beg-pt (+ beg-pt 1))))))))

(defun starhugger--init-overlay (suggt pt)
  "Initialize over to show SUGGT and mark PT as the original position."
  (when (and starhugger--overlay (overlay-buffer starhugger--overlay))
    (delete-overlay starhugger--overlay))
  (setq starhugger--overlay
        (make-overlay pt (+ pt 1)
                      nil
                      ;; allow inserting before the overlay
                      t t))
  (overlay-put starhugger--overlay 'keymap starhugger-suggestion-beg-map)
  (starhugger--update-overlay suggt pt))

(defun starhugger--suggestion-state (&optional pt)
  (if pt
      (save-excursion
        (goto-char pt)
        (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (vector (point) (buffer-substring-no-properties (pos-bol) (pos-eol)))))

(defun starhugger--relevant-fetched-suggestions (&optional all pt)
  (-let* ((cur-state (starhugger--suggestion-state pt)))
    (-keep
     (-lambda ((state suggt)) (and (or all (equal cur-state state)) suggt))
     starhugger--suggestion-list)))

(defun starhugger--add-to-suggestion-list (suggestions state)
  (dolist (suggt suggestions)
    (when suggt
      (-let* ((elem (list state suggt)))
        (add-to-list 'starhugger--suggestion-list elem))))
  (-let* ((leng1 (length starhugger--suggestion-list)))
    (when (< starhugger-suggestion-list-size leng1)
      (setq starhugger--suggestion-list
            (ntake
             starhugger-suggestion-list-size starhugger--suggestion-list)))))

(defun starhugger--ensure-suggestion-list-syntax-highlighed (&optional force)
  "Ensure that fetched suggestions are syntax highlighted in current `major-mode'.
Normally only apply for unhighlighted suggestions, but FORCE
will (re-)apply for all."
  (-let* ((mjmode major-mode)
          (suggt-list starhugger--suggestion-list))
    (if (or force
            ;; some suggestions are not fontified?
            (-some
             (-lambda ((_state str)) (null (object-intervals str))) suggt-list))
        (-let* ((strings-around-alist
                 (save-excursion
                   (-->
                    suggt-list (-map (-lambda (([pt])) pt) it) (-uniq it)
                    (-map
                     (lambda (pt)
                       (goto-char pt)
                       (-let* (((toplv-beg . toplv-end)
                                (bounds-of-thing-at-point 'defun)))
                         (list
                          pt
                          (buffer-substring (or toplv-beg (point-min)) pt)
                          (buffer-substring pt (or toplv-end (point-max))))))
                     it))))
                (fontified-lst
                 (with-temp-buffer
                   (delay-mode-hooks
                     (funcall mjmode))
                   (-map
                    (-lambda ((state suggt))
                      (-let* ((fontified-str
                               (cond
                                ((and (not force) (object-intervals suggt))
                                 suggt)
                                (t
                                 (-let* (([pt _] state)
                                         ((pre suf)
                                          (alist-get pt strings-around-alist)))
                                   (erase-buffer)
                                   (insert pre)
                                   (-let* ((pt* (point)))
                                     (insert suggt)
                                     (insert suf)
                                     (font-lock-ensure)
                                     (buffer-substring
                                      pt* (+ pt* (length suggt)))))))))
                        (list state fontified-str)))
                    suggt-list))))
          (setq starhugger--suggestion-list fontified-lst))
      suggt-list)))

(defun starhugger--try-show-most-recent-suggestion ()
  (-let* ((pt (point))
          (cur-state (starhugger--suggestion-state))
          (recent-suggt
           (-some
            (-lambda ((state suggt)) (and (equal cur-state state) suggt))
            starhugger--suggestion-list)))
    (starhugger--ensure-active-suggestion-mode)
    (when recent-suggt
      (when starhugger-debug
        (starhugger--log
         #'starhugger--try-show-most-recent-suggestion
         "at"
         pt
         ":"
         recent-suggt))
      (starhugger--init-overlay recent-suggt pt))
    recent-suggt))

;; TODO: implement this
(defun starhugger--prompt ())

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key interact force-new num)
  "Show AI-powered code suggestions as overlays.
NUM: number of suggestions to fetch at once (actually
sequentially, the newly fetched ones are appended silently).
FORCE-NEW: try to fetch different responses. Non-nil INTERACT:
show spinner."
  (interactive (list :interact t :force-new starhugger-active-suggestion-mode))
  (-let* ((num (or num starhugger-high-number-of-suggestions-to-fetch))
          (buf (current-buffer))
          (pt0 (point))
          (state (starhugger--suggestion-state))
          (prompt (buffer-substring (point-min) (point)))
          (modftick (buffer-modified-tick)))
    (starhugger--ensure-active-suggestion-mode)
    (letrec ((func
              (lambda (fetch-time)
                (starhugger--query-internal
                 prompt
                 (lambda (suggestions)
                   (with-current-buffer buf
                     (-let* ((suggt-1st (cl-first suggestions)))
                       (starhugger--add-to-suggestion-list suggestions state)
                       (when (equal modftick (buffer-modified-tick))
                         (when (= 0 fetch-time)
                           (starhugger--init-overlay suggt-1st pt0))
                         (when (< fetch-time num)
                           (funcall func (+ fetch-time 1)))))))
                 :spin (or starhugger-debug interact)
                 :force-new (or force-new (< 0 fetch-time))))))
      (funcall func 0))))

(defun starhugger--triggger-suggestion-prefer-cache (in-buffer)
  (when (equal in-buffer (current-buffer))
    (or (starhugger--try-show-most-recent-suggestion)
        (starhugger-trigger-suggestion
         :num starhugger-low-number-of-suggestions-to-fetch))))

(defun starhugger-dismiss-suggestion (&optional stop-fetching)
  "Clear current suggestion and stop running requests.
Non-nil STOP-FETCHING (interactively true by default): also kill
unfinished fetches."
  (interactive (list (not current-prefix-arg)))
  (when stop-fetching
    (dlet ((kill-buffer-query-functions '()))
      (dolist (request-buf starhugger--current-request-buffer-list)
        (delete-process (get-buffer-process request-buf))
        (kill-buffer request-buf))))
  (starhugger-active-suggestion-mode 0))

(defcustom starhugger-trigger-suggestion-after-accepting t
  "Whether to continue triggering suggestion after accepting."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--accept-suggestion-partially (by &optional args)
  "Insert a part of active suggestion by the function BY.
Accept the part that is before the point after applying BY on
ARGS. Note that BY should be `major-mode' dependant."
  (-when-let* ((pos (overlay-start starhugger--overlay)))
    (goto-char pos)
    (-let* ((suggt (starhugger--current-overlay-suggestion))
            (suggt*
             (if starhugger-chop-stop-token
                 (string-remove-suffix starhugger-stop-token suggt)
               suggt))
            (text-to-insert
             (with-temp-buffer
               (insert suggt*)
               (goto-char (point-min))
               (apply by args)
               (buffer-substring (point-min) (point)))))
      (if (equal suggt* text-to-insert)
          (progn
            (starhugger-dismiss-suggestion)
            (and starhugger-trigger-suggestion-after-accepting
                 (starhugger-trigger-suggestion :interact t)))
        (progn
          (starhugger--update-overlay
           (string-remove-prefix text-to-insert suggt))
          (overlay-put
           starhugger--overlay 'starhugger-ovlp-partially-accepted t)))
      ;; insert after marking as partially-accepted
      (insert text-to-insert)
      ;; maybe put parentheses balancer here?
      (run-hooks 'starhugger-post-insert-hook)
      text-to-insert)))

(defun starhugger--suggestion-accepted-partially ()
  (and starhugger-active-suggestion-mode
       (overlayp starhugger--overlay)
       (overlay-get starhugger--overlay 'starhugger-ovlp-partially-accepted)))

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
           (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (str (buffer-substring orig-point (point))))
    (delete-char (- (length str)))
    (overlay-put starhugger--overlay 'starhugger-ovlp-partially-accepted nil)
    (starhugger--update-overlay
     (concat str (starhugger--current-overlay-suggestion)))))


(defun starhugger--get-prev-suggestion-index (delta suggestions)
  (-let* ((leng (length suggestions))
          (cur-idx
           (-elem-index (starhugger--current-overlay-suggestion) suggestions)))
    (-->
     ;; either `+': recent at first or `-': recent at last, depends of
     ;; `starhugger--suggestion-list''s ordering
     (+ cur-idx delta)
     ;; disable wrapping
     (min it (- leng 1)) (max it 0))))

(defun starhugger-show-prev-suggestion (delta)
  "Show the previous suggestion.
With prefix argument DELTA, show the suggestion that is DELTA away."
  (interactive "p")
  (-let* ((pt (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (suggestions (starhugger--relevant-fetched-suggestions nil pt))
          (prev-idx (starhugger--get-prev-suggestion-index delta suggestions))
          (suggt (elt suggestions prev-idx)))
    (starhugger--update-overlay suggt pt)))

(defun starhugger-show-next-suggestion (delta)
  "Show the next suggestion.
With prefix argument DELTA, show the suggestion that is DELTA away."
  (interactive "p")
  (starhugger-show-prev-suggestion (- delta)))

;;;###autoload
(defun starhugger-show-fetched-suggestions (&optional all)
  "Display fetched suggestions at point, or ALL positions.
Note that the number of suggestions are limited by
`starhugger-suggestion-list-size'."
  (interactive "P")
  (starhugger--ensure-suggestion-list-syntax-highlighed all)
  (-let* ((prompt-end-pt
           (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (bufname (format "*%s %s*" 'starhugger-suggestions (buffer-name)))
          (suggestions*
           (-keep
            (-lambda (([pt] suggt))
              (and (or all (= prompt-end-pt pt))
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
  "Seconds to wait after typing, before fetching.
Note that the time taken to fetch isn' instantaneous, so we have
to wait more after this unless the suggestion(s) is already
cached, for the suggestion to appear."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-auto-dismiss-when-move-out t
  "Whether to dismiss suggestion when moving point outside."
  :group 'starhugger
  :type 'sexp)

(defvar-local starhugger--auto-timer nil)

;;;###autoload
(defun starhugger-auto--after-change-h (&optional _beg _end _len)
  (when this-command
    (when (timerp starhugger--auto-timer)
      (cancel-timer starhugger--auto-timer))
    (when (and (not (starhugger--suggestion-accepted-partially)))
      (setq starhugger--auto-timer
            (run-with-idle-timer starhugger-auto-idle-time
                                 nil
                                 #'starhugger--triggger-suggestion-prefer-cache
                                 (current-buffer))))))

;;;###autoload
(defun starhugger-auto--post-command-h ()
  (when (and starhugger--overlay
             starhugger-active-suggestion-mode
             starhugger-auto-dismiss-when-move-out)
    (-let* ((beg
             (overlay-get
              starhugger--overlay 'starhugger-ovlp-original-position))
            (end (overlay-end starhugger--overlay)))
      (unless (<= beg (point) end)
        (starhugger-dismiss-suggestion)))))

;;;###autoload
(progn

  (define-minor-mode starhugger-auto-mode
    "Automatic `starhugger-trigger-suggestion'."
    :lighter " 💫"
    :global nil
    (if starhugger-auto-mode
        (progn
          (add-hook 'post-command-hook #'starhugger-auto--post-command-h nil t)
          (add-hook 'after-change-functions #'starhugger-auto--after-change-h nil t))
      (progn
        (remove-hook 'post-command-hook #'starhugger-auto--post-command-h t)
        (remove-hook 'after-change-functions #'starhugger-auto--after-change-h t))))

  ;;
  )

;;; starhugger.el ends here

(provide 'starhugger)
