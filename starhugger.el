;; -*- lexical-binding: t; -*-

;; Version: 0.1.18
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
Doesn't count fills tokens."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--log (&rest args)
  (unless (get-buffer starhugger-log-buffer)
    (with-current-buffer (get-buffer-create starhugger-log-buffer)
      (read-only-mode)))
  (with-current-buffer starhugger-log-buffer
    (-let* ((pt0 (and (not (eobp)) (point))))
      (dlet ((inhibit-read-only t))
        (goto-char (point-max))
        (when starhugger-log-time
          (insert (format-time-string starhugger-log-time)))
        (dolist (obj (-interpose " " args))
          (insert (format "%s" obj)))
        (insert "\n")
        (when pt0
          (goto-char pt0))))))

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

(defcustom starhugger-hexify-request-data nil
  "Whether to hexify string before making request and unhex when receiving.
Warning: this will increase the token count by a significant magnitude!"
  :group 'starhugger
  :type 'sexp)

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (-->
   object (apply #'json-serialize it args)
   (if starhugger-hexify-request-data
       it
     (encode-coding-string it 'utf-8))))

(defvar starhugger--last-request nil)
(defvar starhugger-debug nil)

;;;###autoload
(defun starhugger-toggle-debug ()
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
                `((inputs .
                   ,(if starhugger-hexify-request-data
                        (url-hexify-string prompt)
                      prompt))
                  (parameters ,@+parameters) (options ,@+options))))))
    (dlet ((url-request-method (or method "POST"))
           (url-request-data data)
           (url-request-extra-headers
            (or headers
                `(("Content-Type" . "application/json")
                  ,@(and (< 0 (length starhugger-api-token))
                         `(("Authorization" .
                            ,(format "Bearer %s" starhugger-api-token))))))))
      (when starhugger-debug
        (dlet ((starhugger-log-buffer " *starhugger sent request data"))
          (starhugger--log data)))
      (url-retrieve
       starhugger-model-api-endpoint-url
       (lambda (status)
         (-let* ((content
                  (and url-http-end-of-headers
                       (-->
                        (buffer-substring url-http-end-of-headers (point-max))
                        (if starhugger-hexify-request-data
                            (decode-coding-string (url-unhex-string it) 'utf-8)
                          it)))))
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
  "A list 3 token to use for `starhugger-fill-in-the-middle'.
See
https://github.com/huggingface/huggingface-vscode/blob/73818334f4939c2f19480a404f74944a47933a12/src/runCompletion.ts#L66"
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-fill-in-the-middle t
  "Enable using code from both before and after point as prompt.
Unless just before the buffer end's trailing newlines (if any),
in that case don't use fill mode. See `starhugger-fill-tokens'
for the relevant tokens."
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
don't set temperature at all. Set this to a list of numbers when
the model doesn't honor use_cache = false.

To test if the model honors use_cache = false, run this twice in the shell:

curl 'https://api-inference.huggingface.co/models/bigcode/starcoder' \\
    -X POST \\
    -H 'Content-Type: application/json' \\
    -d '{\"options\": {\"use_cache\": false},\"inputs\": \"ping!\"}'

It should return 2 different responses."
  :group 'starhugger
  :type 'sexp)

(defvar-local starhugger-query--last-prompt nil)

(defun starhugger--data-for-different-response ()
  (if (and (listp starhugger-retry-temperature-range)
           (not (seq-empty-p starhugger-retry-temperature-range)))
      `((parameters
         (temperature .
                      ,(-let* (((lo hi) starhugger-retry-temperature-range))
                         (--> (cl-random 1.0) (* it (- hi lo)) (+ lo it))))))
    `((options (use_cache . :false))
      (parameters
       ,@(and starhugger-retry-temperature-range
              `((temperature . ,starhugger-retry-temperature-range)))))))

(cl-defun starhugger--query-internal (prompt callback &rest args &key display spin force-new num &allow-other-keys)
  "CALLBACK is called with the generated text list.
PROMPT is the prompt to use. DISPLAY is whether to display the
generated text in a buffer. SPIN is whether to show a spinner.
FORCE-NEW is whether to force a new request. NUM is the number of
responses to return. ARGS are the arguments to pass to
`starhugger--request'. See `starhugger--request' for the other
arguments. See `starhugger-additional-data-alist' for additional
data to pass."
  (-let* ((call-buf (current-buffer))
          (spin-obj
           (and spin starhugger-enable-spinner (starhugger--spinner-start)))
          ((&alist 'options +options 'parameters +parameters)
           (and force-new (starhugger--data-for-different-response)))
          ((&alist 'parameters dynm-parameters 'options dynm-options)
           starhugger-additional-data-alist)
          (prompt* prompt))
    (letrec ((request-buf
              (starhugger--request
               prompt*
               (lambda (returned)
                 (when (buffer-live-p call-buf)
                   (with-current-buffer call-buf
                     (setq starhugger--current-request-buffer-list
                           (delete
                            request-buf
                            starhugger--current-request-buffer-list))))
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
                              ,@(and num `((num_return_sequences . ,num)))
                              ,@+parameters
                              ,@dynm-parameters))))
      (push request-buf starhugger--current-request-buffer-list)
      (-let* ((proc (get-buffer-process request-buf)))
        (set-process-query-on-exit-flag proc nil)
        (when starhugger-debug
          (set-process-plist
           proc `(:prompt ,prompt :args ,args ,@(process-plist proc))))
        request-buf))))

;;;; Overlay inline suggestion

(defvar-local starhugger--overlay nil)

(defface starhugger-inline-suggestion-face '((t :foreground "gray" :italic t))
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
  "Whether to clear the overlay when text changes and not partially accepted."
  :group 'starhugger
  :type 'sexp)

(defun starhugger-inlining--after-change-h (&optional _beg _end _old-len)
  (when (and this-command
             ;; (not (starhugger--suggestion-accepted-partially))
             starhugger-dismiss-suggestion-after-change)
    (starhugger-dismiss-suggestion)))

(define-minor-mode starhugger-inlining-mode
  "Not meant to be called normally.
When this minor mode is off, the overlay must not be shown."
  :global nil
  :lighter " 🌠"
  :keymap `( ;
            (,(kbd "<remap> <keyboard-quit>") . starhugger-dismiss-suggestion)
            ;;
            )
  (if starhugger-inlining-mode
      (progn
        (add-hook 'after-change-functions 'starhugger-inlining--after-change-h nil t))
    (progn
      (remove-hook 'after-change-functions 'starhugger-inlining--after-change-h t)
      (when (overlayp starhugger--overlay)
        (delete-overlay starhugger--overlay)))))

(defun starhugger--ensure-inlininng-mode ()
  (unless starhugger-inlining-mode
    (starhugger-inlining-mode)))

(defcustom starhugger-number-of-suggestions-to-fetch-interactively 3
  "Positive natural number of suggestions to fetch interactively.
Allow quickly previewing to a different one. Use for commands
such as `starhugger-trigger-suggestion'.

Note that the model may return the same response repeatedly."
  :group 'starhugger
  :type 'sexp)

(defcustom starhugger-number-of-suggestions-to-fetch-automatically 2
  "Positive natural number of suggestions to fetch when automatically."
  :group 'starhugger
  :type 'sexp)

(defun starhugger--current-overlay-suggestion (&optional chop-stop-token)
  (-->
   (overlay-get starhugger--overlay 'starhugger-ovlp-current-suggestion)
   (if chop-stop-token
       (string-remove-suffix starhugger-stop-token it)
     it)))

(defvar starhugger-at-suggestion-map (make-sparse-keymap)
  "Use `starhugger-inline-menu-item' instead!
This doesn't work at the end of buffer.

Keymap used when at the beginning of suggestion overlay.")
(make-obsolete-variable 'starhugger-at-suggestion-map nil "0.1.18")

(defun starhugger--update-overlay (suggt &optional orig-pt)
  "Update overlay to displayer SUGGT after ORIG-PT.
ORIG-PT defaults to current point, when supplying it with a
non-nil (numeric) value, mark SUGGT and ORIG-PT as the original
ones."
  (-let* ((beg-pt (or orig-pt (point)))
          (suggt*
           (-->
            (propertize suggt
                        'face 'starhugger-inline-suggestion-face
                        ;; allow placing the cursor before the overlay when
                        ;; 'after-string
                        'cursor t)
            ;; WORKAROUND: when the suggestions begins with a newline, the point
            ;; will be placed at the start of the first non-newline character,
            ;; therefore won't stay at the current position visually, workaround
            ;; this by prefixing with a space
            (if (and (< 0 (length suggt)) (= ?\n (aref suggt 0)))
                (concat " " it)
              it))))
    (overlay-put starhugger--overlay 'starhugger-ovlp-current-suggestion suggt)
    (when orig-pt
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-suggestion suggt)
      (overlay-put
       starhugger--overlay 'starhugger-ovlp-original-position orig-pt))
    ;; at end of buffer, 'display doesn't show anything because
    ;; `overlay-starr'=`overlay-end'
    (if (<= (point-max) beg-pt)
        (progn
          (overlay-put starhugger--overlay 'display nil)
          (overlay-put starhugger--overlay 'after-string suggt*))
      ;; currently I can't find a way to to achieve this:

      ;; 〈before〉|〈overlay〉〈after〉

      ;; so the workaround is too concatenate "overlay" and "after" and and put
      ;; the overlay on "after"
      (progn
        (overlay-put starhugger--overlay 'after-string nil)
        (overlay-put
         starhugger--overlay
         'display
         (concat suggt* (buffer-substring beg-pt (+ beg-pt 1))))))))

(defun starhugger--init-overlay (suggt pt)
  "Initialize over to show SUGGT and mark PT as the original position."
  (when (and starhugger--overlay (overlay-buffer starhugger--overlay))
    (delete-overlay starhugger--overlay))
  (when (<= pt (point-max))
    (setq starhugger--overlay
          (make-overlay pt (+ pt 1)
                        nil
                        ;; allow inserting before the overlay
                        t t))
    ;; (overlay-put starhugger--overlay 'keymap starhugger-at-suggestion-map)
    (starhugger--update-overlay suggt pt)))

(defun starhugger-at-suggestion-beg-p (&optional cmd)
  "Return CMD (or true) when point is at suggestion start.
See `starhugger-inline-menu-item'."
  (and starhugger-inlining-mode
       (equal (overlay-start starhugger--overlay) (point))
       (or cmd t)))

(defun starhugger-inline-menu-item (cmd)
  "Return a CMD when only at the start of suggestion at run-time.
Use this when binding keys. See info node `(elisp) Extended Menu
Items'."
  `(menu-item "" ,cmd nil :filter starhugger-at-suggestion-beg-p))

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
    (starhugger--ensure-inlininng-mode)
    (when recent-suggt
      (when starhugger-debug
        (starhugger--log #'starhugger--try-show-most-recent-suggestion
                         "at" pt ":" recent-suggt))
      (starhugger--init-overlay recent-suggt pt))
    recent-suggt))

(defcustom starhugger-trim-spaces-around-prompt t
  "Whether to trim spaces in the prompt.
Trim space before the prompt, and in fill mode, spaces after the
prompt."
  :group 'starhugger :type 'boolean)

(defun starhugger--no-fill-prompt ()
  (-let* ((pt-cur (point)))
    (-->
     (buffer-substring-no-properties
      (max (- pt-cur starhugger-max-prompt-length) (point-min)) pt-cur)
     (if starhugger-trim-spaces-around-prompt
         (string-trim-left it)
       it))))

(defun starhugger--prompt ()
  "Build the prompt to send to the model."
  (if (and starhugger-fill-in-the-middle
           ;; don't use fill mode when at trailing newlines
           (not (looking-at-p "\n*\\'")))
      (-let* (((pre-token mid-token suf-token) starhugger-fill-tokens)
              (intend-suf-len
               (floor
                (* starhugger-max-prompt-length
                   starhugger-prompt-after-point-fraction)))
              (intend-pre-len (- starhugger-max-prompt-length intend-suf-len))
              (avail-pre (- (point) (point-min)))
              (avail-suf (- (point-max) (point)))
              ([pre-beg-pos suf-end-pos]
               (cond
                ((and (> avail-pre intend-pre-len) (< avail-suf intend-suf-len))
                 (vector
                  (- (point) (- starhugger-max-prompt-length avail-suf))
                  (point-max)))
                ((and (< avail-pre intend-pre-len) (> avail-suf intend-suf-len))
                 (vector
                  (point-min)
                  (+ (point) (- starhugger-max-prompt-length avail-pre))))
                ((and (< avail-pre intend-pre-len) (< avail-suf intend-suf-len))
                 (vector (point-min) (point-max)))
                (t
                 (vector
                  (- (point) intend-pre-len) (+ (point) intend-suf-len)))))
              (suf-str
               (-->
                (buffer-substring-no-properties (point) suf-end-pos)
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-left it)
                  it)))
              (pre-str
               (-->
                (buffer-substring-no-properties pre-beg-pos (point))
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-right it)
                  it))))
        (concat pre-token pre-str mid-token suf-str suf-token))
    (starhugger--no-fill-prompt)))

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key interact force-new num)
  "Show AI-powered code suggestions as overlays.
NUM: number of suggestions to fetch at once (actually
sequentially, the newly fetched ones are appended silently).
FORCE-NEW: try to fetch different responses. Non-nil INTERACT:
show spinner."
  (interactive (list :interact t :force-new starhugger-inlining-mode))
  (-let* ((num (or num starhugger-number-of-suggestions-to-fetch-interactively))
          (call-buf (current-buffer))
          (pt0 (point))
          (state (starhugger--suggestion-state))
          ;; (modftick (buffer-modified-tick))
          (prompt (starhugger--prompt)))
    (starhugger--ensure-inlininng-mode)
    (letrec ((func
              (lambda (fetch-time)
                (starhugger--query-internal
                 prompt
                 (lambda (suggestions)
                   (when (buffer-live-p call-buf)
                     (with-current-buffer call-buf
                       (-let* ((suggt-1st (-first-item suggestions)))
                         (starhugger--add-to-suggestion-list suggestions state)
                         ;; only display when didn't move or interacive (in that
                         ;; case we are explicitly waiting)
                         (when (or interact (= pt0 (point)))
                           ;; ;; TODO: why is the buffer modified here?
                           ;; (equal modftick (buffer-modified-tick))
                           (when (= 1 fetch-time)
                             (starhugger--ensure-inlininng-mode)
                             (starhugger--init-overlay suggt-1st pt0))
                           (when (and (< fetch-time num)
                                      (< (length suggestions) num))
                             (funcall func (+ fetch-time 1))))))))
                 :spin (or starhugger-debug interact)
                 :force-new (or force-new (< 1 fetch-time))
                 :num num))))
      (funcall func 1))))

(defun starhugger--triggger-suggestion-prefer-cache
    (in-buffer position &optional cache-only)
  (when (and (equal in-buffer (current-buffer)) (equal position (point)))
    (or (starhugger--try-show-most-recent-suggestion)
        (when (not cache-only)
          (starhugger-trigger-suggestion
           :num starhugger-number-of-suggestions-to-fetch-automatically)))))

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
  (starhugger-inlining-mode 0))

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
      (overlay-put starhugger--overlay 'starhugger-ovlp-partially-accepted t)
      ;; insert after marking as partially-accepted
      (insert text-to-insert)
      (if (equal suggt* text-to-insert)
          (progn
            (starhugger-dismiss-suggestion)
            (and starhugger-trigger-suggestion-after-accepting
                 (starhugger-trigger-suggestion :interact t)))
        (progn
          (starhugger--update-overlay
           (string-remove-prefix text-to-insert suggt))))
      ;; maybe put parentheses balancer here?
      (run-hooks 'starhugger-post-insert-hook)
      text-to-insert)))

(defun starhugger--suggestion-accepted-partially ()
  (and starhugger-inlining-mode
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
(defun starhugger-auto--after-change-h (&optional _beg _end old-len)
  (when this-command
    (when (timerp starhugger--auto-timer)
      (cancel-timer starhugger--auto-timer))
    (setq starhugger--auto-timer
          (run-with-idle-timer
           starhugger-auto-idle-time
           nil
           #'starhugger--triggger-suggestion-prefer-cache
           (current-buffer)
           (point)
           (< 0 old-len) ; don't fetch new when deleting
           ))))

;;;###autoload
(defun starhugger-auto--post-command-h ()
  (when (and starhugger--overlay
             starhugger-inlining-mode
             starhugger-auto-dismiss-when-move-out)
    (-let* ((beg
             (overlay-get
              starhugger--overlay 'starhugger-ovlp-original-position))
            (end (overlay-end starhugger--overlay)))
      (unless (and (numberp beg) (numberp end) (<= beg (point) end))
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
