;;; starhugger.el --- Hugging Face/AI-powered text & code completion client  -*- lexical-binding: t; -*-

;; Version: 0.6.0-git
;; Package-Requires: ((emacs "28.2") (compat "29.1.4.0") (dash "2.18.0") (s "1.13.1") (spinner "1.7.4") (request "0.3.2"))
;; Keywords: completion, convenience, languages
;; Homepage: https://gitlab.com/daanturo/starhugger.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Hugging Face/AI-powered text & code completion client (unofficial).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)

(require 'compat)
(require 'dash)
(require 's)
(require 'request)


;;;; Helpers

(cl-defmacro starhugger--with-buffer-scrolling (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
Like `with-current-buffer', but allow scrolling the visible
window of BUFFER-OR-NAME when at the buffer end, if any."
  (declare (debug t) (indent defun))
  `(-let* ((wd (get-buffer-window ,buffer-or-name t)))
     (cond
      (wd
       (with-selected-window wd
         ;; disallow scrolling when visibly not at end of buffer
         (-let* ((eob-flag (eobp))
                 (pt0 (point)))
           (unwind-protect
               (progn
                 ,@body)
             (unless eob-flag
               (goto-char pt0))))))
      (t
       (with-current-buffer ,buffer-or-name
         ,@body)))))

;;;; Making requests

(define-obsolete-variable-alias
  'starhugger-api-token 'starhugger-hugging-face-api-token "0.5.0")

(defcustom starhugger-hugging-face-api-token nil
  "Hugging Face user access token.
Generate yours at URL `https://huggingface.co/settings/tokens'.
Can be either a direct string, or a function to be called with no
arguments that returns a string.  When being a function, it has
to be fast to return.

Note: the (returned) string must be unibyte, ensure that before
dynamically setting with (`encode-coding-string' ... \\='utf-8)."
  :group 'starhugger
  :type '(choice string function))

(define-obsolete-variable-alias
  'starhugger-model-api-endpoint-url
  'starhugger-hugging-face-api-url
  "0.5.0")
(defvar starhugger-hugging-face-api-url nil
  "Hugging Face text inference API's URL to make requests (includes model name).
To be concatenated with `starhugger-hugging-face-api-base-url'
and `starhugger-model-id'.")
(defvar starhugger-fill-tokens)
(defvar starhugger-stop-tokens)

(defcustom starhugger-hugging-face-api-base-url
  "https://api-inference.huggingface.co/models/"
  "Hugging Face inference API's base URL, to be concatenated with model ID.
See https://huggingface.co/docs/api-inference/quicktour#running-inference-with-api-requests."
  :group 'starhugger
  :type 'string)

(defvar starhugger--model-config-presets
  '(
    ;; the original Starcoder
    ("\\bStarcoder\\b" .
     (:fill-tokens
      ("<fim_prefix>" "<fim_suffix>" "<fim_middle>")
      :stop-tokens ("<|endoftext|>")
      :family starcoder))
    ("\\bStarcoder[0-9]+\\b" .
     (:fill-tokens
      ("<fim_prefix>" "<fim_suffix>" "<fim_middle>")
      :stop-tokens
      ("<|endoftext|>"
       "<file_sep>" ; https://github.com/bigcode-project/starcoder2/issues/10#issuecomment-1979014959
       )
      :file-separator "<file_sep>"
      :family starcoder))
    ("\\bCode[^a-z]?Gemma\\b" .
     (:fill-tokens
      ("<|fim_prefix|>" "<|fim_suffix|>" "<|fim_middle|>")
      :stop-tokens ()
      :file-separator "<|file_separator|>"
      :family code-gemma))
    ("\\bDeepseek\\b" .
     (:fill-tokens
      ("<｜fim▁begin｜>" "<｜fim▁hole｜>" "<｜fim▁end｜>")
      :stop-tokens ("<｜eos▁token｜>")
      :family deepseek))
    ("\\bCode[^a-z]?Llama\\b" .
     (:fill-tokens
      ("<PRE>" "<SUF>" "<MID>")
      :stop-tokens ("<|endoftext|>" "<EOT>")
      :family code-llama))
    ("\\bQwen[.0-9]*-?Coder\\b" .
     ;; https://arxiv.org/abs/2409.12186
     (:fill-tokens
      ("<|fim_prefix|>" "<|fim_suffix|>" "<|fim_middle|>")
      :stop-tokens ("<|endoftext|>")
      :file-separator "<|file_sep|>"
      :family qwen-coder)))
  "Refer to https://github.com/huggingface/huggingface-vscode/blob/f044ff02f08e49a5da9849f34235fece4a32535b/src/configTemplates.ts#L17.")

(defvar starhugger-model-id)
(defun starhugger--get-model-preset (&optional model-id)
  (-let* ((model-id (or model-id starhugger-model-id)))
    (alist-get model-id starhugger--model-config-presets
               nil nil
               (lambda (alist-car _)
                 (dlet ((case-fold-search t))
                   (string-match
                    alist-car
                    ;; ignore organization name
                    (replace-regexp-in-string "^.*?/" "" model-id)))))))

(defun starhugger--model-id-set-fn (sym val)
  (set-default-toplevel-value sym val)
  (save-match-data
    (-when-let* ((preset (starhugger--get-model-preset val)))
      (setq starhugger-hugging-face-api-url
            (concat starhugger-hugging-face-api-base-url val))
      (setq starhugger-fill-tokens (map-nested-elt preset '(:fill-tokens)))
      (setq starhugger-stop-tokens (map-nested-elt preset '(:stop-tokens))))))

(defcustom starhugger-model-id "qwen2.5-coder"
  "The language model's ID.
If you want to use one of the configuration presets, set this
before loading `starhugger.el' or use `setopt' (or Emacs's
customization interface). Else if you use a custom model,
configure `starhugger-hugging-face-api-url',
`starhugger-fill-tokens', `starhugger-stop-tokens' manually (and
you don't need to customize this variable if they are set that
way)."
  :group 'starhugger
  :type 'string
  :set #'starhugger--model-id-set-fn)

(defcustom starhugger-stop-tokens nil
  "End of sentence tokens."
  :group 'starhugger
  :type 'string)

(defcustom starhugger-fill-tokens nil
  "List of 3 tokens to use for `starhugger-fill-in-the-middle'.
See
https://github.com/huggingface/huggingface-vscode/blob/73818334f4939c2f19480a404f74944a47933a12/src/runCompletion.ts#L66"
  :group 'starhugger
  :type '(list string string string))



(defcustom starhugger-generated-buffer (format "*%s*" 'starhugger)
  "Buffer name to log parsed responses."
  :group 'starhugger
  :type 'string)

(defcustom starhugger-max-prompt-length (* 1024 8)
  "Max length of the code in current buffer to send.
Doesn't count fills tokens and maybe the context."
  :group 'starhugger
  :type 'natnum)


(defvar starhugger--log-buffer (format " *%s-log*" 'starhugger)
  "Buffer name to log things, hidden by default.")

(defun starhugger--log (&rest args)
  (unless (get-buffer starhugger--log-buffer)
    (with-current-buffer (get-buffer-create starhugger--log-buffer)
      (dlet ((view-inhibit-help-message t))
        (read-only-mode))))
  (with-current-buffer starhugger--log-buffer
    (-let* ((pt0 (and (not (eobp)) (point))))
      (dlet ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "(%F %T) "))
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
  "Show spinner when fetching interactively."
  :group 'starhugger
  :type 'boolean)

;; WHY isn't this documented?!
(defvar url-http-end-of-headers)


(defcustom starhugger-max-new-tokens nil
  "When a number, set it to max_new_tokens.
It can be a list of two natural numbers: the number of tokens to
fetch when called automatically and the number of token to fetch
when called interactively. See also
`starhugger-hugging-face-additional-data-alist'."
  :group 'starhugger
  :type '(choice natnum (list natnum natnum)))

(defun starhugger--json-serialize (object &rest args)
  "Like (`json-serialize' OBJECT @ARGS).
Additionally prevent errors about multi-byte characters."
  (-->
   object (apply #'json-serialize it args) (encode-coding-string it 'utf-8)))

(defvar starhugger-debug nil)
(defvar starhugger--last-returned-request nil
  "Last returned request info, for debugging.")
(defvar starhugger--last-sent-request nil
  "Last sent request info, for debugging.")

(defvar starhugger-before-request-hook '()
  "Hook run before making an HTTP request.")

(defvar starhugger--running-request-table--table nil
  "Keys are buffers.")
(defun starhugger--running-request-table ()
  (unless starhugger--running-request-table--table
    (setq starhugger--running-request-table--table
          (make-hash-table :test #'equal)))
  starhugger--running-request-table--table)


(defun starhugger--record-propertize (str)
  (propertize str 'face '(:foreground "yellow" :weight bold)))

(defvar starhugger--record-heading-beg "#*> ")

(cl-defun starhugger--record-generated (prompt parsed-response-list &rest args &key display &allow-other-keys)
  (-let* ((buf
           (or (get-buffer starhugger-generated-buffer)
               (prog1 (get-buffer-create starhugger-generated-buffer)
                 (with-current-buffer starhugger-generated-buffer
                   (setq-local outline-regexp
                               (regexp-quote starhugger--record-heading-beg))
                   (setq-local window-point-insertion-type t)
                   (read-only-mode))))))
    (starhugger--with-buffer-scrolling
      buf
      (dlet ((inhibit-read-only t))

        (goto-char (point-max))
        (insert
         (starhugger--record-propertize
          (concat starhugger--record-heading-beg "API INPUT: ")))
        (insert (format "(info: %S)" args) "\n\n")
        (insert prompt)
        (insert "\n\n")

        (if (equal parsed-response-list '())
            (insert
             (starhugger--record-propertize
              (concat starhugger--record-heading-beg "OUTPUT from API: None!\n")))
          (-let* ((lst (ensure-list parsed-response-list)))
            (--each lst
              (insert
               (starhugger--record-propertize
                (format
                 "%sAPI OUTPUT #%d/%d:"
                 starhugger--record-heading-beg (+ 1 it-index) (length lst))))
              (insert "\n" it "\n\n"))))

        (insert "\n\n\n")))
    (when display
      (save-selected-window
        (pop-to-buffer buf)))
    buf))

(defcustom starhugger-strip-prompt-before-insert nil
  "Whether to remove the prompt in the parsed response before inserting.
Enable this when the return_full_text parameter isn't honored."
  :group 'starhugger
  :type 'boolean)

(defvar starhugger-stop-token nil
  "Obsolete, customize `starhugger-stop-tokens' instead.")

(defcustom starhugger-chop-stop-token t
  "Whether to remove `starhugger-stop-tokens' before inserting."
  :group 'starhugger
  :type 'boolean)

(defcustom starhugger-fill-in-the-middle t
  "Enable using code from both before and after point as prompt.
Unless just before the buffer end's trailing newlines (if any),
in that case don't use fill mode. See `starhugger-fill-tokens'
for the relevant tokens."
  :group 'starhugger
  :type 'boolean)

(defcustom starhugger-prompt-after-point-fraction (/ 1.0 4)
  "The length fraction that code after point should take in the prompt."
  :group 'starhugger
  :type 'float)

(defcustom starhugger-retry-temperature-range '(0.0 1.0)
  "The lower and upper bound of random temperature when retrying.
A single number means just use it without generating. nil means
don't set temperature at all. Set this to a list of numbers when
the model doesn't honor use_cache = false.

To test if the model honors use_cache = false, run this twice in
the shell:

curl https://api-inference.huggingface.co/models/bigcode/starcoder \\
        -X POST \\
        -H \"Content-Type: application/json\" \\
        -d \\='{\"options\": {\"use_cache\": false},\
 \"parameters\": {\"num_return_sequences\": 2}, \"inputs\": \"ping!\"}\\='

It should return 2 different responses, each with 2
\"generated_text\"."
  :group 'starhugger
  :type '(list float float))

(defun starhugger--retry-temperature ()
  (if (and (listp starhugger-retry-temperature-range)
           (not (seq-empty-p starhugger-retry-temperature-range)))
      (-let* (((lo hi) starhugger-retry-temperature-range))
        (--> (cl-random 1.0) (* it (- hi lo)) (+ lo it)))
    starhugger-retry-temperature-range))

(defun starhugger--log-before-request (url data &rest args)
  (setq starhugger--last-sent-request (list :url url :data data))
  (when starhugger-debug
    (dlet ((starhugger--log-buffer " *starhugger sent request data*"))
      (apply #'starhugger--log url data args))))

(defun starhugger--log-after-request
    (request-data &optional error-flag &rest args)
  (setq starhugger--last-returned-request request-data)
  (when (or starhugger-debug error-flag)
    (apply #'starhugger--log starhugger--last-returned-request args)))

(defcustom starhugger-notify-request-error t
  "Whether to notify if an error was thrown when the request is completed."
  :group 'starhugger
  :type 'boolean)

(defun starhugger--without-notify-request-error--a (func &rest args)
  (dlet ((starhugger-notify-request-error nil))
    (apply func args)))

(defun starhugger--request-dot-el--silent-sentinel-a (fn proc event &rest args)
  (cond
   ((member event '("interrupt" "interrupt\n"))
    (dlet ((request-message-level -1))
      (apply fn proc event args)))
   (:else
    (apply fn proc event args))))

(defun starhugger--request-dot-el-request (url &rest settings)
  "Wrapper around (`request' URL @SETTINGS) that silence errors when aborting.
See https://github.com/tkf/emacs-request/issues/226."
  (declare (indent defun))
  (-let* ((req (apply #'request url settings)))
    (add-function :around
                  (process-sentinel
                   (get-buffer-process (request-response--buffer req)))
                  #'starhugger--request-dot-el--silent-sentinel-a)
    req))

;;;;; Backends

;;;;;; Hugging Face inference API

(define-obsolete-variable-alias
  'starhugger-additional-data-alist
  'starhugger-hugging-face-additional-data-alist
  "0.5.0")

(defcustom starhugger-hugging-face-additional-data-alist
  '((parameters
     (return_full_text . :false) ; don't re-include the prompt
     )
    (options)
    ;;
    )
  "Detailed parameter list.
An association list to be converted by `json-serialize'. See
https://huggingface.co/docs/api-inference/detailed_parameters#text-generation-task
for parameters. Note that this packages is built around the
parameter \"return_full_text\" being false, setting it otherwise
may cause unexpected behaviors."
  :group 'starhugger
  :type 'alist)

(defun starhugger--HFI-get-api-token-as-string ()
  (cond
   ((stringp starhugger-hugging-face-api-token)
    starhugger-hugging-face-api-token)
   ((functionp starhugger-hugging-face-api-token)
    (funcall starhugger-hugging-face-api-token))))

(cl-defun starhugger-HFI--request (prompt
                                   callback
                                   &key
                                   data
                                   headers
                                   method
                                   max-new-tokens
                                   num-return-sequences
                                   force-new
                                   url
                                   &allow-other-keys)
  (-let* ((data
           (or data
               (starhugger--json-serialize
                `((inputs . ,prompt)
                  (parameters
                   ,@(and max-new-tokens `((max_new_tokens . ,max-new-tokens)))
                   ,@(and num-return-sequences
                          `((num_return_sequences . ,num-return-sequences)))
                   ,@(and force-new
                          starhugger-retry-temperature-range
                          `((temperature . ,(starhugger--retry-temperature))))
                   ,@(alist-get
                      'parameters starhugger-hugging-face-additional-data-alist)
                   (return_full_text . :false))
                  (options
                   ,@(and force-new '((use_cache . :false)))
                   ,@(alist-get
                      'options starhugger-hugging-face-additional-data-alist))))))
          (url (or url starhugger-hugging-face-api-url)))
    (dlet ((url-request-method (or method "POST"))
           (url-request-data data)
           (url-request-extra-headers
            (or headers
                `(("Content-Type" . "application/json")
                  ,@(-some-->
                        (starhugger--HFI-get-api-token-as-string)
                      `(("Authorization" . ,(format "Bearer %s" it))))))))
      (starhugger--log-before-request url data)
      (-let* ((request-buf
               (url-retrieve
                url
                (lambda (status)
                  (-let* ((content
                           (and url-http-end-of-headers
                                (buffer-substring
                                 url-http-end-of-headers (point-max)))))
                    (starhugger--log-after-request
                     (list
                      :response-content content
                      :send-data data
                      :response-status status)
                     (not url-http-end-of-headers)
                     :header
                     (buffer-substring-no-properties
                      (point-min) (or url-http-end-of-headers (point-max))))
                    (funcall callback content :url url)))
                nil t))
              (request-proc (get-buffer-process request-buf)))
        (list :process request-proc)))))

(cl-defun starhugger-hugging-face-inference-api (prompt callback &rest args &key &allow-other-keys)
  (apply #'starhugger-HFI--request
         prompt
         (cl-function
          (lambda (content &key url)
            (-let* ((parsed (json-parse-string content :object-type 'alist))
                    ((_ . err-msg) (and (listp parsed) (assoc 'error parsed)))
                    (generated-lst
                     (and (null err-msg)
                          (-map
                           (lambda (elem)
                             (alist-get 'generated_text elem))
                           parsed))))
              (funcall callback generated-lst :error err-msg :url url))))
         args))

;;;;;; Ollama

(defcustom starhugger-ollama-generate-api-url
  "http://localhost:11434/api/generate"
  "Ollama API's generation endpoint."
  :group 'starhugger
  :type 'string)

(defcustom starhugger-ollama-additional-parameter-alist
  '((options) (stream . :false))
  "Ollama API's advanced parameters.
See https://github.com/ollama/ollama/blob/main/docs/api.md#parameters."
  :group 'starhugger
  :type 'alist)

(cl-defun starhugger-ollama-completion-api (prompt callback &rest args &key
                                                   model force-new max-new-tokens
                                                   prefix suffix &allow-other-keys)
  (-let* ((model (or model starhugger-model-id))
          (sending-data
           (starhugger--json-serialize
            `((prompt . ,(if suffix prefix prompt))
              (model . ,model)
              ,@(and suffix `((suffix . ,suffix)))
              (options
               ,@(and max-new-tokens `((num_predict . ,max-new-tokens)))
               ,@(and force-new
                      starhugger-retry-temperature-range
                      `((temperature . ,(starhugger--retry-temperature))))
               ,@(and starhugger-chop-stop-token
                      `((stop . [,@starhugger-stop-tokens])))
               ,@(alist-get 'options starhugger-ollama-additional-parameter-alist))
              (stream . :false)
              ,@starhugger-ollama-additional-parameter-alist))))
    (starhugger--log-before-request
     starhugger-ollama-generate-api-url sending-data)
    (-let* ((request-obj
             (starhugger--request-dot-el-request
               starhugger-ollama-generate-api-url
               :type "POST"
               :data sending-data
               :error #'ignore
               :complete
               (cl-function
                (lambda (&rest
                         returned
                         &key
                         data
                         error-thrown
                         response
                         &allow-other-keys)
                  (-let* ((generated-lst
                           (if error-thrown
                               '()
                             (-some-->
                                 data
                               (json-parse-string it :object-type 'alist)
                               (list (alist-get 'response it))))))
                    (starhugger--log-after-request
                     (list
                      :response-content returned
                      :send-data sending-data
                      :response-status
                      (request-response-status-code response))
                     error-thrown)
                    (funcall callback
                             generated-lst
                             :model model
                             :error
                             (and error-thrown
                                  `((error-thrown ,error-thrown)
                                    (data ,data)))))))))
            (request-buf (request-response--buffer request-obj))
            (request-proc (get-buffer-process request-buf))
            (cancel-fn (lambda () (request-abort request-obj))))
      (list
       :cancel-fn cancel-fn
       :process request-proc
       :request-response request-obj))))


;;;;; Completion

(defcustom starhugger-completion-backend-function
  #'starhugger-ollama-completion-api
  "The backend for code suggestion.
The function accepts those arguments: prompt (string), callback
function (that accepts these arguments and should be supplied:
the model's list of generated strings, optional keywords such as
`:error'), and optional keyword(s): `:parameters', `:options':
alist of parameters and options, respectively to pass to the
backend's model; and other optional arguments: `:force-new',
`:max-new-tokens', `:num-return-sequences'.

It must return a plist that contains: `:process': the process to
be terminate to cancel the request, whose `process-sentinel' can
be decorated; optionally `:cancel-fn': a function that terminates
the running request when called, (calling `:cancel-fn' is
prioritized over stopping `:process')."
  :group 'starhugger
  :type 'function
  :options '(starhugger-ollama-completion-api starhugger-hugging-face-inference-api))

(defun starhugger--trim-from-stop-tokens (str &optional stop-token-lst)
  (named-let
      recur
      ((stop-token-lst (or stop-token-lst starhugger-stop-tokens))
       ;; I think literal `string-search' is faster than `replace-regexp-in-string'?
       (retval str))
    (-let* ((stop-token (car stop-token-lst))
            (found-end-pos (and stop-token (string-search stop-token retval))))
      (cond
       ((null stop-token)
        retval)
       (found-end-pos
        (recur (cdr stop-token-lst) (substring retval 0 found-end-pos)))
       (:else
        (recur (cdr stop-token-lst) retval))))))

(cl-defun starhugger--query-internal (prompt callback &rest args &key display spin backend caller &allow-other-keys)
  "CALLBACK is called with the generated text list and a plist.
PROMPT is the prompt to use. DISPLAY is whether to display the
generated text in a buffer. SPIN is whether to show a spinner.
ARGS are the arguments to pass to the BACKEND (or
`starhugger-completion-backend-function')"
  (run-hooks 'starhugger-before-request-hook)
  (-let*
      ((call-buf (current-buffer))
       (spin-obj
        (and spin starhugger-enable-spinner (starhugger--spinner-start)))
       (backend (or backend starhugger-completion-backend-function))
       ;; Haven't tested with HuggingFace so always remove just to be safe,
       ;; Ollama allows supplying stop sequences in the request:
       ;; https://github.com/ollama/ollama/blob/main/docs/modelfile.md#valid-parameters-and-values
       (manual-trim-stop-tokens
        (and starhugger-chop-stop-token
             (not (member backend '(starhugger-ollama-completion-api)))))
       (request-record nil))
    (letrec ((returned
              (apply backend
                     prompt
                     (cl-function
                      (lambda (gen-texts
                               &rest cb-args &key error &allow-other-keys)
                        (cl-callf
                            (lambda (lst) (delete request-record lst))
                            (gethash call-buf (starhugger--running-request-table)
                                     '()))
                        (when spin-obj
                          (funcall spin-obj))
                        (-let* ((err-str (format "%S" error))
                                (gen-texts-post-process
                                 (if manual-trim-stop-tokens
                                     (-map
                                      #'starhugger--trim-from-stop-tokens
                                      gen-texts)
                                   gen-texts)))
                          (starhugger--record-generated
                           prompt
                           gen-texts
                           :parameters args
                           :other-info cb-args
                           :backend backend
                           :display display)
                          (when (and error starhugger-notify-request-error)
                            (message "`starhugger' response error: %s" err-str))
                          (apply callback gen-texts-post-process cb-args))))
                     args)))
      (setq request-record (append returned `(:caller ,caller)))
      (push
       request-record (gethash call-buf (starhugger--running-request-table) '()))
      request-record)))


;;;; Overlay inline suggestion

(defvar-local starhugger--overlay nil)

(defface starhugger-inline-suggestion-face
  '((t
     :foreground "gray"
     :italic t
     ;; override `:underline' so that the flymake (or any other on-the-fly
     ;; checker) errors at point won't make the suggestion unreadable because of
     ;; the intrusive underlines, `:extend' prevents drawn underlines after line
     ;; ends
     :underline nil
     :extend t))
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
  :type 'natnum)


(defcustom starhugger-dismiss-suggestion-after-change t
  "Whether to clear the overlay when text changes and not partially accepted."
  :group 'starhugger
  :type 'boolean)

(defvar-local starhugger--inline-inhibit-changing-overlay nil)

(defun starhugger-inlining--after-change-h (&optional _beg _end _old-len)
  (when (and this-command
             (not starhugger--inline-inhibit-changing-overlay)
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
        (add-hook 'after-change-functions #'starhugger-inlining--after-change-h nil t))
    (progn
      (remove-hook 'after-change-functions #'starhugger-inlining--after-change-h t)
      (when (overlayp starhugger--overlay)
        (delete-overlay starhugger--overlay)))))

(defun starhugger--ensure-inlininng-mode (&optional off)
  (if off
      (when starhugger-inlining-mode
        (starhugger-inlining-mode 0))
    (unless starhugger-inlining-mode
      (starhugger-inlining-mode))))

(defcustom starhugger-numbers-of-suggestions-to-fetch '(2 3)
  "List of (natural) numbers of suggestions to fetch.

The first number is the number of suggestions to fetch when
`starhugger-trigger-suggestion' is called automatically.

The second number is the number of suggestions to fetch when
`starhugger-trigger-suggestion' is called interactively.

It can also be a single number, in which case the first number is
the same as the second number."
  :group 'starhugger
  :type '(choice natnum (list natnum natnum)))

(defun starhugger--current-overlay-suggestion ()
  (overlay-get starhugger--overlay 'starhugger-ovlp-current-suggestion))

(defvar starhugger-at-suggestion-map (make-sparse-keymap)
  "Use `starhugger-inline-menu-item' instead!
This doesn't work at the end of buffer.

Keymap used when at the beginning of suggestion overlay.")
(make-obsolete-variable 'starhugger-at-suggestion-map nil "0.1.18")

(defun starhugger--update-overlay (suggt &optional orig-pt)
  "Update overlay to display SUGGT after ORIG-PT.
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
          (when (overlay-get starhugger--overlay 'display)
            (overlay-put starhugger--overlay 'display nil))
          (overlay-put starhugger--overlay 'after-string suggt*))
      ;; currently I can't find a way to to achieve this:

      ;; 〈before〉|〈overlay〉〈after〉

      ;; so the workaround is too concatenate "overlay" and "after" and and put
      ;; the overlay on "after"
      (progn
        (when (overlay-get starhugger--overlay 'after-string)
          (overlay-put starhugger--overlay 'after-string nil))
        (overlay-put
         starhugger--overlay
         'display
         (concat suggt* (buffer-substring beg-pt (+ beg-pt 1))))))))

(defun starhugger--active-overlay-p ()
  (and starhugger--overlay (overlay-buffer starhugger--overlay)))

(defun starhugger--init-overlay (suggt pt)
  "Initialize over to show SUGGT and mark PT as the original position."
  (when (starhugger--active-overlay-p)
    (delete-overlay starhugger--overlay))
  (when (<= pt (point-max))
    (setq starhugger--overlay
          (make-overlay pt (+ pt 1)
                        nil
                        ;; allow inserting before the overlay
                        t t))
    ;; (overlay-put starhugger--overlay 'keymap starhugger-at-suggestion-map)
    (overlay-put starhugger--overlay 'priority 1)
    (starhugger--update-overlay suggt pt)))

(defun starhugger-at-suggestion-beg-p (&optional cmd)
  "Return CMD (or true) when point is at suggestion start.
See `starhugger-inline-menu-item'."
  (and starhugger-inlining-mode
       starhugger--overlay
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
    (when recent-suggt
      (starhugger--ensure-inlininng-mode)
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

(defun starhugger--prompt-build-components ()
  (if (and starhugger-fill-in-the-middle
           starhugger-fill-tokens
           ;; don't use fill mode when at trailing newlines
           (not (looking-at-p "\n*\\'")))
      (-let* ((intend-suf-len
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
                (t
                 (vector
                  (- (point) intend-pre-len) (+ (point) intend-suf-len)))))
              ([pre-beg-pos suf-end-pos]
               (vector
                (max (point-min) pre-beg-pos) (min (point-max) suf-end-pos)))
              (suf-str
               (-->
                (buffer-substring-no-properties (point) suf-end-pos)
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-right it)
                  it)))
              (pre-str
               (-->
                (buffer-substring-no-properties pre-beg-pos (point))
                (if starhugger-trim-spaces-around-prompt
                    (string-trim-left it)
                  it))))
        (vector pre-str suf-str))
    (vector (starhugger--no-fill-prompt) nil)))

(defcustom starhugger-enable-dumb-grep-context nil
  "Whether to inject a dumb grep-based project-wide context to the prompt.
Experimental! This requires ripgrep and python3 as hard
dependencies. Also remember to reduce
`starhugger-max-prompt-length' if you enable this."
  :group 'starhugger
  :type 'boolean)

(cl-defun starhugger--fim-concatenate (pre-code
                                       &optional
                                       suf-code
                                       &key
                                       pre-fim-prefix
                                       suf-fim-prefix
                                       suf-fim-suffix
                                       fim-tokens)
  (-let* (((pre-token mid-token suf-token)
           (and suf-code ; when no code after point, let fill tokens be nil
                (or fim-tokens starhugger-fill-tokens))))
    (concat
     pre-fim-prefix pre-token suf-fim-prefix ;
     pre-code mid-token suf-code ;
     suf-token suf-fim-suffix)))

(declare-function starhugger-grep-context--prefix-comments
                  "starhugger-grep-context")
(declare-function starhugger-grep-context--file-sep-before-prefix "starhugger-grep-context")

(defun starhugger--async-prompt (callback)
  "CALLBACK is called with the constructed prompt."
  (-let* (([pre-code suf-code] (starhugger--prompt-build-components))
          ((pre-token mid-token suf-token) starhugger-fill-tokens)
          (preset (starhugger--get-model-preset))
          (file-sep (map-nested-elt preset '(:file-separator)))
          (wrapped-callback
           (lambda (dumb-context)
             (-let* ((prompt
                      (cond
                       (suf-code
                        (concat
                         pre-token
                         dumb-context
                         pre-code
                         mid-token
                         suf-code
                         suf-token))
                       (t
                        (concat dumb-context pre-code)))))
               (funcall callback prompt)))))
    (if starhugger-enable-dumb-grep-context
        (progn
          (require 'starhugger-grep-context)
          (cond
           ((and file-sep)
            (starhugger-grep-context--file-sep-before-prefix
             callback pre-code suf-code file-sep))
           (:else
            (starhugger-grep-context--prefix-comments
             wrapped-callback pre-code suf-code))))
      (funcall callback
               (starhugger--fim-concatenate pre-code suf-code)
               :prefix pre-code
               :suffix suf-code))))

(defun starhugger--get-from-num-or-list (num-or-list &optional idx)
  (cond
   ((null num-or-list)
    nil)
   ((numberp num-or-list)
    num-or-list)
   ((numberp idx)
    (elt num-or-list idx))
   (idx
    (elt num-or-list 1))
   (t
    (elt num-or-list 0))))

;;;###autoload
(cl-defun starhugger-trigger-suggestion (&key interact force-new num prompt-fn max-new-tokens backend callback)
  "Show AI-powered code suggestions as overlays.
When an inline suggestion is already showing, new suggestions
will be fetched, you can switch to them by calling
`starhugger-show-next-suggestion' after fetching finishes. NUM:
number of suggestions to fetch at once (actually sequentially,
the newly fetched ones are appended silently). FORCE-NEW: try to
fetch different responses. Non-nil INTERACT: show spinner.
CALLBACK: to be called with the list of generated suggestions,
and a variadic plist."
  (interactive (list :interact t :force-new starhugger-inlining-mode))
  (-let*
      ((num
        (or num
            (starhugger--get-from-num-or-list
             starhugger-numbers-of-suggestions-to-fetch
             interact)))
       (caller-buf (current-buffer))
       (pt0 (point))
       (state (starhugger--suggestion-state))
       (prompt-fn (or prompt-fn #'starhugger--async-prompt))
       (prompt-callback
        (lambda (prompt &rest args)
          (when (< 0 (length prompt))
            (starhugger--ensure-inlininng-mode)
            (letrec
                ((func
                  (lambda (fetch-time)
                    (apply
                     #'starhugger--query-internal
                     prompt
                     (lambda (generated-texts &rest returned-args)
                       (when (and (buffer-live-p caller-buf)
                                  (not (plist-get returned-args :error)))
                         (with-current-buffer caller-buf
                           (-let* ((suggt-1st
                                    (-first-item generated-texts)))
                             (starhugger--add-to-suggestion-list
                              generated-texts state)
                             ;; only display when didn't move or interactive (in that
                             ;; case we are explicitly waiting)
                             (when (or interact (= pt0 (point)))
                               (when (= 1 fetch-time)
                                 (starhugger--ensure-inlininng-mode)
                                 (starhugger--init-overlay suggt-1st pt0))
                               (cond
                                ((and (< fetch-time num)
                                      (< (length generated-texts) num))
                                 (funcall func (+ fetch-time 1)))
                                ((not (starhugger--active-overlay-p))
                                 (starhugger--ensure-inlininng-mode 0)))))))
                       (when callback
                         (apply callback generated-texts returned-args)))
                     :max-new-tokens
                     (or max-new-tokens
                         (starhugger--get-from-num-or-list
                          starhugger-max-new-tokens
                          interact))
                     :num-return-sequences num
                     :force-new (or force-new (< 1 fetch-time))
                     :spin (or starhugger-debug interact)
                     :caller #'starhugger-trigger-suggestion
                     :backend backend
                     args))))
              (funcall func 1))))))
    (funcall prompt-fn prompt-callback)))

(defun starhugger--triggger-suggestion-prefer-cache
    (in-buffer position &optional cache-only callback)
  (when (and (equal in-buffer (current-buffer)) (equal position (point)))
    (or (starhugger--try-show-most-recent-suggestion)
        (when (not cache-only)
          (starhugger--cancel-requests-in-buffer
           (current-buffer)
           nil
           (lambda (request-plist)
             (member
              (plist-get request-plist :caller)
              '(starhugger-trigger-suggestion))))

          ;; TODO: prevent spamming requests
          (when starhugger-debug
            (-let* ((requests
                     (gethash
                      (current-buffer) (starhugger--running-request-table))))
              (when (<= 1 (length requests))
                (starhugger--log
                 (format "`%s' Already running %d requests in %S!"
                         'starhugger--triggger-suggestion-prefer-cache
                         (length requests)
                         (buffer-name))))))

          (starhugger-trigger-suggestion
           :callback callback
           :num
           (starhugger--get-from-num-or-list
            starhugger-numbers-of-suggestions-to-fetch
            nil))))))

(defun starhugger--cancel-request (plist)
  (-let* (((&plist :cancel-fn cancel-fn :process process) plist))
    (when process
      (add-function :around
                    (process-sentinel process)
                    #'starhugger--without-notify-request-error--a))
    (dlet ((starhugger-notify-request-error nil))
      (cond
       (cancel-fn
        (funcall cancel-fn))
       (process
        (dlet ((kill-buffer-query-functions '()))
          (-let* ((proc-buf (process-buffer process)))
            (delete-process process)
            (when proc-buf
              (kill-buffer proc-buf)))))))))

(defun starhugger--cancel-requests-in-buffer
    (buf &optional plist-lst plist-pred)
  (-let* ((plist-lst
           (or plist-lst (gethash buf (starhugger--running-request-table) '())))
          (new-plist-lst
           (-remove
            (lambda (plist)
              (cond
               ((or (null plist-pred) (funcall plist-pred plist))
                (starhugger--cancel-request plist)
                t)
               (:else
                nil)))
            plist-lst)))
    (puthash buf new-plist-lst (starhugger--running-request-table))))

(defun starhugger-cancel-all-requests-globally ()
  "Terminate running requests in all buffers."
  (interactive)
  (maphash
   (lambda (buf plist-lst)
     (starhugger--cancel-requests-in-buffer buf plist-lst))
   (starhugger--running-request-table)))

(defun starhugger-dismiss-suggestion (&optional stop-fetching)
  "Clear current suggestion.
Non-nil STOP-FETCHING (interactively, true by default): also
cancel unfinished fetches."
  (interactive (list (not current-prefix-arg)))
  (when stop-fetching
    (starhugger--cancel-requests-in-buffer (current-buffer)))
  (starhugger-inlining-mode 0))

(defcustom starhugger-trigger-suggestion-after-accepting t
  "Whether to continue triggering suggestion after accepting."
  :group 'starhugger
  :type 'boolean)

(defun starhugger-turn-off-completion-in-region-mode ()
  "Use this when inserting parsed response.
To prevent key binding conflicts such as TAB."
  (completion-in-region-mode 0))

(defvar starhugger-post-insert-hook
  '(starhugger-turn-off-completion-in-region-mode)
  "Hook run after inserting the parsed response.")

(defun starhugger--accept-suggestion-partially (by &optional args)
  "Insert a part of active suggestion by the function BY.
Accept the part that is before the point after applying BY on
ARGS. Note that BY should be `major-mode' independent (being
executed in a temporary buffer)."
  (-when-let* ((pos (overlay-start starhugger--overlay)))
    (dlet ((starhugger--inline-inhibit-changing-overlay t))
      (goto-char pos)
      (-let* ((suggt (starhugger--current-overlay-suggestion))
              (text-to-insert
               (with-temp-buffer
                 (insert suggt)
                 (goto-char (point-min))
                 (apply by args)
                 (buffer-substring (point-min) (point)))))
        (insert text-to-insert)
        (if (equal suggt text-to-insert)
            (progn
              (starhugger-dismiss-suggestion)
              (and starhugger-trigger-suggestion-after-accepting
                   (starhugger-trigger-suggestion :interact t)))
          (progn
            (starhugger--update-overlay
             (string-remove-prefix text-to-insert suggt))))
        ;; maybe put parentheses balancer here?
        (run-hooks 'starhugger-post-insert-hook)
        text-to-insert))))

(defun starhugger-accept-suggestion ()
  "Insert the whole suggestion."
  (interactive)
  (starhugger--accept-suggestion-partially
   (lambda ()
     (goto-char (point-max))
     ;; don't end at an empty newline awkwardly
     (skip-chars-backward "\n"))))

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
  "Insert N paragraphs from the suggestion."
  (interactive "p")
  (starhugger--accept-suggestion-partially #'forward-paragraph (list n)))


(defun starhugger-undo-accept-suggestion-partially ()
  "Undo all partial acceptances and go back."
  (interactive)
  (-let* ((orig-point
           (overlay-get starhugger--overlay 'starhugger-ovlp-original-position))
          (str (buffer-substring orig-point (point))))
    (dlet ((starhugger--inline-inhibit-changing-overlay t))
      (delete-char (- (length str)))
      (starhugger--update-overlay
       (concat str (starhugger--current-overlay-suggestion))))))


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

(defun starhugger-goto-suggestion ()
  "Go to the beginning of inline suggestion."
  (interactive)
  (goto-char (overlay-start starhugger--overlay)))


;;;; Auto-mode

(defcustom starhugger-auto-idle-time 0.5
  "Seconds to wait after typing, before fetching.
Note that the time taken to fetch isn' instantaneous, so we have
to wait more after this unless the suggestion(s) is already
cached, for the suggestion to appear."
  :group 'starhugger
  :type 'float)

(defcustom starhugger-auto-dismiss-when-move-out t
  "Whether to dismiss suggestion when moving point outside."
  :group 'starhugger
  :type 'boolean)

(defvar-local starhugger--auto-timer nil)

(defvar-local starhugger--auto-positional-state-old nil)

(defun starhugger--auto-positional-state (&rest other-info)
  (vector
   (point) (buffer-substring-no-properties (point-min) (point-max)) other-info))

(defun starhugger--auto-after-change-h (&optional beg end old-len)
  (when (and this-command (not starhugger--inline-inhibit-changing-overlay))
    (-let* ((tick-current (starhugger--auto-positional-state beg end old-len)))
      ;; Avoid spamming requests: do not activate another idle timer that have
      ;; exactly the same positional state as the previous one, whose request is
      ;; probably still running
      (when (not (equal starhugger--auto-positional-state-old tick-current))
        (setq starhugger--auto-positional-state-old tick-current)
        (when (timerp starhugger--auto-timer)
          (cancel-timer starhugger--auto-timer))
        (setq
         starhugger--auto-timer
         (run-with-idle-timer
          starhugger-auto-idle-time
          nil
          #'starhugger--triggger-suggestion-prefer-cache
          (current-buffer)
          (point)
          ;; don't fetch new when deleting
          (< 0 old-len)
          ;; When receiving suggestions, reset positional state so that
          ;; automatic trigger can happen later
          (lambda (&rest _)
            (setq starhugger--auto-positional-state-old nil))))))))

(defun starhugger--auto-post-command-h ()
  (when (and starhugger--overlay
             starhugger-inlining-mode
             starhugger-auto-dismiss-when-move-out
             (not starhugger--inline-inhibit-changing-overlay))
    (-let* ((beg
             (overlay-get
              starhugger--overlay 'starhugger-ovlp-original-position))
            (end (overlay-end starhugger--overlay)))
      (unless (and (numberp beg) (numberp end) (<= beg (point) end))
        (starhugger-dismiss-suggestion)))))

;;;###autoload
(define-minor-mode starhugger-auto-mode
  "Automatic `starhugger-trigger-suggestion' in current buffer."
  :lighter " 💫"
  :global nil
  (if starhugger-auto-mode
      (progn
        (add-hook 'post-command-hook #'starhugger--auto-post-command-h nil t)
        (add-hook 'after-change-functions #'starhugger--auto-after-change-h nil t))
    (progn
      (remove-hook 'post-command-hook #'starhugger--auto-post-command-h t)
      (remove-hook 'after-change-functions #'starhugger--auto-after-change-h t))))

;;;; Other commands

(defcustom starhugger-send-max-new-tokens 256
  "\"max_new_tokens\" for `starhugger-send'."
  :group 'starhugger
  :type 'natnum)

;;;###autoload
(defun starhugger-send (prompt &optional force-new)
  "Send PROMPT to the model without any contexts and display the answer.
Note this command is just a proof of concept, unlike a proper
chatbot, there are no contexts (including previous inputs) taken
into consideration.

Non-nil FORCE-NEW (interactively, prefix argument): try to force
a new answer."
  (interactive (list
                (read-string "Input: "
                             (and (use-region-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))))
                current-prefix-arg))
  (starhugger--query-internal
   prompt
   (lambda (&rest _)
     (with-selected-window (get-buffer-window starhugger-generated-buffer t)
       (goto-char (point-max))
       (search-backward (concat starhugger--record-heading-beg "INPUT"))
       (recenter 0)))
   :max-new-tokens starhugger-send-max-new-tokens
   :force-new force-new
   :caller #'starhugger-send
   :display t
   :spin t))

(defun starhugger--suggest-git-commit-message-prompt-fn (callback)
  (-let*
      ((cmd-args '("git" "--no-pager" "diff" "--cached"))
       (outbuf
        (generate-new-buffer-name
         " starhugger--suggest-git-commit-message-prompt-fn"))
       (sentinel
        (lambda (proc _event)
          (with-current-buffer outbuf
            (-let* ((exit-code (process-exit-status proc))
                    (proc-output
                     (buffer-substring-no-properties (point-min) (point-max))))
              (if (= 0 exit-code)
                  (-let*
                      ((prompt
                        (format
                         ;; "%s<commit_msg>"
                         "`git diff --cached`'s output:\n```\n%s\n```\nSuggested commit message:"
                         proc-output)))
                    (funcall callback prompt))
                (message "`starhugger:' %S exited with code: %s, output:\n%s"
                         cmd-args
                         exit-code
                         proc-output))))
          (kill-buffer outbuf)))
       (proc
        (apply #'start-process
               "starhugger--suggest-git-commit-message-prompt-fn"
               outbuf
               cmd-args)))
    (set-process-sentinel proc sentinel)))

;;;###autoload
(defun starhugger-suggest-git-commit-message ()
  (interactive)
  (starhugger-trigger-suggestion
   :interact t
   :force-new starhugger-inlining-mode
   :max-new-tokens 256
   :prompt-fn #'starhugger--suggest-git-commit-message-prompt-fn))


;;; starhugger.el ends here

(provide 'starhugger)

;; Local Variables:
;; byte-compile-docstring-max-column: 200
;; End:
