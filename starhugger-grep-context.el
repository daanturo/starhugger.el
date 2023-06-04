;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'dash)

(require 'project)

;; (require 's)

;;; Helpers

(defun starhugger--project-root ()
  (-some--> (project-current) (project-root it)))

(cl-defmacro starhugger--at-project-root (&rest body)
  (declare (debug t) (indent defun))
  `(dlet ((default-directory (or (starhugger--project-root) default-directory)))
     ,@body))

(defun starhugger--not-literal-regex (str &optional additional-negations)
  "Return an ungrouped Rust regex that is the negation of literal STR.
ADDITIONAL-NEGATIONS: a string to put in [^...] to negate also
them, default to an empty string."
  (-let* ((additional-negations (or additional-negations "")))
    (-->
     (-map
      (lambda (idx)
        (concat
         (regexp-quote (substring str 0 idx))
         (format "[^%c%s]" (elt str idx) additional-negations)))
      (-iota (length str)))
     (string-join it "|"))))

(defun starhugger--buffer-string-lines ()
  "Doesn't `save-excursion'.
Credit: `process-lines-handling-status'."
  (goto-char (point-min))
  (let (lines)
    (while (not (eobp))
      (setq lines
            (cons
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position))
             lines))
      (forward-line 1))
    (nreverse lines)))

;;; Dumb Grep/Regex-based project-wide code context

(defun starhugger-grep-context--top-level-regex ()
  "Return a Rust regex for top-level definitions.
May still include script statements."
  (eval-when-compile
    (-->
     `(,(concat
         "^[a-zA-Z]" ; begin with an alphabetic character
         "[^=]*?" ; some characters before "(", but they can't be some
         "[(]" ; function indicator
         ".*?[^\"',;.]$" ; doesn't end with those, cause it looks more like a natural sentence
         )
       ;; C macros
       "^#define "
       ;; lisp
       "^\\(([-a-z]+-)?def")
     (string-join it "|") (concat "(" it ")"))))

(defun starhugger-grep-context--non-top-level-def-regex ()
  "Return an Rust regex to catch non-top-level definitions."
  (eval-when-compile
    (concat
     ;; line start, then a series of spaces or identifiers
     "^[ \t_a-zA-Z0-9]+"
     ;; (then a word delimiter,) then a defined keyword
     (format "\\b(%s)"
             (string-join '("def"
                            "define"
                            "defn"
                            "defun"
                            "fn"
                            "func"
                            "function")
                          "|"))
     ;; space, then a non-underscore character (public function)
     " [^_]"
     ;; contains "("
     "[^(]*\\(")))

(defun starhugger-grep-context--class-methods-regex ()
  "Return an ungrouped ERE to catch class methods."
  (eval-when-compile
    ;; C++-style class methods:
    (concat
     ;; begin with spaces
     "^[ \t]*?"
     ;; then some identifiers separated by spaces
     "([ \t]+[_a-zA-Z]+[_a-zA-Z0-9]*)+"
     ;;  then an argument list (determined by heuristics)
     "\\([^-\"'()!+<>;]*?\\)"
     ;; (maybe return type then) a brace
     ".*?\\{")))

(defvar starhugger-grep-context--ignored-keywords
  '["if" "while"
    ;;
    "switch" "else" "elif" "when" "for" "catch" "return"]
  "List of keywords to ignore when looking for context.
When they are at line beginning.")

(defvar starhugger-grep-context--max-lines 96)

(defun starhugger-grep-context--regex-args ()
  `("-e"
    ,(starhugger-grep-context--top-level-regex)
    "-e"
    ,(starhugger-grep-context--non-top-level-def-regex)
    "-e"
    ,(starhugger-grep-context--class-methods-regex)))

(defcustom starhugger-grep-context-ripgrep-executable (executable-find "rg")
  "Executable path for ripgrep (https://github.com/BurntSushi/ripgrep).
When nil, project context won't be used."
  :group 'starhugger
  :type 'string)

(defun starhugger-grep-context--command-args-globs (&optional ext)
  `(,@(and ext (list "-g" (format "*.%s" ext)))
    ;; exclude tests, after the "positive" globs
    "-g" "!test"
    "-g" "!tests"
    "-g" "!test/"
    "-g" "!tests/"
    "-g" "!tests[-_.]*"
    "-g" "!test[-_.]*"
    "-g" "!**[-_.]tests[-_.]*"
    "-g" "!**[-_.]test[-_.]*"
    ;;
    ))

(cl-defun starhugger-grep-context--command-args (&key ext)
  (when starhugger-grep-context-ripgrep-executable
    (-let* ((cmd-args
             `[,starhugger-grep-context-ripgrep-executable
               ,@(starhugger-grep-context--command-args-globs
                  (or ext (-some--> buffer-file-name (file-name-extension it))))
               "--no-line-number"
               "--no-filename"
               "--max-columns=160"
               ,(and (not starhugger-debug) "--color=never")
               ,@(starhugger-grep-context--regex-args)]))
      cmd-args)))

(defvar starhugger-grep-context--script-path nil)
(defun starhugger-grep-context--script-path ()
  "Return the path of the script to execute for grepping the context.
Reason(s ) for using another non-Elisp script: The filtering and
sorting computation is a bit expensive to run so it will block
Emacs, while current Emacs lisp's asynchronous capabilities are
not great: `async-start' doesn't have access to the environment
of the current Emacs session including packages and libraries."
  (with-memoization starhugger-grep-context--script-path
    (or (locate-library "starhugger-grep-context-process.py")
        (error
         "%s not found, please review your installation recipe."
         "starhugger-grep-context-process.py"))))

(defun starhugger-grep-context--get-lines (callback)
  "CALLBACK is called with the grepped lines as argument.
Or when an error occurs, with nil."
  (starhugger--at-project-root
    (-let* ((grep-args (starhugger-grep-context--command-args))
            (compare-text
             (concat
              (-let* ((prj-root (starhugger--project-root)))
                (cond
                 ((and prj-root buffer-file-name)
                  (file-name-sans-extension
                   (file-relative-name buffer-file-name prj-root)))
                 (buffer-file-name
                  (file-name-base buffer-file-name))
                 (t
                  (buffer-name))))))
            (script-json-arg
             (json-serialize
              `((command-arguments . ,grep-args)
                (compare-text . ,compare-text)
                (ignored-keywords . ,starhugger-grep-context--ignored-keywords)
                (max-lines . ,starhugger-grep-context--max-lines))))
            (buf (generate-new-buffer " starhugger-grep-context--get-lines"))
            (script-path (starhugger-grep-context--script-path))
            (wrapped-callback
             (lambda (stn-proc event)
               (if (zerop (process-exit-status stn-proc))
                   (progn
                     (with-current-buffer buf
                       (-let* ((lines (starhugger--buffer-string-lines)))
                         (funcall callback lines)))
                     (kill-buffer buf))
                 (progn
                   (message "%s %s
error: %s
%s"
                            (file-name-nondirectory script-path)
                            script-json-arg
                            event
                            (buffer-string))
                   (funcall callback nil)))))
            (proc
             (start-process "starhugger-grep-context--get-lines" buf script-path
                            script-json-arg)))
      (set-process-sentinel proc wrapped-callback)
      proc)))

(defvar-local starhugger-grep-context--prefix-comments--cache nil)

;;;###autoload
(defun starhugger-grep-context--prefix-comments (callback)
  "CALLBACK is called with an annotated comment string as argument.
This must be called in the completing buffer."
  (if starhugger-grep-context--prefix-comments--cache
      (funcall callback starhugger-grep-context--prefix-comments--cache)
    (-let* ((buf0 (current-buffer))
            (cmt-beg comment-start)
            (cmt-end comment-end)
            (cmt-end* (and (< 0 (length cmt-end)) (concat " " cmt-end)))
            (cmt-fn (lambda (str) (concat cmt-beg " " str cmt-end*))))
      (starhugger-grep-context--get-lines
       (lambda (lines)
         (-let* ((commented-lines
                  (--> lines (-map cmt-fn it) (string-join it "\n")))
                 (note (funcall cmt-fn "Context in other files:"))
                 (context
                  (if (< 0 (length lines))
                      (concat note "\n" commented-lines "\n")
                    "")))
           (with-current-buffer buf0
             (setq starhugger-grep-context--prefix-comments--cache context)
             (funcall callback context))))))))


;;; starhugger-grep-context.el ends here

(provide 'starhugger-grep-context)
