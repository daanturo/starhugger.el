;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'dash)

(require 'project)

;; (require 's)

;;; Helpers

(cl-defmacro starhugger--at-project-root (&rest body)
  (declare (debug t) (indent defun))
  `(dlet ((default-directory
           (or (-some--> (project-current) (project-root it))
               default-directory)))
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

;;
;; This is a very naive implementation of project-wide context.
;; It's not meant to be used in production, but it's a good
;; starting point for a more sophisticated one.
;;
;; It's based on ripgrep, which is a Rust-based grep.
;;
;; It's not meant to be used in production, but it's a good
;; starting point for a more sophistic

(defun starhugger-grep-context--top-level-regex ()
  "Return a Rust regex for top-level definitions.
Still include script statements."
  (eval-when-compile
    (-->
     `(,(concat
         "^[a-zA-Z]" ; begin with an alphabetic character
         "[^=]*?" ; some characters before "(", but they can't be some
         "[(]" ; function indicator
         ".*?[^\"',;.]$" ; doesn't end with those, cause it looks more like a natural sentence
         )
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

(defvar starhugger-grep-context--ignore-keywords
  '("if" "while"
    ;;
    "switch" "else" "elif" "when" "for" "catch" "return")
  "List of keywords to ignore when looking for context.
When they are at line beginning.")

(defvar starhugger-grep-context--max-lines 192)

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
             `(,starhugger-grep-context-ripgrep-executable
               ,@(starhugger-grep-context--command-args-globs
                  (or ext (-some--> buffer-file-name (file-name-extension it))))
               "--no-line-number"
               "--no-filename"
               "--max-columns=192"
               ,(and (not starhugger-debug) "--color=never")
               ,@(starhugger-grep-context--regex-args))))
      cmd-args)))

(defun starhugger-grep-context--process-grepped-buffer (compare-text callback)
  (-let* ((lines
           (-->
            (starhugger--buffer-string-lines) (delete "" it) (delete-dups it)
            (cl-delete-if
             (lambda (line)
               (-some
                (lambda (kw) (string-match-p (format "^[ \t]*%s\\b" kw) line))
                starhugger-grep-context--ignore-keywords))
             it)))
          (sorted
           (cl-sort
            lines #'<
            :key (lambda (line)
                   (-let* ((len (length line))
                           (distance (string-distance compare-text line)))
                     ;; don't penalize long lines so harsh
                     (if (< 1 len)
                         (/ distance (log len))
                       distance)))))
          (top-lines
           (save-match-data
             (-->
              (ntake starhugger-grep-context--max-lines sorted)
              (cl-stable-sort
               it #'>
               :key (lambda (line) (string-match "[^ \t]" line)))))))
    (funcall callback top-lines)))

(defun starhugger-grep-context--get-lines (callback)
  "CALLBACK is called with the grepped lines as argument.
Or when an error occurs, with nil."
  (starhugger--at-project-root
    (-let* (((prog . args) (starhugger-grep-context--command-args))
            (compare-text
             (concat
              (-let* ((prj (project-current))
                      (prj-root (and prj (project-root prj))))
                (cond
                 ((and prj buffer-file-name)
                  (file-name-sans-extension
                   (file-relative-name buffer-file-name prj-root)))
                 (buffer-file-name
                  (file-name-base buffer-file-name))
                 (t
                  (buffer-name))))))
            (buf (generate-new-buffer " starhugger-grep-context--get-lines"))
            (proc
             (apply #'start-process
                    "starhugger-grep-context--get-lines"
                    buf
                    prog
                    args)))
      (set-process-sentinel
       proc
       (lambda (_proc _event)
         (and (zerop (process-exit-status proc))
              (with-current-buffer buf
                (starhugger-grep-context--process-grepped-buffer
                 compare-text callback)))
         (kill-buffer buf))))))


;;; starhugger-project-context.el ends here

(provide 'starhugger-project-context)
