;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'dash)

(require 'project)

;; (require 's)

;;; Helpers

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
Still include script statements.

One of those case:

- Begins with a alphabetic characters and some
  contraints with the ending

- A Lisp function definition: (...def

And must contain a \"(\": sigifies a function in most programming
languages."
  "(^[a-zA-Z].*?[(].*?[^\"',;.]+$|^\\(([a-z-]+-)?def)")

(defun starhugger-grep-context--non-top-level-def-regex ()
  "Return an Rust regex to catch non-top-level definitions."
  (format "^[ \t_a-zA-Z0-9]+\\b(%s) "
          (string-join '("def" "define" "defn" "defun" "fn" "func" "function")
                       "|")))

(defun starhugger-grep-context--class-methods-regex ()
  "Return an ungrouped ERE to catch class methods."
  ;; C++-style class methods: begin with spaces, then some identifiers separated
  ;; by spaces, then an argument list (determined by heuristics), then a brace
  "^[ \t]*([ \t]+[_a-zA-Z]+[_a-zA-Z0-9]*)+\\([^\"'()!+-<>;]*?\\).*?\\{")

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
    (dlet ((default-directory
            (or (-some--> (project-current) (project-root it))
                default-directory)))
      (-let* ((cmd-args
               `(,starhugger-grep-context-ripgrep-executable
                 ,@(starhugger-grep-context--command-args-globs
                    (or ext (-some--> buffer-file-name (file-name-extension it))))
                 "--no-line-number"
                 "--no-filename"
                 "--max-columns=192"
                 "--color=never"
                 ,@(starhugger-grep-context--regex-args))))
        cmd-args))))

(defun starhugger-grep-context--get-lines (callback)
  "CALLBACK is called with the grepped lines as argument.
Or when an error occurs, with nil."
  (-let*
      (((prog . args) (starhugger-grep-context--command-args))
       (compare-text
        (concat
         (or (-some--> buffer-file-name (file-name-base it)) (buffer-name))
         ;; also take the first paragraph to compare? but this turns out to be too
         ;; noisy.
         ;; "\n"
         ;; (save-excursion
         ;;   (goto-char (point-min))
         ;;   (thing-at-point 'paragraph))
         ))
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
              (-let* ((lines
                       (-->
                        (starhugger--buffer-string-lines)
                        (delete "" it)
                        (delete-dups it)))
                      (sorted
                       (cl-sort
                        lines #'<
                        :key (lambda (line) (string-distance compare-text line))))
                      (top-lines (ntake 192 sorted)))
                (funcall callback top-lines))))
       (kill-buffer buf)))))

;;; starhugger-project-context.el ends here

(provide 'starhugger-project-context)
