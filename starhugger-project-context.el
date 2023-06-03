;; -*- lexical-binding: t; -*-

(require 'dash)

(defun starhugger-project-context--top-level-non-comment-regex
    (&optional comment-beg)
  (-let* ((comment-beg (or comment-beg (string-trim comment-start))))
    (if comment-beg
        (-->
         (-map
          (lambda (idx)
            (if (zerop idx)
                (-->
                 (format "[^\\s%c(]" (elt comment-beg 0))
                 ;; lisp scripts: top-level must be *def
                 (concat it "|\\(([a-z-]+-)?def"))
              (format "%s[^%c]"
                      (regexp-quote (substring comment-beg 0 idx))
                      (elt comment-beg idx))))
          (-iota (length comment-beg)))
         (string-join it "|") (format "^(%s)" it))
      "^[^[:space:]]")))

(defun starhugger-project-context--regex ()
  (-->
   `(
     ;; top-level definitions, currently still include script statements
     ,(starhugger-project-context--top-level-non-comment-regex)
     ;; common keywords
     ,(format "^[\\s_a-zA-Z0-9]+\\b(%s) "
              (string-join '("def"
                             "define"
                             "defn"
                             "defun"
                             "fn"
                             "func"
                             "function")
                           "|"))
     ;; C++-style class methods: begin with spaces, then some identifiers
     ;; separated by spaces, then an argument list (determined by heuristics),
     ;; then a brace
     "^\\s+(\\s+[_a-zA-Z]+[_a-zA-Z0-9]*)+\\([^\"'()!+-<>;]*?\\).*?\\{")
   (string-join it "|")))

(defun starhugger-project-context ()
  (dlet ((default-directory
          (or (-some--> (project-current) (project-root it))
              default-directory)))
    (-let* ((glob-file-extension
             (-if-let* ((ext (file-name-extension buffer-file-name)))
                 (format "-g '*.%s'" ext)
               ""))
            (regex (starhugger-project-context--regex)))
      (shell-command-to-string
       (format "rg --no-line-number --no-filename %s %S"
               glob-file-extension
               regex)))))


;;; starhugger-project-context.el ends here

(provide 'starhugger-project-context)
