;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'starhugger)

;;; Commentary:

;;; Code:

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
  "Insert completion using the prompt from BEGINNING to current point.
BEGINNING defaults to start of current line, paragraph or defun,
whichever is the furthest.

Interactively, you may find `starhugger-complete*' performs
better as it takes as much as `starhugger-max-prompt-length'
allows, starting from buffer beginning."
  (declare (obsolete nil "0.1.8"))
  (interactive)
  (-let* ((beg (or beginning (starhugger--complete-default-beg-position)))
          (prompt (buffer-substring-no-properties beg (point))))
    (starhugger-query prompt :beg-pos beg :end-pos (point))))

;;;###autoload
(defun starhugger-complete* ()
  "Insert completion, use as much text as possible before point as prompt.
Just `starhugger-complete' under the hood."
  (declare (obsolete nil "0.1.8"))
  (interactive)
  (starhugger-complete (point-min)))

(defun starhugger-complete-from-last-prompt ()
  (interactive)
  (starhugger-complete starhugger--last-prompt-beg-pos))

;;;###autoload
(defun starhugger--continue-complete-menu-item-filter (_)
  (and starhugger--last-prompt-beg-pos #'starhugger-complete-from-last-prompt))

;;;###autoload
(progn
  (define-minor-mode starhugger-global-mode
    "Currently doesn't do very much.
Beside enabling successive completions.
This doesn't trigger loading `starhugger.el'."
    :group 'starhugger
    :global t
    :keymap
    (let* ((tab-item
            `(menu-item "" nil
              :filter starhugger--continue-complete-menu-item-filter)))
      (list
       (cons (kbd "TAB") tab-item) ;
       (cons (kbd "<tab>") tab-item) ;
       ))
    (if starhugger-global-mode
        (progn)
      (progn))))




;;; starhugger-x.el ends here

(provide 'starhugger-x)
