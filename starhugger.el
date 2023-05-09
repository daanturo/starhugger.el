;; -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'dash)
(require 's)
(require 'compat)


(defcustom starhugger-api-token nil
  nil
  :group 'starhugger)

(defcustom starhugger-base-url "https://api-inference.huggingface.co/models/"
  nil
  :group 'starhugger)

(defcustom starhugger-model "bigcode/starcoder"
  nil
  :group 'starhugger)

(defun starhugger--alist->url-request-data (alst)
  (mapconcat (-lambda
               ((k . v))
               (concat
                (url-hexify-string (format "%s" k)) "=" (url-hexify-string v)))
             alst
             "&"))

(defun starhugger--get-generated-text (str)
  (-->
   (json-parse-string str :object-type 'alist)
   (seq-elt it 0)
   (alist-get 'generated_text it)))

;;;###autoload
(cl-defun starhugger-request
    (query callback &key model data headers non-json-data method)
  (dlet ((url-request-method (or method "POST"))
         (url-request-data
          (or data
              (if non-json-data
                  (starhugger--alist->url-request-data `((inputs . ,query)))
                (json-serialize `((inputs . ,query))))))
         (url-request-extra-headers
          (or headers
              `(("Content-Type" . "application/json")
                ,@
                (and (< 0 (length starhugger-api-token))
                     `(("Authorization" .
                        ,(format "Bearer %s" starhugger-api-token))))))))
    (url-retrieve
     (concat starhugger-base-url (or model starhugger-model))
     (lambda (_status &rest _)
       (funcall callback (buffer-substring url-http-end-of-headers (point-max))))
     nil t)))

;;;###autoload
(defun starhugger-query (query-str &optional insert)
  (interactive (list
                (read-string "Prompt: "
                             (and (use-region-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))))
                current-prefix-arg))
  (-let* ((pt0
           (if (use-region-p)
               (region-end)
             (point)))
          (buf0 (current-buffer))
          (modtick (buffer-modified-tick)))
    (starhugger-request
     query-str
     (lambda (retstr)
       (with-current-buffer buf0
         (-let* ((gen-text (starhugger--get-generated-text retstr))
                 (record-fn
                  (lambda ()
                    (pop-to-buffer
                     (get-buffer-create (format "*%s*" 'starhugger)))
                    (goto-char (point-max))
                    (insert "\n\n" gen-text)))
                 (insert-flag
                  (and insert (equal modtick (buffer-modified-tick)))))
           (when insert-flag
             (deactivate-mark)
             (goto-char pt0)
             (insert (string-remove-prefix query-str gen-text)))
           (if insert-flag
               (save-window-excursion (funcall record-fn))
             (save-selected-window
               (funcall record-fn)))))))))

;;; starhugger.el ends here

(provide 'starhugger)
