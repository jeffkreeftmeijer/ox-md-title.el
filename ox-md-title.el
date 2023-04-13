;;; ox-md-title.el --- Document titles for ox-md.el

;;; Commentary:

;; ox-md-title.el adds document titles to markdown files generated
;; with ox-md and derivatives.

;;; Code:

(require 'ox-md)

(defun org-md-title--advise-template (orig-fun &rest args)
  (let* ((info (nth 1 args))
         (style (plist-get info :md-headline-style))
         (title (org-export-data (plist-get info :title) info)))
    (concat
     (org-md--headline-title style 0 title nil)
     (apply orig-fun args))))

(defun org-md-title--advise-headline (args)
  (setf (nth 1 args) (+ (nth 1 args) 1))
  args)

(defun org-md-title-add ()
  (advice-add 'org-md--headline-title :filter-args #'org-md-title--advise-headline)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md--headline-title #'org-md-title--advise-headline)
  (advice-remove 'org-md-template #'org-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
