;;; ox-md-title.el --- Document titles for ox-md.el

;;; Commentary:

;; ox-md-title.el adds document titles to markdown files generated
;; with ox-md and derivatives.

;;; Code:

(require 'ox-md)

(defun ox-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args)))
    (let ((style (plist-get info :md-headline-style))
          (title (org-export-data (plist-get info :title) info)))
      (concat
       (org-md--headline-title (plist-get info :md-headline-style) 1 (org-export-data (plist-get info :title) info) nil)
       (apply orig-fun args)))))

(defun ox-md-title-add ()
  (setq org-md-toplevel-hlevel 2)
  (advice-add 'org-md-template :around #'ox-md-title--advise-template))

(defun ox-md-title-remove ()
  (setq org-md-toplevel-hlevel 1)
  (advice-remove 'org-md-template #'ox-md-title--advise-template))

(provide 'ox-md-title)

;;; ox-md-title.el ends here
