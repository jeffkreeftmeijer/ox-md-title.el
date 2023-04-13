
# ox-md-title: Document titles for ox-md.el

Ox-md-title.el adds document titles to markdown files generated with ox-md and derivatives. It consists of a single advice, which is added around the `org-md-template` function to prepend the document title:

```emacs-lisp
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
```


## Usage

Call `org-md-title-add` to add ox-md-title's advice before exporting<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>, and call `org-md-title-remove` after:

```emacs-lisp
(use-package ox-gfm)
(require 'ox-md-title)

(org-md-title-add)
(org-gfm-export-to-markdown)
(org-md-title-remove)
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> This example uses [ox-gfm](https://github.com/larstvei/ox-gfm), which works because it's a backend derived from ox-md.