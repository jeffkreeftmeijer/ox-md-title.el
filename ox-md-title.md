
# ox-md-title: Document titles for ox-md.el

Ox-md-title.el adds document titles to markdown files generated with ox-md and derivatives. It consists of a single advice, which is added around the `org-md-template` function to prepend the document title:

```emacs-lisp
(defun org-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args)))
    (let ((style (plist-get info :md-headline-style))
          (title (org-export-data (plist-get info :title) info)))
      (concat
       (org-md--headline-title (plist-get info :md-headline-style) 1 (org-export-data (plist-get info :title) info) nil)
       (apply orig-fun args)))))

(defun org-md-title-add ()
  (setq org-md-toplevel-hlevel 2)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (setq org-md-toplevel-hlevel 1)
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