
# ox-md-title: Document titles for ox-md.el

Ox-md-title.el adds document titles to Markdown files generated with ox-md and derivatives.


## Introduction

Org documents often have their document titles set in a `+#title` export setting at the start of the file. The title is used when exporting to other formats. For example, when exporting to HTML, the title is used in the resulting document's `<title>` and `<h1>` tags.

Markdown doesn't have an equivalent to Org's titles. Instead, it's common to add a top-level headline to the start of the document.

[Ox-md](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/ox-md.el), the Markdown exporter shipped with Org mode, adheres to Markdown's lack of explicit titles.<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> Even though Org documents can have titles through the title export setting, ox-md produces Markdown files with the titles omitted. For example, the current document has export settings, including a title:

```org
#+title: ox-md-title: Document titles for ox-md.el
#+author: Jeff Kreeftmeijer
#+date: 2023-04-24
#+options: toc:nil

Ox-md-title.el adds document titles to Markdown files generated with ox-md and derivatives.

* Introduction
```

Exporting with `org-md-export-as-markdown` produces a Markdown document without a title headline:

```markdown
Ox-md-title.el adds document titles to Markdown files generated with ox-md and derivatives.


# Introduction
```

Ox-md-title deviates from the Markdown standard and adds document titles to behave like the other org exporters. Once enabled, it adds the document title in front of the exported document, and shifts the sub headlines one level down:

```markdown

# ox-md-title: Document titles for ox-md.el

Ox-md-title.el adds document titles to Markdown files generated with ox-md and derivatives.


## Introduction
```


## Implementation

The package works by advising two functions. First, it advises `org-md-template` by prepending the document title if `org-export-with-title` is non-nil. The title is built by calling out to `org-md--headline-title` with the headline style and title extracted from the `info` variable:

```emacs-lisp
(defgroup org-export-md-title nil
  "Options for org-md-title."
  :tag "Org Markdown Title"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defun org-md-title--advise-template (orig-fun &rest args)
  (let ((info (nth 1 args))
	(orig-result (apply orig-fun args)))
    (if (plist-get info :with-title)
	(let ((style (plist-get info :md-headline-style))
	      (title (plist-get info :title))
	      (subtitle (plist-get info :subtitle)))
	  (concat
	   (when title
	     (org-md--headline-title style 0 (org-export-data title info) nil))
	   (when subtitle
	     (org-md--headline-title style 1 (org-export-data subtitle info) nil))
	   orig-result))
      orig-result)))
```

Because a new title is prepended to the document, any already-existing headlines need their levels bumped up. The second advice intercepts calls to `org-md--headline-title`, which is the internal function the Markdown exporter uses to generate headlines in the selected headline style.

Whenever that function is called, the advise kicks in and increments the second argument with 1<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup>: This means that whenever the `org-md--headline-title` is called with a headline level of 1, it actually receives a 2. The previously defined advise in `org-md-title--advise-template` already accounts for that by using 0 and 1, instead of 1 and 2, for its title and subtitle levels.

```emacs-lisp
(defun org-md-title--advise-headline-title (args)
  (setf (nth 1 args) (+ (nth 1 args) 1))
  args)
```

Finally, the added functions are added as advice:

```emacs-lisp
(defun org-md-title-add ()
  (advice-add 'org-md--headline-title :filter-args #'org-md-title--advise-headline-title)
  (advice-add 'org-md-template :around #'org-md-title--advise-template))

(defun org-md-title-remove ()
  (advice-remove 'org-md--headline-title #'org-md-title--advise-headline-title)
  (advice-remove 'org-md-template #'org-md-title--advise-template))
```


## Installation and usage

Ox-md-title is currently not available through any of the package registries. Instead, install it from the repository direcly. Install the package with [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/radian-software/straight.el), and enable it by calling `org-md-title-add`:

```emacs-lisp
(use-package ox-md-title
  :straight
  (ox-md-title :type git :host codeberg :repo "jkreeftmeijer/ox-md-title.el")
  :config
  (org-md-title-add))
```

Alternatively, download `ox-md-title.el` and require it manually:

```emacs-lisp
(require 'ox-md-title)
(org-md-title-add)
```

After calling `org-md-title-add`, set the `org-md-title` variable to add document titles when exporting with ox-md:

```emacs-lisp
(let ((org-md-title t))
  (org-markdown-export-to-markdown))
```


## Contributing

The git repository for ox-md-title.el is hosted on [GitHub](https://github.com/jeffkreeftmeijer/ox-md-title.el) and [Codeberg](https://codeberg.org/jkreeftmeijer/ox-md-title.el). Contributions are welcome via either platform.


### Tests

Regression tests are written with [ERT](https://www.gnu.org/software/emacs/manual/html_mono/ert.html) and included in `test.el`. To run the tests in batch mode:

```shell
emacs -batch -l ert -l test.el -f ert-run-tests-batch-and-exit
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> A [patch to add titles](https://lists.gnu.org/archive/html/emacs-orgmode/2017-08/msg00553.html) was rejected to keep ox-md compatible with standard Markdown. Instead of adding support for titles in the main implementation, it's suggested that features like this should be implemented in more specific backends:

> The point of "md" export back-end is not to provide the same features as full-fledged ones like "latex" or "html". I wrote it to take care of the boring stuff of markdown syntax. Anyone willing to write a back-end with a different Markdown flavour just needs to concentrate of the differences between the original syntax.

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> When the document has a title set, it's used as the document's top level, and other headlines are pushed down one level:

```markdown
# Title
## Headline 1
### Headline 2
```

If the document does not have a title, but `org-md-title`'s advice is added, the headlines are still pushed down:

```markdown
## Headline 1
### Headline 2
```

Bumping all headline levels up gives the same behavior as in Org's HTML exporter, which uses a `<h1>` tag for the title, and a `<h2>` tag for the top level headline, even if the source document does not have a title set.