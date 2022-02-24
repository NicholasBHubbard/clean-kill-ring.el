# -*- mode:org;mode:auto-fill;fill-column:79 -*-
#+title: clean-kill-ring.el 
#+author: Nicholas Hubbard

* Disclaimer

This package is still under development.

* Usage

When the minor-mode =clean-kill-ring-mode= is enabled, any string that
satisfies one of the filter predicates in the =clean-kill-ring-filters= list
variable will not be allowed into the =kill-ring=.

By default =clean-kill-ring-filters= only contains the predicate
=string-blank-p=, which means that no blank lines will be allowed into the
=kill-ring=.

If the variable =clean-kill-ring-prevent-duplicates= is set to a non-nil value
then upon killing text that already exists in =kill-ring=, the old duplicate
entries will be removed.

To manually clean the =kill-ring=, call the interactive function
=clean-kill-ring-clean=.

* Installation

*** MELPA

This library is not on MELPA yet.

*** Quelpa

#+BEGIN_SRC
(use-package clean-kill-ring
  :quelpa (clean-kill-ring :fetcher github :repo "NicholasBHubbard/clean-kill-ring.el")
  :config
  (clean-kill-ring-mode 1))
#+END_SRC

*** Straight

#+BEGIN_SRC 
(use-package clean-kill-ring
  :straight (clean-kill-ring :type git :host github :repo "NicholasBHubbard/clean-kill-ring.el")
  :config
  (clean-kill-ring-mode 1))
#+END_SRC

*** Manual

Put =clean-kill-ring.el= in your =load-path=, then:

#+BEGIN_SRC
(require 'clean-kill-ring)

(clean-kill-ring-mode 1)
#+END_SRC

* Implementation

When =clean-kill-ring-mode= is active the function =kill-new= is advised.

* License

MIT
