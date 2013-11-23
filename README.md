blip
====

Automatically find and run tests in emacs

Usage
-----

Use `M-x blip-mode` to enable `blip-mode`, which runs tests whenever
you save the buffer.

Tests are deduced using the rules in the variable
`blip-find-test-file-rules`

The function to perform the testing is deduced using the variable
`blip-run-functions`. Currently only emacs24's own `ert`-based tests
are supported through `blip--run-ert-tests`, which is bundled.

Both `blip-find-test-file-rules` and `blip-run-functions` are intended
to be customized by the user, see their doc.

Notes
-----

`blip.el` does more or less the same as
[unit-test.el](http://www.emacswiki.org/emacs/unit-test.el) but

* NIH
* I didn't know of `unit-test.el` when I started
* I think `blip.el` does a bit more and takes greater advantage of
  lexical binding in emacs 24
* NIH

TODO
----

* more self tests
* more functions to run tests in other languages/frameworks
* fix bugs and cleanup
