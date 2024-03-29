+++
title = "Imaginary AGI Library with Pen.el"
author = ["Shane Mulligan"]
date = 2021-11-24T00:00:00+13:00
keywords = ["demo", "pen", "𝑖λ", "imaginary"]
draft = false
+++

## Summary {#summary}

Just a bit of fun to showcase the imaginary interpreter and universal syntax highlighting. I demo interacting with an AGI via imaginary python.


### Technical details {#technical-details}

This is a dockerized emacs running within
neovim. Emacs is communicating with itself via
a bash interop, that constructs a readline-
capable imaginary interpreter. emacs' `comint`
mode is used to provide structure and prompt highlighting.

Pen.el is used to provide autocompletion. The
interperter is imagined, and the python
libraries used are imagined too.


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/moaMT9uoQ18oEFR07QPY8jP5s" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/moaMT9uoQ18oEFR07QPY8jP5s.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/moaMT9uoQ18oEFR07QPY8jP5s.js" id="asciicast-moaMT9uoQ18oEFR07QPY8jP5s" async></script>