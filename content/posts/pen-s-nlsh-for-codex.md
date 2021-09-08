+++
title = "Pen's nlsh for Codex - Stepping towards an imaginary OS"
author = ["Shane Mulligan"]
date = 2021-09-08T00:00:00+12:00
keywords = ["openai", "pen", "codex"]
draft = false
+++

## Summary {#summary}

Codex is much more powerful, and so is Pen.el.
This demo of `nlsh` gives me the chills.

Under the hood, Pen.el is coded using robust
imaginary programming and lazy functions.

An imaginary operating system is being created
where most functions are inferred.

It's still very important to build tools to
normalise imaginary code and languages onto
the real axis.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/xaMBknFUIDMRJLDANeFT8Fkce" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/xaMBknFUIDMRJLDANeFT8Fkce.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/xaMBknFUIDMRJLDANeFT8Fkce.js" id="asciicast-xaMBknFUIDMRJLDANeFT8Fkce" async></script>


## Source {#source}

A combination of lazy prompting with real
emacs lisp, a dockerized Pen.el server, makes
for a great imaginary OS.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun nlsh-os (os)
  (interactive (list (fz
                      (append
                       '(;;  There has been a name change
                         ;; That's why this is giving bad results
                         ;; "GNU Guix System"
                         "GuixSD"
                         "Alpine Linux"
                         "RHEL Red Hat Enterprise Linux"
                         "Amazon Linux 2"
                         "NixOS"
                         "macOS"
                         "Ubuntu 20.04"
                         "Arch Linux")
                       (pen-ci (pen-one (pf-list-of/2 10 "operating systems with a command line"))))
                      nil nil "nlsh-os: ")))
  (comint-quick (cmd "nlsh" os) pen-prompts-directory))
{{< /highlight >}}