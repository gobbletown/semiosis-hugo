+++
title = "Codex for anything: Imaginary + Real (with a bit of Codex) = Complex"
author = ["Shane Mulligan"]
date = 2021-08-25T00:00:00+12:00
keywords = ["codex", "gpt", "openai", "pen", "imaginary-programming", "ii"]
draft = false
+++

## Summary {#summary}

I add an imaginary dimension to the emacs
terminal.

Integrating real interpreters with imaginary
ones creates something new that is greater than
the sum of their parts; A _complex terminal_.

The complex terminal utilises the Codex LM to
perform its underlying prompt functions. A
complex terminal uses imaginary functions to
enable the user to see what may happen upon
running a command, to see what may follow
after writing some code, and see what people
usually do within a terminal context, no
matter how deeply nested within interpreters
or files.

Much more of this to come. I am trying to make
this as robust as possible and it's too exciting.


## Demos {#demos}


### Using Codex with vim and nano {#using-codex-with-vim-and-nano}

-   First start emacs
-   Then start the emacs terminal
-   As you can see I am autocompleting for vim, using emacs
-   Likewise you will see I can do the same for nano
-   Nano is still pretty hard to use haha
-   I could design as many prompt functions as I like for vim using this method
-   As you can see I could use context menus and create definitions for vim-related things
-   What else can I do?
-   Well, while browsing syslogs, I can generate commands to match things, for example
-   I could also rely on the autocompletion for EX from within vim

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/A1y3lQZyAnp9n8APxarsb8HT1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/A1y3lQZyAnp9n8APxarsb8HT1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/A1y3lQZyAnp9n8APxarsb8HT1.js" id="asciicast-A1y3lQZyAnp9n8APxarsb8HT1" async></script>


### What do people typically do in `/var/log`? {#what-do-people-typically-do-in-var-log}

-   I demonstrated showing what people would probably do in a given context.
-   I demonstrated using the glossary system to
    find out what a linux utility/log for a linux utility does.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/67PYiqKAHTzGWDuj0NNIu8YSu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/67PYiqKAHTzGWDuj0NNIu8YSu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/67PYiqKAHTzGWDuj0NNIu8YSu.js" id="asciicast-67PYiqKAHTzGWDuj0NNIu8YSu" async></script>


### Switching from bash to python {#switching-from-bash-to-python}

-   I will demonstrate using the same imaginary
    functions from one interpreter to the next.
-   It's also interesting that upon switching to
    a different interpreter such as python, Codex
    will suggest relevant Python libraries for the
    directory, so long as you listed out the
    contents or made it apparent.


### Switching from bash to vim and using Codex inside vim {#switching-from-bash-to-vim-and-using-codex-inside-vim}

-   It appears that codex runs commands from the EX REPL.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/52413ZGnS7T1tLKHgeBC2sPYg" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/52413ZGnS7T1tLKHgeBC2sPYg.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/52413ZGnS7T1tLKHgeBC2sPYg.js" id="asciicast-52413ZGnS7T1tLKHgeBC2sPYg" async></script>


### OK, what about automating emacs with Codex? {#ok-what-about-automating-emacs-with-codex}

-   I can't generate the emacs chrome from `emacs -nw -Q`, at least from an initial attempt
-   Well, I just did "Hello World". What's next?