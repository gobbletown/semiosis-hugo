+++
title = "Imaginary chimera languages with Codex"
author = ["Shane Mulligan"]
date = 2021-09-07T00:00:00+12:00
keywords = ["codex", "pen", "gpt", "imaginary-programming"]
draft = false
+++

## Summary {#summary}

I explore some imaginary chimera languages with Codex.


## scheme + bash {#scheme-plus-bash}

The expressions in this list are all imagined
by Codex. Codex thinks that this is a useful
language. A chimera language can be made with
any 2 languages or more languages.

{{< highlight text "linenos=table, linenostart=1" >}}
Language: chimera language based on scheme and bash.
File contents:
;; String replace is with isnt
(apply (sed s/is/isnt/) "My name is Shane")
--> "My name isnt Shane"

(split "\n" (ls))
--> '("chimera" "chimera.vim" "chimera.vimrc" "install.sh")

(sed 's/chimera/chameleon/' (split "\n" (ls)))
--> '("chameleon" "chameleon.vim" "chameleon.vimrc" "install.sh")

;; Get the first field in each string of list using sed
(mapcar (lambda (str) (apply (sed s/.* //) [str])) '("a b c" "d e f" "d e f"))
--> ("a" "d" "d")
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/wIcdDZP0iI36G8B09PcGEMO8t" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/wIcdDZP0iI36G8B09PcGEMO8t.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/wIcdDZP0iI36G8B09PcGEMO8t.js" id="asciicast-wIcdDZP0iI36G8B09PcGEMO8t" async></script>

<https://asciinema.org/a/wIcdDZP0iI36G8B09PcGEMO8t>

{{< figure src="./chimera-scheme-bash.gif" >}}


## Prompt engineering and imaginary chimera languages {#prompt-engineering-and-imaginary-chimera-languages}

When designing prompts for Codex `"""` is a
powerful way to delimit fields while
explaining to the LM your task via example.

With the Davinci model, `###` is preferred.
I'm unsure where `###` comes from but I have
some guesses as to why `"""` is a great
delimiter with Codex.

Python docstrings are make use of triple quote sometimes.

It is my best guess that the reason `"""` is
such an effective delimiter with prompt
engineering in Codex is that prompts employing
`"""` are effectively chimera languages that
are in part 'python docstring'.