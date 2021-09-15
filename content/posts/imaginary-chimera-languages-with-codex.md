+++
title = "Imaginary chimeric languages with Codex"
author = ["Shane Mulligan"]
date = 2021-09-07T00:00:00+12:00
keywords = ["codex", "pen", "gpt", "imaginary-programming", "symbology"]
draft = false
+++

## Summary {#summary}

I explore some imaginary chimeric languages with Codex.

{{< highlight text "linenos=table, linenostart=1" >}}
The Chimera
Chimaera
    According to Greek mythology, was a
    monstrous fire-breathing hybrid creature
    of Lycia in Asia Minor, composed of the
    parts of more than one animal.

    It is usually depicted as a lion, with the
    head of a goat protruding from its back,
    and a tail that might end with a snake's
    head.

    The term "chimera" has come to describe
    any mythical or fictional creature with
    parts taken from various animals, to
    describe anything composed of very
    disparate parts, or perceived as wildly
    imaginative, implausible, or dazzling.
{{< /highlight >}}

{{< figure src="/ox-hugo/harappan-chimaera.jpg" >}}


### Definition of `chimeric` {#definition-of-chimeric}

A chimeric language in the context of
"imaginary programming" is one which employs
syntax from 2 or more languages.

`chimerism`, however, is exhibited in other
parts of GPT generations. For example,
generated URLs. Generated URLs are usually not
correct due to chimerism. However, it would be
interesting to generate _imaginary websites_ using
`chimeric URLs`.

Given the following prompt, Codex returned the
following definition:

{{< highlight text "linenos=table, linenostart=1" >}}
Glossary of terms.

ossified
Definition: Turn into bone or bony tissue.

chimeric in the context of imaginary-programming
Definition:
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
chimeric
    NLG: Program that is not in any way
    written in a language known to the
    programmer.
{{< /highlight >}}


## scheme + bash {#scheme-plus-bash}

The expressions in this list are all imagined
by Codex. Codex thinks that this is a useful
language. A chimeric language can be made with
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

{{< figure src="/ox-hugo/chimera-scheme-bash.gif" >}}


## Prompt engineering and imaginary chimeric languages {#prompt-engineering-and-imaginary-chimeric-languages}

When designing prompts for Codex `"""` is a
powerful way to delimit fields while
explaining to the LM your task via example.

With the Davinci model, `###` is preferred.
I'm more unsure of where `###` comes (markdown
inpart, perhaps), from but I have some guesses
as to why `"""` is a great delimiter with
Codex.

Python docstrings make use of triple quote.

It is my best guess that the reason `"""` is
such an effective delimiter with prompt
engineering in Codex is that prompts employing
`"""` are effectively chimera languages that
are in part 'python docstring' and inspiration
is taken from python docstrings to nudge GPT
in the right way for it to expect effective
delimitation.