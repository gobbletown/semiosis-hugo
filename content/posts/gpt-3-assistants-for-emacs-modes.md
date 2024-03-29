+++
title = "GPT-3 assistants for emacs modes"
author = ["Shane Mulligan"]
date = 2021-06-02T00:00:00+12:00
keywords = ["gpt", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

In this article I will show how I transition
from using shell script to emacs lisp with my
'any topic' tutor in emacs lisp.

I am working on integrating GPT-3, GPT-j and
more GPT completion engines into emacs, and
connecting more and more emacs packages to
GPT-3.

{{< highlight sh "linenos=table, linenostart=1" >}}
# Latest docker image of =pen.el=
docker run --rm -ti --entrypoint= semiosis/pen.el:latest ./run.sh
{{< /highlight >}}

Here is an assistant for any major mode / context.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b.js" id="asciicast-MS8xMQfLoExEVyh4Nqu9fX49b" async></script>

Some ideas I have:

-   Correct spelling and grammar in conversations over `erc` and other `emacs` chat modes.
-   Select error messages and ask what they mean.

What I've already built:

-   Context menus for selecting text and running prompt functions on that text
    -   Translating
    -   Context-aware functions
    -   Natural language shell for any OS.
    -   Explain shell code.
    -   Get a code snippet.
    -   Get a code one-liner.
    -   Generate code from comments.
    -   Automatically add comments to code
    -   Ask GPT-3, "What is the word for ..."
    -   Abstractive summarization of text within emacs.
    -   Subtopic generation for mind maps
    -   Tutor for any topic
        -   e.g. JavaScript. Ask it which version of node was available in 2018.
    -   Generate lists of things


### Introducing `pen.el` (Prompt Engineering in emacs) {#introducing-pen-dot-el--prompt-engineering-in-emacs}

Please see my article on `Pen`  [Pen // Bodacious Blog](https://mullikine.github.io/posts/pen/).

Pen facilitates the creation, development,
discovery and usage of prompts to a LM such as
GPT-3 and GPT-j.

And please email me if you would like to help: <mailto:mullikine@gmail.com>.

Pen Project on GitHub
: <https://github.com/semiosis/pen.el>


## Original `shell` script, `asktutor` {#original-shell-script-asktutor}

`asktutor` queries the OpenAI API via the python library (`pip install openai`).

This is how it is called:

{{< highlight sh "linenos=table, linenostart=1" >}}
asktutor haskell ghc "What does could not deduce mean?"
{{< /highlight >}}

Here is its code:

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash

topic="$1"
test -n "$topic" || exit 1
shift

in_the_context_of="$1"
test -n "$in_the_context_of" || exit 1
shift

question="$@"
test -n "$question" || exit 1

if ! pl "$question" | grep -q -P ".*\\?"; then
    question="${question}?"
fi

oci openai-complete $MYGIT/semiosis/prompts/prompts/tutor.prompt "$topic" "$in_the_context_of" "$question" | pavs
{{< /highlight >}}


### Converting above into elisp together with a prompt function {#converting-above-into-elisp-together-with-a-prompt-function}

`pen-pf-asktutor` is generated from a `.prompt` file (<http://github.com/semiosis/prompts/blob/master/prompts/generic-tutor-for-any-topic-and-subtopic.prompt>).

Here is the generation function, `pen-generate-prompt-functions`: <https://github.com/semiosis/pen.el/blob/master/pen.el#L131>

Now we can use `pen-pf-asktutor` inside `org-
brain`. We supply the parent node as the
context to the function.

Org-brain tutor code
: <https://github.com/semiosis/pen.el/blob/master/pen-brain.el#L98>

The prompt function calls a simple shell script (`openai-complete.sh`) which sends the final templated prompt to the OpenAI API.

This can be substituted for other completers such as GPT-j.

`openai-complete.sh`
: <https://github.com/semiosis/pen.el/blob/master/scripts/openai-complete.sh>

`pen-tutor-mode-assist` is another example of using the same prompt function.

`M-x pen-tutor-mode-assist`, enter your query and have the answer displayed in a new buffer.

Code
: <http://github.com/semiosis/pen.el/blob/master/pen-contrib.el>

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-tutor-mode-assist (query)
  (interactive (let* ((bl (buffer-language t t)))
                 (list
                  (read-string-hist
                   (concat "asktutor (" bl "): ")
                   (my/thing-at-point)))))
  (pen-pf-asktutor bl bl query))
{{< /highlight >}}


## Original `shell` script, `cq` {#original-shell-script-cq}

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

topic="$1"
test -n "$topic" || exit 1
shift

task="$@"
test -n "$task" || exit 1

openai-complete code-snippet.prompt "$topic" "$task"
{{< /highlight >}}


### Converting above into elisp together with a prompt function {#converting-above-into-elisp-together-with-a-prompt-function}

`pen-pf-cq` is generated from a `.prompt` file (<http://github.com/semiosis/prompts/blob/master/prompts/code-snippet-from-natural-language.prompt>).

Documentation for generated function:

{{< highlight text "linenos=table, linenostart=1" >}}
pen-pf-cq is an alias for ‘pen-pf-code-snippet-from-natural-language’
in ‘pen-example-config.el’.

(pen-pf-cq LANGUAGE TASK)

code snippet from natural language
Get a bash one liner from natural langauge
future-titles:- Get code snippet
- Get snippet
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/foNqrgSZLJcDPDsaqanffOJSY" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/foNqrgSZLJcDPDsaqanffOJSY.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/foNqrgSZLJcDPDsaqanffOJSY.js" id="asciicast-foNqrgSZLJcDPDsaqanffOJSY" async></script>


## `right-click-context-click-menu` {#right-click-context-click-menu}

This is an example of connecting a prompt function to `right-click-context-menu`.

GPT language models are capable of classification as well as generation.

Classification is just a downstream task of generation, it seems.

Right click menu code
: <https://github.com/semiosis/pen.el/blob/master/pen-right-click-menu.el#L126>

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
("GPT-3: Convert Haskell to Clojure"
 :call pen-pf-translate-haskell-to-clojure
 :if (gpt-test-haskell))
("pen (code)"
 ("asktutor"
  :call pen-tutor-mode-assist
  :if (major-mode-p 'prog-mode)))
{{< /highlight >}}


## Tutor `GPT-3` prompt in `yaml` {#tutor-gpt-3-prompt-in-yaml}

{{< figure src="/ox-hugo/tutor-code.png" >}}