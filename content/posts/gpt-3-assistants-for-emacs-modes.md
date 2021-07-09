+++
title = "GPT-3 assistants for emacs modes"
author = ["Shane Mulligan"]
date = 2021-06-02T00:00:00+12:00
keywords = ["gpt", "emacs"]
draft = false
+++

## Summary {#summary}

I'm looking for places to integrate GPT-3 into emacs.

Context menus for refactoring stuff is good.

I'd also like to make some personal
assistants, so I can do things like select
error messages and ask what they mean.

Please see my article on `Pen` (Prompt Engineering in emacs) [Pen // Bodacious Blog](https://mullikine.github.io/posts/pen/).

Please email me if you would like to help: <mailto:mullikine@gmail.com>.

Pen facilitates the creation, development,
discovery and usage of prompts to a LM such as
GPT-3 and GPT-j.

`pen.el` code
: <https://github.com/semiosis/pen.el>


## `shell` {#shell}

This is what I want to integrate into emacs.


### `asktutor` {#asktutor}

Code
: <https://github.com/semiosis/pen.el/blob/master/scripts/openai-complete.sh>

<!--listend-->

{{< highlight sh "linenos=table, linenostart=1" >}}
asktutor haskell ghc "What does could not deduce mean?"
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

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

{{< highlight sh "linenos=table, linenostart=1" >}}
cq haskell could not deduce from the context
{{< /highlight >}}


## Converting above into elisp together with a prompt function {#converting-above-into-elisp-together-with-a-prompt-function}

`pen-pf-asktutor` is generated from a `.prompt` file.


### `pen-tutor-mode-assist` {#pen-tutor-mode-assist}

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


### `right-click-context-click-menu` {#right-click-context-click-menu}

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

{{< figure src="./tutor-code.png" >}}

`tutor.prompt`

Tutor prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/tutor.prompt>


Tutor elisp code
: <https://github.com/semiosis/pen.el/blob/master/pen-brain.el#L98>