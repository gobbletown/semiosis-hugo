+++
title = "How to use, with Codex"
author = ["Shane Mulligan"]
date = 2021-10-11T00:00:00+13:00
keywords = ["codex", "pen", "openai", "emacs"]
draft = false
+++

## Summary {#summary}

This is a prompt for obtaining examples of how
to use something, such as a function or an
import.


## Bindings {#bindings}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key pen-map (kbd "H-p e") 'pf-get-an-example-of-the-usage-of-a-function/2)
{{< /highlight >}}

| kb      | f                                                |           |
|---------|--------------------------------------------------|-----------|
| `H-p e` | `pf-get-an-example-of-the-usage-of-a-function/2` | `pen-map` |


## Prompt {#prompt}

`pf-get-an-example-of-the-usage-of-a-function/2`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get an example of the usage of a function"
doc: "Given a function name, provide an example of its usage"
prompt-version: 2
prompt: |+
  Examples of function usage:
  <delim>
  Language: "Haskell"
  Function: "map"
  Usage:<delim>
  map (*2) [1..10]
  <delim>
  Language: <q:language>
  Function: <q:function>
  Usage:<delim>

engine: OpenAI Codex
force-engine: OpenAI Codex
temperature: 0.5
max-generated-tokens: 80
n-completions: 8
top-p: 1
stop-sequences:
- "<delim>"
vars:
- function
- language
examples:
- map
completion: true
# This is just to make it a bit easier when you select multiple lines to define
preprocessors:
- pen-str onelineify
# Also trim off excess, when unonelineify doesn't quite do it
postprocessor: pen-str unonelineify | sed -z "s/Language:.*//"
# fz-pretty: on
var-defaults:
- "(pen-thing-at-point-ask \"symbol\" t)"
- "(pen-detect-language-ask)"
# validator: "grep -q <function>"
{{< /highlight >}}


## Question {#question}

How to use use this?

{{< highlight text "linenos=table, linenostart=1" >}}
import Control.Monad ( forM_ )
{{< /highlight >}}


### Prompting result {#prompting-result}

{{< highlight text "linenos=table, linenostart=1" >}}
import Control.Monad ( forM_ )
forM_ [1..10] $ \x -> do
    print x
    print x
{{< /highlight >}}


### Transformation with `pf-transform-code/3` {#transformation-with-pf-transform-code-3}

{{< highlight text "linenos=table, linenostart=1" >}}
remove the do and put on one line
{{< /highlight >}}


### Solution {#solution}

{{< highlight text "linenos=table, linenostart=1" >}}
*Main Control.Monad> forM_ [1..10] $ \x -> print x
1
2
3
4
5
6
7
8
9
10
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qqil5ZUjIDHRxQyaLDnigIq4k" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qqil5ZUjIDHRxQyaLDnigIq4k.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qqil5ZUjIDHRxQyaLDnigIq4k.js" id="asciicast-qqil5ZUjIDHRxQyaLDnigIq4k" async></script>