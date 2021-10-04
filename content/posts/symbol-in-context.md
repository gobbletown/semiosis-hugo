+++
title = "Symbol in context"
author = ["Shane Mulligan"]
date = 2021-10-04T00:00:00+13:00
keywords = ["codex", "openai", "gpt"]
draft = false
+++

## Summary {#summary}

I make a couple of prompts for showing usages
of words and syntax forms for code.

-   External related: <https://wordincontext.com/en>


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/sotQB87kSGp8EpV3TAKyhVLwr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/sotQB87kSGp8EpV3TAKyhVLwr.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/sotQB87kSGp8EpV3TAKyhVLwr.js" id="asciicast-sotQB87kSGp8EpV3TAKyhVLwr" async></script>


### Word {#word}

`retribution`

{{< highlight text "linenos=table, linenostart=1" >}}
"Who knows if he will die for your
retributions?" said Conan.
{{< /highlight >}}

`rescinded`

{{< highlight text "linenos=table, linenostart=1" >}}
I was upset that the court rescinded the
judgement.
{{< /highlight >}}


### Function {#function}

`prolog` `format`

{{< highlight text "linenos=table, linenostart=1" >}}
format("~w~n", [1])
format("~w", [Hello, world!])
format("~d ~s~20|", [200, "bytes"])
format(\"~d~3| ~d~n\", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
format("~d~n", [123123123123123123123123123123123123123123123123123123123123123123])
format(atom(X), 'Hello, ~w', [X])
format('~d~t~d~5+~a~p~30+~n', [1,2,3])
format("~d~n", L).
{{< /highlight >}}


## Prompts {#prompts}


### `pf-get-an-example-sentence-for-a-word/1` {#pf-get-an-example-sentence-for-a-word-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get an example sentence for a word"
doc: "Given a word, provide an example of its usage"
prompt-version: 1
prompt: |+
  Glossary of terms with usage.
  <delim>
  impiety
  Usage: He blamed the fall of the city on the impiety of the people.
  <delim>
  <word>
  Usage:
engine: OpenAI Davinci
force-engine: OpenAI Davinci
temperature: 0.5
max-generated-tokens: 80
n-completions: 8
top-p: 1
stop-sequences:
- "<delim>"
vars:
- word
examples:
- boysenberry
completion: true
# This is just to make it a bit easier when you select multiple lines to define
preprocessors:
- pen-str onelineify
postprocessor: pen-str capveryfirst | sed 's/\s\+/ /g' | pen-str join ' ' | sed 's/^ *//'
fz-pretty: on
prettifier: pen-pretty-paragraph
var-defaults:
- "(pen-thing-at-point-ask \"word\" t)"
external-related:
- "https://wordincontext.com/en"
{{< /highlight >}}


### `pf-get-an-example-of-the-usage-of-a-function/2` {#pf-get-an-example-of-the-usage-of-a-function-2}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get an example of the usage of a function"
doc: "Given a function name, provide an example of its usage"
prompt-version: 1
prompt: |+
  Examples of function usage:
  <delim>
  Language: Haskell
  Function: map
  Usage: map (*2) [1..10]
  <delim>
  Language: <language>
  Function: <function>
  Usage:
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
postprocessor: pen-str unonelineify
# fz-pretty: on
var-defaults:
- "(pen-thing-at-point-ask \"symbol\" t)"
- "(pen-detect-language-ask)"
{{< /highlight >}}