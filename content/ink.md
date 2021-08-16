+++
title = "Ink.el - Provenance for intelligibility"
author = ["Shane Mulligan"]
date = 2021-08-16T00:00:00+12:00
keywords = ["pen", "ink", "openai", "gpt", "emacs"]
draft = false
+++

Pen.el
: <https://github.com/semiosis/pen.el>

Ink.el
: <https://github.com/semiosis/ink.el>

prompts
: <https://github.com/semiosis/prompts>

engines
: <https://github.com/semiosis/engines>

Moire's blog on Quantifying Curation
: <https://generative.ink/posts/quantifying-curation/>


## Introducing Ink.el {#introducing-ink-dot-el}

The purpose of `Ink.el` is to encode within
its text properties information about its own
provenance.

It was created for `Pen.el`. It is generated
when you generate text with `Pen.el` in order
to encode how the text was generated.

`Ink.el` is used by `Pen.el` when
interpretation of the meaning of text is
required. Knowledge of which LM has generated
a given text snippet, the language of the text
(understood by said model) and the context
within that language are all important when
interpreting the meaning of that text, and it
is for that reason that tasks and metaprompts
may employ the `ink` format.

Quantifying curation is also important. After
using many different LMs to assist in creating a
document, the text may be analysed by reading
information contained in the `ink` format.

This is a text format that is designed for
easy editing via emacs, like normal text.

It is loaded and emacs text properties encode
the state of the buffer, plus functions for
regenerating text.

The prototype will be in emacs lisp using text properties.

-   This is a document format that contains and evaluates to the document as it is meant to be viewed.
-   It also contains annotations for the transformations, inputs and outputs of those transformations.

{{< figure src="/ox-hugo/pen-gehn.png" >}}


## Example {#example}

Here is some _ink_ text as the value for the
`task:` key in a yaml file.

The text contains information on the engine,
language and topic required to interpret the
meaning of the task.

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: '*("Translate Haskell to Clojure" 0 27 (engine "OpenAI Davinci" language "English" topic "programming"))'
{{< /highlight >}}


## Goals {#goals}

-   Encode state as well as metadata about what model generated the text.
-   Retain information on the completion functions used to generate the document so parts can be re-evaluated at will.
-   Become a file format for encoding meta-documents.
    -   A meta-document is like a template.
-   YASnippet compatibility (may be used as snippets).


## Example prompt {#example-prompt}

Here, the task is encoded in _Ink_.

If (in future) the prompt disappears, the task
will remain and it will be important to associate the task with
a LM so the user knows what the task means.

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: '*("Transpile from programming language X to Y" 0 41 (engine "OpenAI Davinci" language "English" topic "Programming"))'
prompt-version: 6
prompt: |+
    ###
    Haskell: zip (map show [1,5,9]) ["a","b","c"]
    Clojure: (println (map vector '(1 2 3) '(4 5 6)))
    ###
    Clojure: (clojure.string/upper-case "MiXeD cAsE")
    Haskell: map toUpper "MiXeD cAsE"
    ###
    <2>: <1>
    <3>:
engine: OpenAI Davinci
temperature: 0.3
max-tokens: 400
top-p: 1
stop-sequences:
- "###"
stop-patterns:
- "^[a-zA-Z]+:"
vars:
- code
- from language
- to language
preprocessors:
- pen-s onelineify
- cat
- cat
# Use s-preserve-trailing-whitespace within pen.el
# Chomp is needed because of stop-patterns
postprocessor: pen-s unonelineify | chomp
examples:
- min 1 2
- Haskell
- Clojure
var-defaults:
- "(pen-selected-text)"
- "(pen-detect-language-ask)"
- "(read-string-hist \"Pen to programming language: \")"
filter: on
{{< /highlight >}}