+++
title = "Ink.el (Intermediate Knowledge in emacs)"
author = ["Shane Mulligan"]
date = 2021-08-16T00:00:00+12:00
keywords = ["pen", "ink", "openai", "gpt", "emacs"]
draft = false
+++

| Install with Pen |                                                      |
|------------------|------------------------------------------------------|
| Pen.el on GitHub | <https://github.com/semiosis/pen.el/>                |
| Tutorial         | <https://mullikine.github.io/posts/pen-el-tutorial/> |
| Ink.el           | <https://github.com/semiosis/ink.el>                 |

Moire's blog on Quantifying Curation
: <https://generative.ink/posts/quantifying-curation/>


## Introducing Ink.el {#introducing-ink-dot-el}

{{< figure src="/ox-hugo/ink.png" >}}

The purpose of `Ink.el` is to encode within
its text properties information about its own
provenance. What was the intended meaning
behind parts of the text? Annotations made by
the writer are encoded in Ink. Where did the
text originate? Data of a LM's influence is
also included.

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


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/mLqcUaTCVADNF7Pkk238MGIvf" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mLqcUaTCVADNF7Pkk238MGIvf.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/mLqcUaTCVADNF7Pkk238MGIvf.js" id="asciicast-mLqcUaTCVADNF7Pkk238MGIvf" async></script>

You can see the text highlighted if it has
been generated, and the text contains metadata
on how it was generated. You can right click
the text to go to the prompt that generated
it, for example.

The file format `.ink` automatically generates
a _view_ buffer running `ink-source-mode`.
`ink-mode` and `ink-source-mode` work in
tandem and try to remain in sync with each
other when either is saved. Saving a `.ink`
file will record all of its metadata.


## Further explanations {#further-explanations}

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


## Another demo {#another-demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/TV11bEJ8bO80CYABYjmtZApBb" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/TV11bEJ8bO80CYABYjmtZApBb.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/TV11bEJ8bO80CYABYjmtZApBb.js" id="asciicast-TV11bEJ8bO80CYABYjmtZApBb" async></script>