+++
title = "Describe Haskell types with Codex"
author = ["Shane Mulligan"]
date = 2021-10-08T00:00:00+13:00
keywords = ["codex", "openai", "haskell"]
draft = false
+++

## Summary {#summary}

I am building an LSP server for Pen.el and I
would like to intercept and provide a NL translation for LSP
provisions, such as function signatures by the
hover facility.


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Describe a Haskell function signature"
doc: "Given a Haskell function signature, describe it"
prompt-version: 1
prompt: |+
  Haskell type explanations:
  <delim>
  Signature: String -> Maybe [String]
  Description: This function takes a String and Maybe will return a list of Strings.
  <delim>
  Signature: String -> IO String
  Description: This function takes a String and performs some I/O before returning a String.
  <delim>
  Signature: <signature>
  Description:
engine: "OpenAI Codex"
temperature: 0.2
max-generated-tokens: 100
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "signature"
examples:
- "FilePath -> [String] -> String -> IO String"
info: on
filter: off
completion: off
insertion: off
{{< /highlight >}}


## Example {#example}

`signature`

{{< highlight text "linenos=table, linenostart=1" >}}
FilePath -> [String] -> String -> IO String
{{< /highlight >}}

`natural language`

{{< highlight text "linenos=table, linenostart=1" >}}
This function takes a FilePath, a list of
Strings and a String and performs some I/O
before returning a String.
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/lnuKRT7GI6JUyc2bcnLGcsUxU" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/lnuKRT7GI6JUyc2bcnLGcsUxU.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/lnuKRT7GI6JUyc2bcnLGcsUxU.js" id="asciicast-lnuKRT7GI6JUyc2bcnLGcsUxU" async></script>