+++
title = "An LSP server for Codex and any language model"
author = ["Shane Mulligan"]
date = 2021-10-17T00:00:00+13:00
draft = false
+++

## Summary {#summary}

I set up an LSP server for OpenAI's Codex.

The LSP server can create hover documentation,
code actions and linting services for any
programming language, word language, fictional
language or computing context.


### Code {#code}

-   <http://github.com/semiosis/pen.el/blob/master/src/pen-lsp-client.el>


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t.js" id="asciicast-qCTVSRGZgUZruwuiW1JVaNI6t" async></script>


## EFM Config {#efm-config}

{{< highlight yaml "linenos=table, linenostart=1" >}}
glossary1: &glossary1
  hover-command: '"penf" "pf-define-word-for-glossary/1"'
  hover-stdin: true
{{< /highlight >}}