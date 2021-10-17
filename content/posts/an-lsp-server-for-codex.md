+++
title = "An LSP server for Codex and any language model"
author = ["Shane Mulligan"]
date = 2021-10-17T00:00:00+13:00
keywords = ["openai", "codex", "pen", "emacs"]
draft = false
+++

## Summary {#summary}

I set up an LSP server for OpenAI's
GPT-3/Codex or any LM via your favourite NLP
service provider.

First LSP server for LMs in the world, as far
as I know.

It gives documentation and refactoring to any
language including world languages and
fictional ones. As an LSP server, it works for
VSCode too.

The LSP server can create hover documentation,
code actions and linting services for any
programming language, world language, fictional
language or computing context.


## How does it work? {#how-does-it-work}

`Pen.el` utilises the `efm-langserver` along with its shell-interop.

Essentially, you design documentation and refactoring functions (prompt-functions) like so.

<https://semiosis.github.io/posts/pen-el-host-interop-and-client-server/>


### efm-langserver {#efm-langserver}

Then you configure EFM langserver like so:

<https://github.com/mattn/efm-langserver>


### EFM Config {#efm-config}

{{< highlight yaml "linenos=table, linenostart=1" >}}
glossary1: &glossary1
  hover-command: '"penf" "pf-define-word-for-glossary/1"'
  hover-stdin: true
{{< /highlight >}}


### emacs lisp {#emacs-lisp}

The LSP client communicates with the LSP server.

-   <http://github.com/semiosis/pen.el/blob/master/src/pen-lsp-client.el>


## Demo {#demo}

Hovering over "The Anatomy of a Monad".

{{< figure src="/ox-hugo/anatomy-of-monad.png" >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Notebook
The
The definite article used before a noun.

Anatomy
The branch of biology concerned with the study of the structure of organisms and their parts.

of
Indicating possession.

a
Used to form the plural of most nouns.

Monad
The smallest unit of matter that exists.
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qCTVSRGZgUZruwuiW1JVaNI6t.js" id="asciicast-qCTVSRGZgUZruwuiW1JVaNI6t" async></script>


## Final product {#final-product}

EFM Langserver is also built into the Pen.el docker image.