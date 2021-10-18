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


## See it in action {#see-it-in-action}

I provide some introspection into the LSP protocol stream.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qOxfj5RzSTp5e2JAKi46nDkbO" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qOxfj5RzSTp5e2JAKi46nDkbO.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qOxfj5RzSTp5e2JAKi46nDkbO.js" id="asciicast-qOxfj5RzSTp5e2JAKi46nDkbO" async></script>


### Caching requests {#caching-requests}

Documentation is instantaneous with caching enabled.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/dDH0uDr5jlgsvrdCBO7hAZYcJ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/dDH0uDr5jlgsvrdCBO7hAZYcJ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/dDH0uDr5jlgsvrdCBO7hAZYcJ.js" id="asciicast-dDH0uDr5jlgsvrdCBO7hAZYcJ" async></script>

Obviously, this means that a centralised cache
of truthful documentation is now a necessity.

But how do we have consensus on what is a high
quality suggestion/generation from a LM?

-   enter blockchain or Ministry of Truth (you decide)


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


## Implementing completion {#implementing-completion}


### Ensure pen can receive the POSITION {#ensure-pen-can-receive-the-position}

Then remove the parts of the text which are
not required, and forward the rest to the
prompt function.

{{< highlight bash "linenos=table, linenostart=1" >}}
stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

if stdin_exists; then
    # 0</dev/null env | tv &>/dev/null
    # cmd "$@" | tv &>/dev/null

    input_fp="$(cat | tf txt)"

    if test -n "$POSITION"; then
        line="$(echo "$POSITION" | cut -d : -f 1)"
        col="$(echo "$POSITION" | cut -d : -f 2)"

        this_line="$(( line + 1 ))"
        next_line="$(( line + 2 ))"

        sed -i "$next_line,\$d" "$input_fp"
        sed -i "${this_line}s/^\\(.\\{$col\\}\\).*\$/\\1/" "$input_fp"
    fi

    exec < <(cat "$input_fp")
fi
{{< /highlight >}}

{{< highlight yaml "linenos=table, linenostart=1" >}}
tools:
  pen-world-language-completion: &pen-world-language-completion
    # completion-command: 'ci penf pf-generic-completion-50-tokens/1'
    completion-command: 'POSITION=${POSITION} penf -u pf-generic-completion-50-tokens/1'
    completion-stdin: true
{{< /highlight >}}


### Study the racket language server {#study-the-racket-language-server}

-   see how output is formed

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

# raco pkg update --batch racket-langserver

if myrc-test ask_view_racket_lsp; then
    tm yn "view stdin?" && exec < <(tmicat)
    tm yn "view stdout?" && exec 1> >(tmicat)
elif myrc-test ask_log_racket_lsp; then
    tm yn "log stdin?" && {
        tf_in="$(ux tf in || echo /dev/null)"
        exec < <(tee "$tf_in")
        0</dev/null sps -E "$(cmd-nice v "$tf_in")"
    }
    tm yn "log stdout?" && {
        tf_out="$(ux tf out || echo /dev/null)"
        exec 1> >(tee "$tf_out")
        0</dev/null sps -E "$(cmd-nice v "$tf_out")"
    }
elif myrc-test ask_arbitrate_racket_lsp; then
    tm yn "arbitrate stdin?" && exec < <(tmi)
    tm yn "arbitrate stdout?" && exec 1> >(tmi)
fi

"racket" "--lib" "racket-langserver" "$@"
{{< /highlight >}}

_**examining racket langserver output**_

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/RiJqI0j6SYzc0JJXsWcIwl9d0" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/RiJqI0j6SYzc0JJXsWcIwl9d0.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/RiJqI0j6SYzc0JJXsWcIwl9d0.js" id="asciicast-RiJqI0j6SYzc0JJXsWcIwl9d0" async></script>

This `json` was sent by the client to the server:

{{< highlight json "linenos=table, linenostart=1" >}}
{
  "jsonrpc": "2.0",
  "method": "textDocument/completion",
  "params": {
    "textDocument": {
      "uri": "file:///home/shane/scripts/glob-grep.rkt"
    },
    "position": {
      "line": 3,
      "character": 4
    },
    "context": {
      "triggerKind": 1
    }
  },
  "id": 126
}
{{< /highlight >}}

The following json was returned by the server:

The `json` that comes back isn't really
the completions yet. I think they're what's
considered int the same scope as the
identifier being completed.

The original list was very long.

{{< highlight json "linenos=table, linenostart=1" >}}
{
  "jsonrpc": "2.0",
  "id": 10,
  "result": [
    {
      "label": "directory-exists?"
    },
    {
      "label": "nack-guard-evt"
    },
    {
      "label": "procedure->method"
    }
  ]
}
{{< /highlight >}}


## Generating `jq` {#generating-jq}

-   <https://mullikine.github.io/posts/codex-is-reversible-computing-exemplified/>

I decided that I wouldn't try to write the
`jq` for this myself, but rather rely on Codex
for it.


### I want to create something like the following {#i-want-to-create-something-like-the-following}

-   However, I don't have the `LOCATION_URI`.

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
export NAME="port->"
export KIND=13
export LOCATION_URI="file:///home/shane/scripts/glob-grep.rkt"
export LOCATION_RANGE_END_CHARACTER=8
export LOCATION_RANGE_END_LINE=3
export LOCATION_RANGE_START_CHARACTER=2
export LOCATION_RANGE_START_LINE=3
jq -n '{jsonrpc: "2.0", id: 2896, result: [{name: env.NAME, kind: env.KIND, location: {uri: env.LOCATION_URI, range: {end: {character: env.LOCATION_RANGE_END_CHARACTER, line: env.LOCATION_RANGE_END_LINE}, start: {character: env.LOCATION_RANGE_START_CHARACTER, line: env.LOCATION_RANGE_START_LINE}}}}]}'
{{< /highlight >}}


## `pen-lsp-complete` {#pen-lsp-complete}


### `shell` script {#shell-script}

-   <http://github.com/semiosis/pen.el/blob/master/scripts/pen-lsp-complete>


### `emacs lisp` {#emacs-lisp}

Because `efm-langserver` expects one
completion per line from the shell script is
uses (rather than json), I had to make a
workaround.

When I provide `efm-langserver` with
completion candidates, I `one-linerize` them by replacing "\n" with `<pen-newline>`.

Since `efm-langserver` is not providing the
appropriate `textEdit` to unonlinerize, I have
to do it myself in `company-lsp`.

Unfortunately, `company-lsp` uses lexical
scope, so I had to copy the entire file across
and rename it to add the functionality.

<https://github.com/semiosis/pen.el/blob/master/src/pen-company-lsp.el#L346>


## Final product {#final-product}


### Demo {#demo}

This is a demo of both documentation and completion using the Pen.el language server.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/aeYBHe2oWZ7bsZFARGEBGyVq3" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/aeYBHe2oWZ7bsZFARGEBGyVq3.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/aeYBHe2oWZ7bsZFARGEBGyVq3.js" id="asciicast-aeYBHe2oWZ7bsZFARGEBGyVq3" async></script>


### Built into the docker image {#built-into-the-docker-image}

EFM Langserver is also built into the Pen.el
docker image.