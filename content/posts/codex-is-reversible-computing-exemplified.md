+++
title = "Codex is reversible computing exemplified"
author = ["Shane Mulligan"]
date = 2021-10-17T00:00:00+13:00
keywords = ["openai", "codex"]
draft = false
+++

## Summary {#summary}

I demonstrate how Codex is reversible computing.


## Janus {#janus}

-   <https://en.wikipedia.org/wiki/Janus%5F(time-reversible%5Fcomputing%5Fprogramming%5Flanguage)>

Janus is a reversible programming language,
i.e. it supports deterministic forward and
backward computation by local inversion.

A language like Janus for reversible non-deterministic inference is sorely needed.


## Codex {#codex}

Codex is non-deterministic forward and
backward inference.


### Example, `json` <-> `jq` {#example-json-jq}

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-jq-from-json-1.prompt>

`pf-generate-jq-from-json/1`

{{< highlight json "linenos=table, linenostart=1" >}}
task: "Generate jq from json"
doc: "Given some json, generate the jq which generates it"
prompt-version: 1
prompt: |+
    cat generated-1.json <<EOD
    {
      "jsonrpc": "John Doe",
      "age": 42
    }
    EOD

    # This is how generated-1.json was generated:
    jq -n '{jsonrpc: "John Doe", age: 42}' > generated-1.json

    cat generated-2.json <<EOD
    <json>
    EOD

    # This is how generated-2.json was generated:
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
stop-sequences:
- " > gen"
cache: on
vars:
- "json"
examples:
- |-
    {
      "jsonrpc": "2.0",
      "id": 2896,
      "result": [
        {
          "name": "port->",
          "kind": 13,
          "location": {
            "uri": "file:///home/shane/scripts/glob-grep.rkt",
            "range": {
              "end": {
                "character": 8,
                "line": 3
              },
              "start": {
                "character": 2,
                "line": 3
              }
            }
          }
        },
      ]
    }
info: on
completion: off
insertion: off
{{< /highlight >}}

From the example `json` above, the example `json` below was generated.

{{< highlight bash "linenos=table, linenostart=1" >}}
jq -n '{jsonrpc: "2.0", id: 2896, result: [{name: "port->", kind: 13, location: {uri: "file:///home/shane/scripts/glob-grep.rkt", range: {end: {character: 8, line: 3}, start: {character: 2, line: 3}}}}]}' | v
{{< /highlight >}}

{{< highlight json "linenos=table, linenostart=1" >}}
{
  "jsonrpc": "2.0",
  "id": 2896,
  "result": [
    {
      "name": "port->",
      "kind": 13,
      "location": {
        "uri": "file:///home/shane/scripts/glob-grep.rkt",
        "range": {
          "end": {
            "character": 8,
            "line": 3
          },
          "start": {
            "character": 2,
            "line": 3
          }
        }
      }
    }
  ]
}
{{< /highlight >}}


## Demo of above {#demo-of-above}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/JzeWyo2hOyKtdDqgfzpAXz0G9" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/JzeWyo2hOyKtdDqgfzpAXz0G9.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/JzeWyo2hOyKtdDqgfzpAXz0G9.js" id="asciicast-JzeWyo2hOyKtdDqgfzpAXz0G9" async></script>