+++
title = "Generate new files with Codex and Pen.el"
author = ["Shane Mulligan"]
date = 2021-10-13T00:00:00+13:00
keywords = ["openai", "codex", "pen"]
draft = false
+++

## Summary {#summary}

I make a prompt for generating the contents of new files.

This takes into account the file name and file listing.

This allows you to quickly scaffold any project.

Simply touch the files you want. You can just
start by prompting a `.gitignore` file. That
will help!


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate the contents of a new file"
doc: "Prompt for the probable contents of a file with this path and sibling files"
prompt-version: 1
prompt: |+
    $ ls | head -n 10
    <ls output>

    $ cat <q:path> <<EOD
    <preceding text>
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
# This means that even a long-completion can't change the stop-sequence
force-stop-sequence: EOD
stop-sequences:
- EOD
cache: on
vars:
- "preceding text"
- "path"
- "ls output"
var-defaults:
- "(pen-preceding-text)"
- "(f-basename (get-path))"
- "(pen-snc \"ls | head -n 10\")"
examples:
- ""
- ".gitignore"
- ".\nLICENSE.md"
filter: off
completion: on
insertion: off
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/LV3TC93BkHRKh0RwfOoM45u83" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/LV3TC93BkHRKh0RwfOoM45u83.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/LV3TC93BkHRKh0RwfOoM45u83.js" id="asciicast-LV3TC93BkHRKh0RwfOoM45u83" async></script>