+++
title = "Simply Complete to Completion the Selection"
author = ["Shane Mulligan"]
date = 2021-10-05T00:00:00+13:00
keywords = ["codex", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

This is not your stereotypical unbounded
completion mechanism. This prompt generates
until it considers the completion truly
complete (i.e. completed).

Your average completion will be unbounded, and
this simple prompt is a bounded completion.


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Prompt until the language model believes it has hit the end"
prompt-version: 1
todo:
- Consider including the EOD in the output as a sentinel, so the user knows
prompt: |+
  cat file <<EOD
  <:pp><contents>
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
stop-sequences:
- "EOD"
cache: on
vars:
- "contents"
var-defaults:
- "(or (sor (pen-selected-text t)) (pen-preceding-text))"
examples:
- "Knock knock. Who's there? "
filter: on
completion: off
insertion: off
{{< /highlight >}}


## Demo {#demo}

Here I demonstrate completing the current
selection until the LM considers it complete.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/WBHiRBjeMJCVpH3pZtP8B1NgS" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/WBHiRBjeMJCVpH3pZtP8B1NgS.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/WBHiRBjeMJCVpH3pZtP8B1NgS.js" id="asciicast-WBHiRBjeMJCVpH3pZtP8B1NgS" async></script>

{{< highlight prolog "linenos=table, linenostart=1" >}}
loop(X) :-
    X \= 15,
    writeln('X: ::', X),
    nl,
    X,
    writeln('X: ::', X),
    nl.
{{< /highlight >}}


## How it could be improved {#how-it-could-be-improved}

-   Include metadata about the current language, etc.