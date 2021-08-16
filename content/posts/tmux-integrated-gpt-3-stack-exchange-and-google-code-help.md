+++
title = "tmux-integrated GPT-3, stack-exchange and google code help"
author = ["Shane Mulligan"]
date = 2021-05-01T00:00:00+12:00
keywords = ["tooling", "gpt", "emacs"]
draft = false
+++

## Summary {#summary}

When I have a question about code, I don't
want to forget about the multiple avenues for
looking for help.

I just make a convenience script to look for
help.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/6ejs3ZKmJCDT4VhsI65xfAPWu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/6ejs3ZKmJCDT4VhsI65xfAPWu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/6ejs3ZKmJCDT4VhsI65xfAPWu.js" id="asciicast-6ejs3ZKmJCDT4VhsI65xfAPWu" async></script>


## Make scripts {#make-scripts}

`tcq`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

lang="$1"
question="$@"

slug="$(printf -- "%s\n" "$@" | tr '\n' ' ' | sed 's/ $//' | slugify | cut -c -20)"

session_id="$(tm ss -n zsh -s cq -c ~ | cut -d : -f 1)"

pcq="$(tm -d -te nw -P -n "cq-$slug" -d -t "$session_id" -args cq "$@")"

tm -d -te nw -n "egr-$slug" -d -t "$session_id" -args egr "$@"
(
sleep 5
psx="$(tm -d -te nw -P -n "sx-$slug" -d -t "$session_id" -args sx "$@")"
tmux select-window -t "$psx"
) &
tmux select-window -t "$pcq"
TMUX= tmux attach -t "$session_id"
{{< /highlight >}}

`cq`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

topic="$1"
test -n "$topic" || exit 1
shift

task="$@"
test -n "$task" || exit 1

openai-complete code-snippet-from-natural-language.prompt "$topic" "$task"
{{< /highlight >}}

`sx`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

CMD="$(cmd "$@")"
: ${CMD:="$(cmd "$@")"}

query="$@"
sx-search-immediately "$query"
{{< /highlight >}}