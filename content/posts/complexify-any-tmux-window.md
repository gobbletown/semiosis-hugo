+++
title = "Complexify any tmux window"
author = ["Shane Mulligan"]
date = 2021-10-07T00:00:00+13:00
keywords = ["tmux", "codex", "emacs", "imaginary-programming", "cterm"]
draft = false
+++

## Summary {#summary}

I create a tmux binding for opening the
current tmux window in a `Pen.el` enabled
`eterm`, thus allowing for codex completion
within the interpreter you are looking at.

I figure it is better to work with the complex
terminal in this manner, rather than doing
**everything** inside the emacs terminal.

This makes it more robust.

{{< highlight text "linenos=table, linenostart=1" >}}
complex programming
    A complex terminal has braided imaginary
    programming with real programming.

    The word complex is suitable because it
    describes this braiding along with the
    mathematical connotations of complex
    numbers.

    We could further extend this terminology
    to include complex programming in the
    realm of imaginary programming to describe
    a programming style that combines both
    real programming with imaginary
    programming.
{{< /highlight >}}

IP Glossary
: <https://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>


## Procedure {#procedure}


### Add a binding {#add-a-binding}

{{< highlight conf-space "linenos=table, linenostart=1" >}}
bind Q run -b "tm -d -t sps \"sh-pane \$(tpn)\""
{{< /highlight >}}


### Add a handler for my `sh-pane` script {#add-a-handler-for-my-sh-pane-script}

{{< highlight text "linenos=table, linenostart=1" >}}
e  tm-window-etermify $window_id
{{< /highlight >}}

`sh-pane`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

: "${pane_id:="$1"}"
: "${pane_id:="$TMUX_PANE"}"
: "${pane_id:="$CALLER_TARGET"}"

: "${window_id:="$1"}"
: "${window_id:=$(tmux display-message -p '#{window_id}')}"

# for i in sh-*; do sed -i 's/read -n1 d/&; test "$d" = "" \&\& read -n1 d/' "$i" ; done

clear
# exec 1> >(mnm)

# I should turn this entire script into an elisp hydra.
# Although that would require that emacs has been started already.

read -r -d '' options <<HEREDOC
.  EDIT
o  tm-pane-open-file $pane_id
i  tm-copy-pane-cmd $pane_id
m  tm-pane-open-file $pane_id media
F  tm-pane-open-file $pane_id feh
V  tm-pane-open-file $pane_id vlc
e  tm-window-etermify $window_id
HEREDOC

echo "$0" | udl
echo "$options" | mnm

IFS="" read -n1 d; test "$d" = "" && read -n1 d
# clear

case "$d" in
    .) cmd="v $0" ;;
    o) cmd="tm-pane-open-file $pane_id" ;;
    i) cmd="tm-copy-pane-cmd $pane_id" ;;
    m) cmd="tm-pane-open-file $pane_id media" ;;
    F) cmd="tm-pane-open-file $pane_id feh" ;;
    V) cmd="tm-pane-open-file $pane_id vlc" ;;
    e) cmd="tm-window-etermify $window_id" ;;
esac
echo
echo "$cmd"

# exec <`tm-tty`
# exec 1> /dev/tty
eval "$cmd"
{{< /highlight >}}

`tm-window-etermify`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

: "${window_id:="$1"}"
: "${window_id:=$(tmux display-message -p '#{window_id}')}"

# This should

# - firstly need a command which wraps a pane in a tmux inside a new window?
# Yeah, I probably need to do that.
# - do it inside a new tmux session? I need to, I think.
# - but I still have to rely on linking the pane. I can't link a pane with tmux, sadly.
# - I have to link the entire window

TMUX= tmux new-window "eterm tmux-attach-window $window_id"
{{< /highlight >}}

My humble eterm script will run whatever
command proceeds inside of an emacs eterm.


## Demo {#demo}

Wow, my Haskell is awful, but this is the
idea. You don't know what you are doing inside
of a terminal, so you wrap it inside of a
terminal that can perform LM operations on it (a complex terminal).

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/btDEL1F7gHAxvvSGhysBTLwJZ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/btDEL1F7gHAxvvSGhysBTLwJZ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/btDEL1F7gHAxvvSGhysBTLwJZ.js" id="asciicast-btDEL1F7gHAxvvSGhysBTLwJZ" async></script>