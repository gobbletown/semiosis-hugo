+++
title = "nlsh upgrades"
author = ["Shane Mulligan"]
date = 2021-10-11T00:00:00+13:00
keywords = ["openai", "codex", "emacs"]
draft = false
+++

## Summary {#summary}

I just demonstrate my latest `nlsh` script.


## elisp {#elisp}

`ilist` from ilambda is used.

ilambda
: <http://github.com/semiosis/pen.el/blob/master/src/ilambda.el>

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun sps-nlsc (os)
  (interactive (list (pen-detect-language-ask)))
  (sps (cmd "eterm" "nlsc" os)))

(defun sps-nlsh (os)
  (interactive (list (fz (ilist 20 "distinctive linux distributions including nixos")
                         nil nil "sps-nlsh OS: ")))
  (sps (cmd "eterm" "nlsh" os)))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key pen-map (kbd "H-p c") 'sps-nlsc)
(define-key pen-map (kbd "H-p s") 'sps-nlsh)
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/lzN7GV37EKBhtB6BNf7A6UzHP" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/lzN7GV37EKBhtB6BNf7A6UzHP.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/lzN7GV37EKBhtB6BNf7A6UzHP.js" id="asciicast-lzN7GV37EKBhtB6BNf7A6UzHP" async></script>


## Generate either a command or a script {#generate-either-a-command-or-a-script}

`nlsh`
: Generate a command


`nlsc`
: Generate a script

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

sn="$(basename "$0")"

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
                                     "") { shift; }; ;;
                                     -norl) {
        norl=y
        shift
    }
    ;;

    *) break;
esac; done

if ! test "$norl" = "y"; then
    rlwrap "$sn" -norl "$@"
    exit $?
fi

# pf-code-snippet-from-natural-language/2
# pf-nlsh/2

case "$sn" in
    nlsh) { prompt=pf-nlsh/2; } ;;
    nlsc) { prompt=pf-code-snippet-from-natural-language/2; } ;;

    *)
esac

os_or_lang="$1"
shift

test -n "$os_or_lang" || {
    echo "Requires 1 argument: OS or lang" 1>&2
    exit 2
}

export USE_CONVERSATION_MODE=y

# rlwrap openai-complete.sh prompts/nlsh-2.prompt "$os_or_lang"

while IFS=$'\n' read -p "${os_or_lang}: " -r nlcommand; do
    if test "$nlcommand" = "!!"; then
        nlcommand="$lastcmd"
        UPDATE=y
    else
        UPDATE=
    fi

    export UPDATE

    pena "$prompt" "${os_or_lang}" "$nlcommand" | jq -r ".[]"

    lastcmd="$nlcommand"
done
{{< /highlight >}}