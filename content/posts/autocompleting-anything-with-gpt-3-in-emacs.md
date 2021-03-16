+++
title = "Autocompleting anything with GPT-3 in emacs"
author = ["Shane Mulligan"]
date = 2021-03-16T00:00:00+13:00
keywords = ["GPT-3", "openai", "emacs"]
draft = false
+++

Emacs package
: <http://github.com/mullikine/emacs/blob/master/config/pen.el>


Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/generic-file-type-completion.prompt>


## Summary {#summary}

I make a prompt for the OpenAI API which
completes given a file type and some preceding
text.

I then make a `company-mode` completion
function for it, and then demo its usage.

This gives me a generic completion mechanism
when dealing with any type of document.


## Demonstration {#demonstration}

This is GPT-3 completing some text for me.

I can type a few characters and then GPT-3
will complete the rest of the text.

These generations use the exact same prompt,
found with the code at the end of this
article.


### Generating `C++` {#generating-c-plus-plus}

I generate a class with particular methods by
writing the docstring first.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/iqVs8kv3PxiQkZyXASQURNSqX" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/iqVs8kv3PxiQkZyXASQURNSqX.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/iqVs8kv3PxiQkZyXASQURNSqX.js" id="asciicast-iqVs8kv3PxiQkZyXASQURNSqX" async></script>


### Generating `python` code with GPT-3 {#generating-python-code-with-gpt-3}

I generate a class with particular methods by
writing the docstring first.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/a5dDY1goh2kWkUQQpPPsGZ87q" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/a5dDY1goh2kWkUQQpPPsGZ87q.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/a5dDY1goh2kWkUQQpPPsGZ87q.js" id="asciicast-a5dDY1goh2kWkUQQpPPsGZ87q" async></script>


### Generate prose {#generate-prose}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/QpZSIuMPlwBQhP6hgr0qKrTh7" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/QpZSIuMPlwBQhP6hgr0qKrTh7.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/QpZSIuMPlwBQhP6hgr0qKrTh7.js" id="asciicast-QpZSIuMPlwBQhP6hgr0qKrTh7" async></script>

This demonstration has shown that I can
complete prose within an org file and complete
python code, using `GPT-3`.


## Commentary {#commentary}

I think being able to make your own autocompletion functions will come in very
handy.

When GPT-3 finetuning is made available or alternatives for fine-tuning or
training large language models are available, there will be also be the
landscape of specialised language models for writing or coding in different
languages or styles.

Being able to step into the **style** of programming or writing of your choosing,
it will become important to have a fully featured editing environment
(autocompletion, error checking, etc.). Emacs provides a framework for
reasoning about these environments and building new ones.

If a 'prompt hub' prompt curation server exists, the editing landscape will be
flooded with options on styles and languages to work within, along with editing
functions.


## Code {#code}


### `OpenAI` API prompt - `generic-file-type-completion.prompt` {#openai-api-prompt-generic-file-type-completion-dot-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "Generic file type completion"
doc: |-
    This is a generic completer for emacs.
prompt: |+
    File type: <1>
    Contents:
    <2>
engine: "davinci"
temperature: 0.3
max-tokens: 60
top-p: 1.0
# Not available yet: openai api completions.create --help
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "\n\n"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "file type"
- "preceding text"
examples:
- "python"
- ""
external: ""
conversation-mode: no
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
needs-work: no
{{< /highlight >}}


### elisp - `pen.el` {#elisp-pen-dot-el}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun company-pen-filetype--candidates (prefix)
  (let* ((preceding-text (str (buffer-substring (point) (max 1 (- (point) 1000)))))
         (response (pen-pf-generic-file-type-completion (detect-language) preceding-text))
         ;; Take only the first line for starters
         (line (car (str2lines response)))
         (res (str2list (snc "monotonically-increasing-tuple-permutations.py" line))))
    ;; Generate a list
    (mapcar (lambda (s) (concat (company-pen-filetype--prefix) s))
            res)))

(defun company-pen--grab-symbol ()
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                            (point))))

(defun company-pen-filetype--prefix ()
  "Grab prefix at point."
  (or (company-pen--grab-symbol)
      'stop))

(defun company-pen-filetype (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pen-filetype))
    (prefix (company-pen-filetype--prefix))
    (candidates (company-pen-filetype--candidates arg))
    ;; TODO doc-buffer may contain info on the completion in the future
    ;; (doc-buffer (company-pen-filetype--doc-buffer arg))
    ;; TODO annotation may contain the probability in the future
    ;; (annotation (company-pen-filetype--annotation arg))
    ))

(require 'company)
(defun my-completion-at-point ()
  (interactive)
  (if (>= (prefix-numeric-value current-prefix-arg) 4)
      (call-interactively 'company-pen-filetype)
    (call-interactively 'completion-at-point)))

(define-key global-map (kbd "M-~") #'my-completion-at-point)
{{< /highlight >}}


### python - `monotonically-increasing-tuple-permutations.py` {#python-monotonically-increasing-tuple-permutations-dot-py}

{{< highlight python "linenos=table, linenostart=1" >}}
#!/usr/bin/env python3.6

from itertools import combinations

import sys

for line in sys.stdin:
    ## I can't split this way or I'll lose the starting space in emacs GPT autocomplete
    #  lst = line.split()
    # I must split like this
    lst = line.split(' ')
    #  print(line)
    for w in lst:
        if w and not w.isspace() and not w == "\n":
            print(w)
        break

    for start, end in combinations(range(len(lst)), 2):
        if start == 1:
            break
        print(' '.join(lst[start:end+1]))
{{< /highlight >}}


### shell - `openai-complete` {#shell-openai-complete}

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null
set -xv

first_arg="$1"

stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

if stdin_exists; then
    # The stdin can be the first argument
    set -- "$@" "$(cat | chomp)"
fi

test -f "$first_arg" || exit
shift

prompt="$(cat "$first_arg" | yq -r ".prompt // empty")"
stop_sequence="$(cat "$first_arg" | yq ".\"stop-sequences\"[0] // empty" | uq | qne)"
temperature="$(cat "$first_arg" | yq -r ".\"temperature\" // empty")"
engine="$(cat "$first_arg" | yq -r ".\"engine\" // empty")"
max_tokens="$(cat "$first_arg" | yq -r ".\"max-tokens\" // empty")"
top_p="$(cat "$first_arg" | yq -r ".\"top-p\" // empty")"

test -n "$prompt" || exit 0

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
    "") { shift; }; ;;
    -e) {
        engine="$2"
        shift
        shift
    }
    ;;

    *) break;
esac; done

: "${engine:="ada"}"
: "${temperature:="0.6"}"
: "${max_tokens:="64"}"

: "${sub_completions:="1"}"

i=1
for var in "$@"
do
    var="$(printf -- "%s" "$var" | uq | chomp)"
    prompt="$(p "$prompt" | template -$i "$var")"
    ((i++))
done

prompt_fp="$(printf -- "%s" "$prompt" | chomp | tf)"

# printf -- "%s\n" "$prompt" | tv

# prompt="$(p "$prompt" | bs '$' | qne)"

# qne will break emojis
# emojis work again
prompt="$(p "$prompt" | bs '$`"' | sed -z 's/\n/\\n/g')"
# exit 1

IFS= read -r -d '' SHCODE <<HEREDOC
openai api \
    completions.create \
    -e "$engine" \
    -t "$temperature" \
    -M "$max_tokens" \
    -n "$sub_completions" \
    $(
        if test -n "$stop_sequence"; then
            printf -- "%s" "--stop \"$stop_sequence\""
        fi
    ) \
    -p "$prompt"
HEREDOC

# printf -- "%s\n" "$SHCODE" | tv
# exit 1

response_fp="$(eval "$SHCODE" | uq | s chomp | tf txt)"

prompt_bytes="$(cat "$prompt_fp" | wc -c)"
response_bytes="$(cat "$response_fp" | wc -c)"

# it may not be a good idea to remove starting whitespace if I want to complete in emacs
# tail -c +$((prompt_bytes + 1)) "$response_fp" | sed -z 's/^\s\+//;s/^\r\+//;s/^\n\+//'
# tail -c +$((prompt_bytes + 1)) "$response_fp" | sed -z 's/^\r\+//;s/^\n\+//'
# Don't remove whitespace at all
tail -c +$((prompt_bytes + 1)) "$response_fp"
{{< /highlight >}}

You could say that `company-mode`, which
stands for "complete-any[thing]" has reached
its apotheosis, but I still think it's only
the beginning.