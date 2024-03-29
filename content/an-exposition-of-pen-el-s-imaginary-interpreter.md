+++
title = "ii (Imaginary Interpreter)"
author = ["Shane Mulligan"]
date = 2021-08-22T00:00:00+12:00
keywords = ["gpt", "pen", "openai"]
draft = false
+++

## Summary {#summary}

This is an introduction to the imaginary
interpreter built into `Pen.el`.

The reason I am building REPLs and emacs modes
based around imaginary interpreters (which are
not at all deterministic or logically sound)
is that I believe they will be very powerful
and useful in the future.

It's important to develop a language-agnostic
harness for working with arbitrarily many
imagined languages.

Solving the problems of utilising current LMs
will make it easier down the line to utilise
more advanced ones.


## Architecture {#architecture}


### The interpreter prompt {#the-interpreter-prompt}

-   An interpreter prompt is selected depending on if a specialised prompt exists for the chosen language.
    -   The generic `ii` prompts is comprised of:
        -   kickstarter: A subprompt declaring that the following are examples of the specified language.
        -   Consecutive Input/Output pairs.
    -   Language-specific `ii` prompts are comprised of:
        -   kickstarter: A subprompt which is typically a banner.
        -   postpostprocessor: A script that takes the interpreter history and generates a new "user prompt".
-   An interpreter prompt accepts as a first argument the history of the interaction with the interpreter.
    This way, the interpreter has an imaginary state as you work with it.


### `ii` script {#ii-script}

-   Accepts a language as an argument
-   Docker bash-host interop:
    `ii` communicates with arbitrary LMs via the
    bash (docker-host) interop of `Pen.el`.
-   `rlwrap` is used to keep a history of the inputs specific to each interpreter.
-   A `REPL` that keeps builds a transcript and passes that as input to the interpreter prompt.


## Code {#code}


### `ii` (Imaginary Interpreter for bash) {#ii--imaginary-interpreter-for-bash}

full code
: <http://github.com/semiosis/pen.el/blob/master/scripts/ii>

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash

# Imaginary interpreter
# Version 2.0, bash

#...

memory=

prompt_the_lm() {
    user_input="$1"

    if test -n "$fun"; then
        penf -u "$fun" "$memory" "$user_input"
    else
        penf -u pf-generic-interpreter/3 "$memory" "$user_input" "$lang"
    fi
}

p "${kickstarter} "
memory="${kickstarter} "

while read -e -r user_input; do
    gen="$(prompt_the_lm "$user_input" | chomp)"
    memory+="$user_input\n"
    p "$gen"
    memory+="$gen"
done

#...
{{< /highlight >}}

The imaginary interpreter may be implemented
for different languages, by using
`Pen.el`'s prompt functions via the bash-docker interop.


## An imaginary python interpreter {#an-imaginary-python-interpreter}

When a supported language such as Python is
chosen, a prompt specifically designed for
it may be used.

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-python-interpreter-2.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
include: Generic Interpreter/3
task: Imagine a <language> interpreter
language: python
subprompts:
- kickstarter: |+
    Python 3.8.5 (default, Jan 27 2021, 15:41:15)
    Type 'copyright', 'credits' or 'license' for more information
    IPython 7.21.0 -- An enhanced Interactive Python. Type '?' for help.

    In [1]:
prompt: |+
    <history><expression>
    <:pp>Out
user-prompt: "^In \\[[0-9]*\\]: "
# Unfortunately, we can't generate the next In
# prompt because we need to match on it with stop-sequences.
# So the user prompt must be reconstructed manually.
stop-sequences:
- "In ["
# Create a user prompt with the number incremented.
# This is like postprocessor but happens even later.
# It is used in special circumstances when the prompt history is also required.
postpostprocessor: pen-s python-gen-next-user-prompt
vars:
- history
- expression
var-defaults:
- kickstarter
examples:
- "In [1]: "
- "5 + 5"
{{< /highlight >}}


### Using the bash interop {#using-the-bash-interop}

`-p` ensures that the entire prompt along
with the generated output is returned.

`-u` ensures that the cache is updated and a
new generation is returned.


### Withholding the first argument {#withholding-the-first-argument}

By supplying an empty string as the first
argument, the history is not passed to the
interpreter prompt. Instead, the prompt
function will use the `kickstarter` subprompt,
as it has been supplied as the default value
for the first variable.

{{< highlight bash "linenos=table, linenostart=1" >}}
penf -p -u imagine-a-python-interpreter/2 "" "5 + 5"
{{< /highlight >}}

```bash
Python 3.8.5 (default, Jan 27 2021, 15:41:15)
Type 'copyright', 'credits' or 'license' for more information
IPython 7.21.0 -- An enhanced Interactive Python. Type '?' for help.

In [1]: 5 + 5
Out[1]: 10
```

{{< highlight bash "linenos=table, linenostart=1" >}}
penf -p -u imagine-a-python-interpreter/2 "In [3]: " "5 + 5"
{{< /highlight >}}

```bash
In [3]: 5 + 5
Out[3]: 10
```


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/PNjJsIwB5NVEE1LLqn2YWrein" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/PNjJsIwB5NVEE1LLqn2YWrein.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/PNjJsIwB5NVEE1LLqn2YWrein.js" id="asciicast-PNjJsIwB5NVEE1LLqn2YWrein" async></script>


## An imaginary interpreter with any language {#an-imaginary-interpreter-with-any-language}

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/generic-interpreter-3.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generic Interpreter"
prompt-version: 1
subprompts:
- kickstarter: |+
    <language> interpreter.

    Input:
prompt: |+
    <history><expression>
    <:pp>Output:
lm-command: "openai-complete.sh"
engine: "OpenAI Davinci"
temperature: 0.4
max-tokens: 60
top-p: 1.0
cache: on
stop-sequences:
- "Input: "
vars:
- history
- expression
- language
var-defaults:
- kickstarter
examples:
- "Input: "
- "5 + 5"
- Python
filter: no
completion: off
insertion: off
interpreter: on
conversation: on
external-related:
- "https://semiosis.github.io/posts/imaginary-programming-with-gpt-3/"
{{< /highlight >}}


### Demo of ruby using the generic interpreter {#demo-of-ruby-using-the-generic-interpreter}

This will use a generic prompt which does any
language, but the name of the language has
been specified as Rubylang.

When using `ii`, if a specialised interpreter
prompt for a given language can be found then
that will be used.

However, since no interpreter can be found for
'Rubylang', the generic interpreter is used.

However, the generic interpreter can still
imagine the 'Ruby' language to a degree, but
is far less accurate.

The beauty of this, of course, is that we
don't need to prime the interpreter with a
banner or terminal history, and we don't need
to know what the prompt for Ruby looks like.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/LkGZQ2pBIQ4V9nUbokwqnaDr1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/LkGZQ2pBIQ4V9nUbokwqnaDr1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/LkGZQ2pBIQ4V9nUbokwqnaDr1.js" id="asciicast-LkGZQ2pBIQ4V9nUbokwqnaDr1" async></script>


### And a demo of a more catered imaginary interpreter {#and-a-demo-of-a-more-catered-imaginary-interpreter}

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-ruby-interpreter-2.prompt>

This prompt requires additional features. `WIP`.


## Support scripts {#support-scripts}

These are used as post-processors in the prompt and in `ii`.


### String utilities (`pen-s`) {#string-utilities--pen-s}

{{< highlight bash "linenos=table, linenostart=1" >}}
remove-trailing-whitespace) {
    sed -z -e "s/\s*\$//" -e "s/\n*\$//"
}
;;

generic-interpreter-tidy-result) {
    pen-s remove-trailing-whitespace | { awk 1; echo -n "Input: "; }
}
;;

ruby-gen-next-user-prompt) {
    IFS= read -rd '' in < <(cat);typeset -p in &>/dev/null

    prompt_number="$(printf -- "%s" "$in" | awk 1 | tac | sed -n "/^2.7.0 :/{s/^2.7.0 :\\([0-9]*\\) .*/\\1/p;q}")"
    echo "$prompt_number"
    new_prompt_number="$(printf "%03d" $(( prompt_number + 1 )))"

    printf -- "%s" "$in" | awk 1
    echo -n "2.7.0 :$new_prompt_number > "
}
;;

python-gen-next-user-prompt) {
    IFS= read -rd '' in < <(cat);typeset -p in &>/dev/null

    prompt_number="$(printf -- "%s" "$in" | awk 1 | tac | sed -n "/^Out\\[/{s/^Out\\[\\([0-9]*\\)\\]:.*/\\1/p;q}")"
    new_prompt_number="$(( prompt_number + 1 ))"

    printf -- "%s" "$in" | awk 1
    echo -n "In [$new_prompt_number]: "
}
;;
{{< /highlight >}}