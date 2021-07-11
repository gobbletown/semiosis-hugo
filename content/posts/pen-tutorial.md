+++
title = "Pen Tutorial"
author = ["Shane Mulligan"]
date = 2021-07-11T00:00:00+12:00
keywords = ["gpt", "emacs"]
draft = false
+++

## Summary {#summary}

This is a tutorial for how to use `pen.el`.


## Default Key bindings {#default-key-bindings}

| kb        | f                               |                |
|-----------|---------------------------------|----------------|
| `H-TAB g` | `pen-generate-prompt-functions` | `pen-map`      |
| `H-TAB r` | `pen-run-prompt-function`       | `pen-map`      |
| `SPC`     | `pen-run-prompt-function`       | `selected-map` |

`H` is the Hyper key, which works similar to Escape, Meta, Alt, Control or Shift.

`pen.el` emulates a Hyper key with `C-M-\`.


## Regenerate key bindings {#regenerate-key-bindings}

Running `pen-generate-prompt-functions` will
load all prompts from the prompts directory,
which is typically located here: `~/.emacs.d/prompts`.

Running `pen-run-prompt-function` will run a prompt function.

You may also press `SPC` while some text is selected to run a prompt function.


## Demos {#demos}


### Select some text and running a prompt function {#select-some-text-and-running-a-prompt-function}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/XrtPxWfh0yhJRdMXpnMnm8i70" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/XrtPxWfh0yhJRdMXpnMnm8i70.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/XrtPxWfh0yhJRdMXpnMnm8i70.js" id="asciicast-XrtPxWfh0yhJRdMXpnMnm8i70" async></script>


### Run a prompt function like an M-x interactive command {#run-a-prompt-function-like-an-m-x-interactive-command}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/mVe7Ujx7urB1nyPdiEuqGUcb4" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mVe7Ujx7urB1nyPdiEuqGUcb4.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/mVe7Ujx7urB1nyPdiEuqGUcb4.js" id="asciicast-mVe7Ujx7urB1nyPdiEuqGUcb4" async></script>


## An exhibition of a `.prompt` {#an-exhibition-of-a-dot-prompt}

Prompt file
: [prompts/get-language.prompt at master  semiosis/prompts  GitHub](http://github.com/semiosis/prompts/blob/master/prompts/get-language.prompt)

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: Get language
prompt-version: 1
doc: This prompt detects the language
notes:
- "It appears that combining ### with Input: Output: has no improvement"
prompt: |+
    Given some text, return the language.

    Input: Hello
    Output: English
    Input: Bon anniversaire !
    Output: French
    Input: printf -- "%s\n" "$lang"
    Output: bash
    Input: Zdravstvuyte
    Output: Russian
    Input: <1>
    Output:
engine: davinci
temperature: 0.3
max-tokens: 200
top-p: 1
stop-sequences:
- "\n"
vars:
- text-or-code
examples:
- Happy birthday
preprocessors:
- "sed -z 's/\\n/\\\\n/g'"
aliases:
- detect-language
{{< /highlight >}}

This is a prompt which, given text selected
will output the language that text is in.

It works for both world languages and for code.

The `title` of the prompt will be
[slugified](https://pypi.org/project/python-%20slugify/) and used as the name of
the prompt function.

`doc` and `notes` will both go into the
documentation for the function.

The prompt is using the `Input` `Output`
pattern.

`engine` is the name of a language model.

An API such as the `OpenAI API` (`GPT-3`) may serve
several different models.

-   Some alternative models for `GPT-3`:
    -   babbage
    -   content-filter-alpha-c4
    -   content-filter-dev
    -   curie
    -   cursing-filter-v6
    -   davinci
    -   instruct-curie-beta
    -   instruct-davinci-beta

`vars` is a list of variable names. Each
variable is substituted into the prompt if it
has a corresponding template placeholder.

For example, the `<1>` in the prompt
corresponds to where the first variable
(`text-or-code`) will be substituted.

`examples` is a list with the same number of
elements as `vars`. The values in `examples`
may be suggested as initial input when
running the prompt function and may be used in
test cases. They also serve as documentation
for the user.

`preprocessors` are a list of shell
pipelineable commands (stream filters) which
expect both input and output and can be used
to preprocess the variables before they are
substituted into the prompt template.

This prompt doesn't have a `postprocessor`,
but if it did it would postprocess the
returned completions in a similar fashion to
how the variables are preprocessed.

Finally, `aliases` is a list of alternative
function names for this prompt.


## Installation {#installation}


### Install dependencies and compile emacs with `--with-modules` {#install-dependencies-and-compile-emacs-with-with-modules}

{{< highlight bash "linenos=table, linenostart=1" >}}
git checkout "https://github.com/semiosis/pen.el"
cd pen.el/src
# Careful with setup script.
# Run the commands manually as this is designed for root user, intended for a Docker container.
./setup.sh
{{< /highlight >}}

Demo of running the script on a vanilla VPS.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/EzlkZpFMS0NVEUOjcNnlKEJao" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/EzlkZpFMS0NVEUOjcNnlKEJao.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/EzlkZpFMS0NVEUOjcNnlKEJao.js" id="asciicast-EzlkZpFMS0NVEUOjcNnlKEJao" async></script>


### Ensure the following or similar file structure {#ensure-the-following-or-similar-file-structure}

Or make the additions / adjustments to your own emacs config.

Take the parts you need from the `init.el` and place inside your own `~/.emacs`.

If you don't have an init file of your own then run this.

{{< highlight bash "linenos=table, linenostart=1" >}}
ln -sf ~/.emacs.d/pen.el/init.el ~/.emacs
{{< /highlight >}}

Ensure you have the prompts repository in place.

{{< highlight bash "linenos=table, linenostart=1" >}}
git checkout "https://github.com/semiosis/prompts/tree/master/prompts" ~/.emacs.d/prompts
{{< /highlight >}}


### OpenAI - Just request a key and place it here {#openai-just-request-a-key-and-place-it-here}

Install OpenAI API key.

{{< highlight bash "linenos=table, linenostart=1" >}}
mkdir -p ~/.pen
touch ~/.pen/openai_api_key
vim ~/.pen/openai_api_key
{{< /highlight >}}


## Using Pen {#using-pen}


### Just starting on a vanilla installation {#just-starting-on-a-vanilla-installation}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/gwnk0DXnHKSzvUfLmfnQegfCx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/gwnk0DXnHKSzvUfLmfnQegfCx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/gwnk0DXnHKSzvUfLmfnQegfCx.js" id="asciicast-gwnk0DXnHKSzvUfLmfnQegfCx" async></script>