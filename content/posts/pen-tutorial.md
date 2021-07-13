+++
title = "Pen Tutorial"
author = ["Shane Mulligan"]
date = 2021-07-11T00:00:00+12:00
keywords = ["gpt", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

This is a tutorial for how to use `pen.el`.

The following command starts `pen.el`. You
only need to have docker installed and an
`OpenAI GPT-3` key to try this out.

{{< highlight sh "linenos=table, linenostart=1" >}}
docker run --rm -ti --entrypoint= semiosis/pen.el:latest ./run.sh
{{< /highlight >}}

Project code
: <https://github.com/semiosis/pen.el/>

For `GPT-neo`, `GPT-2` and `booste` support,
and latest updates, please install from master
branch. Docker support coming. `GPT-j` also in
the works.


## Default Key bindings {#default-key-bindings}

| kb        | f                               |                |
|-----------|---------------------------------|----------------|
| `H-TAB g` | `pen-generate-prompt-functions` | `pen-map`      |
| `H-TAB r` | `pen-run-prompt-function`       | `pen-map`      |
| `SPC`     | `pen-run-prompt-function`       | `selected-map` |
| `H-TAB l` | `pen-complete-long`             | `pen-map`      |

`H` is the Hyper key, which works similar to Escape, Meta, Alt, Control or Shift that is present on the Space Cadet Keyboard.

`pen.el` emulates a Hyper key (`H-`) with `C-M-\`.

I like `Hyper` because you're writing `hyperreality`.

{{< highlight text "linenos=table, linenostart=1" >}}
hyperreality
    [#semiotics]
    [#postmodernism]

    An inability of consciousness to
    distinguish reality from a simulation of
    reality, especially in technologically
    advanced postmodern societies.
{{< /highlight >}}


### How to run `H-TAB r` for emacs noobies {#how-to-run-h-tab-r-for-emacs-noobies}

For mac users
: Select some text, tap `Esc`, hold `Ctrl` and press  `\`, release and tap `r`.


For everyone else
: Select some text, hold `Ctrl Alt \`, release and tap `r`.

You may also press `SPC` while some text is selected to run a prompt function.

You may also use `right click` for starting the context menu.


### Company-mode {#company-mode}

For mac users
: Select some text, tap `Esc`, hold `Ctrl` and press  `\`, release and tap `c`.


For everyone else
: Select some text, hold `Ctrl Alt \`, release and tap `c`.


### Usage {#usage}

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
[slugified](https://pypi.org/project/python-slugify/) and used as the name of
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


### Prompt Engineering Workflow {#prompt-engineering-workflow}

-   Setup
    -   Install `prompt` snippet into yasnippet.
    -   M-x `yas/reload-all`
    -   M-x `yas-insert-snippet`

-   Prompt design
    -   1. Come up with a task. Let's call it "Negate sentence"
    -   2. Insert the prompt snippet into a new prompt file.
    -   3. Remove keys from prompts file which we don't need.
    -   4. `var-defaults` is an advanced usage of prompts
        -   But we will remove them
    -   5. Now load the prompt with `M-x pen-generate-prompt-functions`
    -   6. Now look at the prompt function documentation
        -   The binding `C-h C-f` is used to bring up help for a function
    -   7. Looks like we made an error: "The Mars is very far away."
        -   Change it and update the version of the prompt
    -   8. Reload functions

Test it out.

I want to eat dinner now.

It didn't work. hurm.

Well, here is the basic process anyway. I'll try and debug this.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/ofJjyh1A696NDOjwNx0zR6DAI" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/ofJjyh1A696NDOjwNx0zR6DAI.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/ofJjyh1A696NDOjwNx0zR6DAI.js" id="asciicast-ofJjyh1A696NDOjwNx0zR6DAI" async></script>


## Another `.prompt` exhibition {#another-dot-prompt-exhibition}


### I create a new prompt here for translating between any world language {#i-create-a-new-prompt-here-for-translating-between-any-world-language}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/jiBD5ZpRJQWXFMlHdvGGgSxjk" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/jiBD5ZpRJQWXFMlHdvGGgSxjk.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/jiBD5ZpRJQWXFMlHdvGGgSxjk.js" id="asciicast-jiBD5ZpRJQWXFMlHdvGGgSxjk" async></script>

Maori isn't a very prominent language on the
web, but it still managed to capture the idea
of a welcome message, which I think is
amazing! I am Maori, so I appreciate this!

I want to demonstrate the usage of two more `.prompt` keys.

The technical jargon
: `var-defaults` overrides the default behaviour of the `(interactive)` form in emacs.

By specifying `var-defaults`, you can change
what functions or expressions are run to
acquire the values for the parameters to the
prompt.

The prompt here captures the selected text and
puts it into the second placeholder, `<2>`.

By default, that would go into the first one, `<1>`.

{{< highlight yaml "linenos=table, linenostart=1" >}}
var-defaults:
- "(read-string \"language: \")"
- "(pen-selected-text)"
{{< /highlight >}}


### Original prompt {#original-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
---
title: Translate from English to
prompt-version: 2
doc: This prompt translates English text to any world langauge
issues:
- I think the unicode characters may be multibyte causing issues with completion
prompt: |
  ###
  # English: Hello
  # Russian: Zdravstvuyte
  # Italian: Salve
  # Japanese: Konnichiwa
  # German: Guten Tag
  # French: Bonjour
  # Spanish: Hola
  ###
  # English: Happy birthday!
  # French: Bon anniversaire !
  # German: Alles Gute zum Geburtstag!
  # Italian: Buon compleanno!
  # Indonesian: Selamat ulang tahun!
  ###
  # English: <2>
  # <1>:
engine: davinci
temperature: 0.5
max-tokens: 200
top-p: 1
stop-sequences:
- "#"
vars:
- language
- phrase
# ascification of the prompt is not ideal
prompt-filter: pen-c ascify
examples:
- French
- Goodnight
var-defaults:
- "(read-string \"language: \")"
- "(pen-selected-text)"
{{< /highlight >}}


### I create this prompt {#i-create-this-prompt}

{{< highlight text "linenos=table, linenostart=1" >}}
prompt-filter: pen-c ascify
{{< /highlight >}}

The `prompt-filter` is a final filter script
to transform the prompt before sending to the
`API` / `LM` for completion.

{{< highlight yaml "linenos=table, linenostart=1" >}}
---
title: Translate from world language X to Y
prompt-version: 2
doc: This prompt translates English text to any world langauge
issues:
- I think the unicode characters may be multibyte causing issues with completion
prompt: |
  ###
  # English: Hello
  # Russian: Zdravstvuyte
  # Italian: Salve
  # Japanese: Konnichiwa
  # German: Guten Tag
  # French: Bonjour
  # Spanish: Hola
  ###
  # English: Happy birthday!
  # French: Bon anniversaire !
  # German: Alles Gute zum Geburtstag!
  # Italian: Buon compleanno!
  # Indonesian: Selamat ulang tahun!
  ###
  # <1>: <3>
  # <2>:
engine: davinci
temperature: 0.5
max-tokens: 200
top-p: 1
stop-sequences:
- "#"
vars:
- from-language
- to-language
- phrase
# ascification of the prompt is not ideal
prompt-filter: pen-c ascify
examples:
- English
- French
- Goodnight
var-defaults:
- "(read-string \"From language: \")"
- "(read-string \"To language: \")"
- "(pen-selected-text)"
{{< /highlight >}}


## Current Development {#current-development}


### `company-mode` {#company-mode}

I'm trying to do something a little more
ambitious than simply having a single
completion function.

There will be infinitely many completion functions that you can select from.

| kb        | f                      |           |
|-----------|------------------------|-----------|
| `H-TAB c` | `pen-company-complete` | `pen-map` |


### HuggingFace transformers {#huggingface-transformers}

Mark Watson in his book "Practical Artificial
Intelligence Programming With Clojure" uses
spaCy and the HuggingFace transformers library
from Clojure. I would like to connect to
HuggingFace's transformers library in this way.

See "<https://markwatson.com/>".


### GPT-neo {#gpt-neo}

<https://github.com/samrawal/emacs-secondmate/>


### GPT-2 {#gpt-2}

Thank you `@Samin` and `@erik` for the
`booste` API support in integrating a free to
use GPT-2.

Please visit <https://www.booste.io/> to get your key.


### `GPT-j` {#gpt-j}

Currently working on a way to integrate this.