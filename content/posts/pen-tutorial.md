+++
title = "Pen Tutorial"
author = ["Shane Mulligan"]
date = 2021-07-11T00:00:00+12:00
keywords = ["gpt", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

The following commands set up `pen.el` for the first time.

Apply for a GPT-3 key
: <https://beta.openai.com/>


Free AIX GPT-j key
: <https://apps.aixsolutionsgroup.com/>

These are the steps to run `Pen`. It will use a docker image.

{{< highlight bash "linenos=table, linenostart=1" >}}
git clone "https://github.com/semiosis/pen.el"
git clone "https://github.com/semiosis/prompts"

mkdir -p ~/.pen

# Put in your keys, or do not, it's up to you!
echo "sk-<openai key here>" > ~/.pen/openai_api_key
echo "<aix key here>" > ~/.pen/aix_api_key

# Add the scripts to the PATH
echo export PATH="$(realpath .)/pen.el/scripts:\$PATH" >> ~/.profile

# Add this to prevent C-s from freezing the terminal
echo "stty stop undef; stty start undef" | tee -a ~/.zshrc >> ~/.bashrc

# Source your .profile
. ~/.profile

# Run pen
pen
{{< /highlight >}}

Or for Windows users:

{{< highlight powershell "linenos=table, linenostart=1" >}}
cd ~
git.exe clone "https://github.com/semiosis/pen.el"
git.exe clone "https://github.com/semiosis/prompts"
.\pen.el\scripts\pen.ps1
{{< /highlight >}}

If there are any issues with the powershell
script, either fix the script and submit a
patch or invoke the docker command manually.

{{< highlight sh "linenos=table, linenostart=1" >}}
docker.exe run -ti -v "C:\<path to prompts>:/root/.emacs.d/host/prompts" --entrypoint= semiosis/pen.el:latest ./run.sh
{{< /highlight >}}

-   Video demo : [Augment Minds 2021: Demo of Loom and Pen.el - YouTube](https://www.youtube.com/watch?v=J9BnZjWV1jw)

-   <https://github.com/semiosis/pen.el/>

-   <https://discord.gg/GDSqFt6j>

{{< figure src="/ox-hugo/wizard4.png" >}}

For `GPT-neo`, `GPT-2` and `booste` support,
and latest updates, please install from master
branch. Docker support coming. `GPT-j` also in
the works.


## Invoking `Pen.el`: Client and server {#invoking-pen-dot-el-client-and-server}

Please glance over this to learn how to invoke.

It'll get you going with `Pen.el` very quickly.

<https://mullikine.github.io/posts/pen-el-host-interop-and-client-server/>

The essence is you simply run the `pen` script
and it takes care of running the emacs+docker server.


## Configuration {#configuration}

For linux users, if you place your OpenAI API
key in the file `~/.pen/openai_api_key`, the
[pen shell script](https://github.com/semiosis/pen.el/blob/mast%20er/scripts/pen) will
automatically pick that up.

If you run `pen` in a directory that contains a [prompts repository](https://github.com/semiosis/prompts/tree/master/prompts) then that repository will be used by `pen.el`.
That way you can persist changes to your prompts repository.

If you run `pen` in a directory containing a [pen.el repository](https://github.com/semiosis/pen.el/blob/master/src/pen.el) then that code will be what is run in the docker, rather than the `pen.el` contained within.

You may save files to the `~/.pen` directory
within the docker and those files will be
accessible to the host and will not be erased.


## Running `Pen.el` from the host, in bash {#running-pen-dot-el-from-the-host-in-bash}

With a server running, you have these commands available on the host:

| Script name | Function                                             | Example                                        |
|-------------|------------------------------------------------------|------------------------------------------------|
| `pen`       | Start a new client                                   |                                                |
| `penl`      | List the prompt functions                            |                                                |
| `penh`      | Get the signature of a prompt function               | `penh pf-list-of`                              |
| `pene`      | Run some emacs code from the host and get the result | `pene '(progn (message "hi") (message "yo"))'` |
| `penq`      | Quit the server                                      |                                                |


## `Acolyte Mode` - Key bindings for emacs noobs {#acolyte-mode-key-bindings-for-emacs-noobs}

These are enabled by default with the docker image.

| kb                       |                                                                                 |
|--------------------------|---------------------------------------------------------------------------------|
| `Alt-p`                  | Open the prompts directory in dired                                             |
| `Alt-t`                  | Start writing in an empty file                                                  |
| `Alt-s`                  | Save file                                                                       |
| `Alt-r`                  | Running a prompt function like this will not insert text or replace it.         |
| `Alt-TAB`                | This completes the current line.                                                |
| `Alt-l` (little L)       | Multiline (long) completion.                                                    |
| `Alt-g`                  | This reloads the prompt functions.                                              |
| `Alt-m`                  | Right click menu                                                                |
| Select text then `Alt-f` | This filters the text through a prompt function specifically designed for this. |
| `Spacebar`               | When text is selected, will run with that text as first argument.               |


## Key bindings for emacs wizards (also enabled in docker) {#key-bindings-for-emacs-wizards--also-enabled-in-docker}

| kb             | f                                 |                                                                                    |
|----------------|-----------------------------------|------------------------------------------------------------------------------------|
| `H-.` or `H-n` | `global-pen-acolyte-minor-mode`   | This toggles Acolyte mode.                                                         |
| `H-TAB g`      | `pen-generate-prompt-functions`   | This reloads the prompt functions.                                                 |
| `H-TAB r`      | `pen-run-prompt-function`         | Running a prompt function like this will not insert text or replace it.            |
| `M-1`          | `pen-company-filetype`            | This completes the current line.                                                   |
| `H-TAB s`      | `pen-filter-with-prompt-function` | This filters the text through a prompt function specifically designed for this.    |
| `H-TAB c`      | `pen-company-complete`            | Select a prompt function as the completer for `company-mode` and complete with it. |
| `SPC`          | `pen-run-prompt-function`         | When text is selected, will run with that text as first argument.                  |
| `H-TAB l`      | `pen-complete-long`               | This is a multiline completion.                                                    |

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

Imagining `Pen.el`
: [Imagery for Pen.el with CLIP and inspired from Myst: The Book of Atrus // Bodacious Blog](https://mullikine.github.io/posts/creating-some-imagery-for-pen-el-with-clip/)


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

More company bindings.

| kb        | f                             |                                            |
|-----------|-------------------------------|--------------------------------------------|
| `H-TAB f` | `pen-company-complete-choose` | Select a single completer. Remove others.  |
| `H-TAB a` | `pen-company-complete-add`    | Add other completers to the completer list |


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
version: 1
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
version: 2
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


## Using prompt functions in your code {#using-prompt-functions-in-your-code}

Prompt functions automatically 'curry' when
you leave out their arguments.

Here is an example, `pf-translate-from-world-language-x-to-y`:

{{< highlight text "linenos=table, linenostart=1" >}}
pf-translate-from-world-language-x-to-y is an interactive function
defined in pen-example-config.el.

Signature
(pf-translate-from-world-language-x-to-y &optional FROM-LANGUAGE TO-LANGUAGE PHRASE)

Documentation
Translate from world language X to Y
This prompt translates English text to any world langauge

path:
- /home/shane/source/git/spacemacs/prompts/prompts/translate-world-languages.prompt

examples:
- English
- French
- Goodnight

preprocessors:
- cat
- cat
- sed -z 's/\n/\\n/g'

var-defaults:
- (read-string-hist "Pen From language: ")
- (read-string-hist "Pen To language: ")
- (pen-selected-text)

prompt-filter:
- pen-c ascify
{{< /highlight >}}

If this function is run without a selection
then `pen-selected-text` will resort to asking
the user for input.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Leave out all arguments to be prompted for each argument
(pf-translate-from-world-language-x-to-y)
{{< /highlight >}}

The following default functions / expressions
(i.e. `var-defaults`) are run when called
interactively or to acquire the values of
optional parameters that were left out of the
call to the prompt function.

{{< highlight yaml "linenos=table, linenostart=1" >}}
var-defaults:
- "(read-string-hist \"Pen From language: \")"
- "(read-string-hist \"Pen To language: \")"
- "(pen-selected-text)"
{{< /highlight >}}

The following invocation supplies `"French"`
as the first parameter, but the others will be
requested.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pf-translate-from-world-language-x-to-y "French")
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
烤面包
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/lG511sKyJPjhwtn98IPbSZjYx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/lG511sKyJPjhwtn98IPbSZjYx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/lG511sKyJPjhwtn98IPbSZjYx.js" id="asciicast-lG511sKyJPjhwtn98IPbSZjYx" async></script>


## An assistant for any major mode {#an-assistant-for-any-major-mode}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/MS8xMQfLoExEVyh4Nqu9fX49b.js" id="asciicast-MS8xMQfLoExEVyh4Nqu9fX49b" async></script>


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