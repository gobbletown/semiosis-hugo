+++
title = "Pen.el Tutorial"
author = ["Shane Mulligan"]
date = 2021-11-20T00:00:00+13:00
keywords = ["openai", "pen", "gpt", "nlp", "prompt-engineering"]
draft = false
+++

## Introduction {#introduction}

As a beginner, to get started I would aim to simply set up the `pen` standalone application.

This enables you do use `pen` without integrating it tightly into your own emacs.

You can still use the prompting functions in a
basic way; The thin-client prompting function names all start with `pen-fn-`
and the names of the fully-featured prompting functions all begin with `pf-`.

But you will not be able to use the `ilambda`
functions, or get the full experience without
integrating `Pen.el` into your host emacs
configuration.

However, if you're going to test out `pen` and
plan on only setting up the standalone
application along with the thin-client, you'll
still be able to use the following
applications:

| Application name       | Script name | Purpose                              |
|------------------------|-------------|--------------------------------------|
| `LookingGlass`         | `lg`        | Imaginary-Web Browser                |
| `Paracosm`             | `cosm`      | AI-Assisted Mind-map                 |
| `ComplexTerm`          | `ct`        | Real+Imaginary Terminal Multiplexer  |
| `ImaginaryInterpreter` | `ii`        | Fully imaginary language interpreter |
| `pen bash interop`     | `penf`      | Prompt function interface for bash   |

They are accessible by running the script name
on the host (or inside the container shell, if
you prefer).


## Initial setup {#initial-setup}

These instructions are designed for a linux
distribution with docker installed.

{{< highlight bash "linenos=table, linenostart=1" >}}
git clone "https://github.com/semiosis/pen.el"
git clone "https://github.com/semiosis/prompts"
git clone "https://github.com/semiosis/engines"
git clone "https://github.com/semiosis/pen-contrib.el"

mkdir -p ~/.pen

# Put in your keys, or do not, it's up to you!
echo "sk-<openai key here>" > ~/.pen/openai_api_key       # https://openai.com/
echo "<ai21 key here>" > ~/.pen/ai21_api_key              # https://www.ai21.com/
echo "<hf key here>" > ~/.pen/hf_api_key                  # https://huggingface.co/
echo "<nlpcloud key here>" > ~/.pen/nlpcloud_api_key      # https://nlpcloud.io/
echo "<alephalpha key here>" > ~/.pen/alephalpha_api_key  # https://aleph-alpha.de/
echo "<cohere key here>" > ~/.pen/cohere_api_key          # https://cohere.ai/

# Add the scripts to the PATH
echo export PATH="$(realpath .)/pen.el/scripts:\$PATH" >> ~/.profile

# Add this to prevent C-s from freezing the terminal
echo "stty stop undef 2>/dev/null; stty start undef 2>/dev/null" | tee -a ~/.zshrc >> ~/.bashrc

# Source your .profile
. ~/.profile

# Run pen
pen
{{< /highlight >}}

This will start `pen` emacs inside of a docker container.


### You can use it like a regular editor {#you-can-use-it-like-a-regular-editor}

This way, it will create a hardlink to the
file, so emacs in the docker container can
access it.

{{< highlight sh "linenos=table, linenostart=1" >}}
pen yas.el
{{< /highlight >}}


## Basic demo {#basic-demo}


### This is prompting {#this-is-prompting}

{{< highlight bash "linenos=table, linenostart=1" >}}
echo "Q: 'Why did the chicken cross the road?' A: '" | pen-prompt | head -n 1 | sed "s/'.*//"
{{< /highlight >}}

```bash
To get to the other side.
```


### Pen.el thin client {#pen-dot-el-thin-client}

-   [Thin-client for Pen.el. Just load pen-client.el // Bodacious Blog](https://mullikine.github.io/posts/thin-client-for-pen-el-just-load-pen-client-el/)

This lets you put basic prompt functions into
your own emacs without loading the whole of
`pen.el`.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pen-fn-translate/3 (buffer-substring (region-beginning) (region-end)) "English" "French")
{{< /highlight >}}


### Pen.el full install into your own `.emacs.d` {#pen-dot-el-full-install-into-your-own-dot-emacs-dot-d}

This is for advanced users, really.

-   Install the prerequsites
    -   <http://github.com/semiosis/pen.el/blob/master/scripts/setup.sh>
    -   These instructions are for root and designed for Ubuntu or Debian.
    -   It will take more work to get set up.
-   Load the example config
    -   <http://github.com/semiosis/pen.el/blob/master/src/pen-example-config.el>


### `pen` server (Using the standalone application, `pen`) {#pen-server--using-the-standalone-application-pen}

`pen` has a standalone application which is an emacs inside docker.
This also functions as a server.

{{< highlight bash "linenos=table, linenostart=1" >}}
# interact with pen as an editor
pen /path/to/my-file.txt

# It can also take stdin
cat my-file.txt | pen
{{< /highlight >}}

The first time you run `pen`, it will start an emacs daemon inside a docker container.
Subsequent calls to `pen` will use the same container and an `emacsclient`.

Starting the `pen` server with `pen -nw -n`
starts a new `pen` frame with "no window" i.e.
the terminal user interface, and says 'no' to
pulling updates.

Now that it is running, you can also use the bash interop.

{{< highlight bash "linenos=table, linenostart=1" >}}
penf pf-very-witty-pick-up-lines-for-a-topic/1 slovenia
{{< /highlight >}}

To create a shell into the docker container, simply run `pen sh`.

{{< highlight sh "linenos=table, linenostart=1" >}}
# From the host, start a shell in the docker container
pen sh

# If you want start a specific program (inside the container)
pen sh vim /
pen sh nlsh Ubuntu
{{< /highlight >}}


### Configuration {#configuration}

I'll explain the following example `pen.yaml` file.

<http://github.com/semiosis/pen.el/blob/master/config/example-pen.yaml>

`pen.yaml` lives at `~/.pen/pen.yaml` on the host machine.

{{< highlight yaml "linenos=table, linenostart=1" >}}
# When debug is on, try is disabled, and all errors throw an exception
debug: on

# Setting sh-update to on would disable caching/memoization
sh-update: off

# In future, this would disable the use of non-libre models
libre-only: off

# These variables are used by pen.el to
# automatically tailor the experience towards
# you.
fav-world-language: English
fav-programming-language: Emacs Lisp

# This overrides the language model / engine used for prompting functions
# However, if a .prompt file specifies
# force-engine, then this override will not
# override.
force-engine: OpenAI Codex

# This
force-few-completions: off

# This prevents multiple requests.
# Under normal circumstances, pen.el might
# perform multiple requests/generations to
# get to the desired quota for a prompt
# function. Under the hood, engines may have
# a max number of generations they can provide
# for a single request. force-single-
# collation ensures that only one request
# happens.
force-single-collation: off

# Force one is more extreme. It also sets
# the number of completions. So you get only
# one generation/completion per request, and only one request.
force-one: off

# This allows you to set the number of collations.
force-n-collate: ~
force-n-completions: ~
# force-temperature: ~

# Ink.el adds text properties to the emacs buffer when text has been generated.
disable-ink: off

# This is a heuristic used within Pen.el to make select cost-effiient options.
cost-efficient: on

# This generates alttext for the LookingGlass web browser
describe-images: on

# Default engines are used when the engine resolver (all fallbacks) fail
default-engines:
- text-to-text: OpenAI Codex
- image-to-text: AlephAlpha EUTranMM
# - text-to-image: OpenAI Dall-E
- text-to-image: ruDALL-E Malevich (XL)

# Here's a way to disable engines. This
# might be useful if you have a bad API key
# for example, and just want to disable the
# engine.
# Pattern match on the names
disabled-prompts:
# disabled-engines:
# - "AlephAlpha.*"
disabled-models:
{{< /highlight >}}


### `M-SPC` (aka. hyperspace) menu {#m-spc--aka-dot-hyperspace--menu}

If you navigate to `pen-define-maps` in <http://github.com/semiosis/pen.el/blob/master/src/pen-example-config.el>
You will find the default key bindings.

A bunch of useful prompting functions for code are bound under `M-SPC c`, for example.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pen-dk-easy "c l" 'pf-transpile/3)

;; The above maps the following:
(progn
  (define-key pen-map (kbd "H-TAB c l") 'pf-transpile/3)
  (define-key pen-map (kbd "H-SPC c l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-Q c l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-u c l") 'pf-transpile/3)
  (define-key pen-map (kbd "<H-tab> c l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC c l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC TAB c l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC C-M-i c l") 'pf-transpile/3)
  (define-key pen-map (kbd "H-TAB M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "H-SPC M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-Q M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-u M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "<H-tab> M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC TAB M-c M-l") 'pf-transpile/3)
  (define-key pen-map (kbd "M-SPC C-M-i M-c M-l") 'pf-transpile/3))
{{< /highlight >}}

The above are all valid ways to access the `hyperspace` menu.

I call it the hyperspace menu because one of the prefixes is `H-SPC`.

`H-` stands for the hyper key. It is invokable in pen with the chord `C-M-\`.


### Right click menu {#right-click-menu}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/ZVnG7bMcDR2zhqTnAOaOgP6qc" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/ZVnG7bMcDR2zhqTnAOaOgP6qc.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/ZVnG7bMcDR2zhqTnAOaOgP6qc.js" id="asciicast-ZVnG7bMcDR2zhqTnAOaOgP6qc" async></script>

To invoke the right-click context menu, you may use `right-click` or `control-left-click`.

`Control-Left-Click` is needed for the web interface.


### Use `pen` with the web server {#use-pen-with-the-web-server}

There are a few differences:

-   Always runs in terminal mode.
-   Right-click doesn't work, so use `Control-Click` instead for the context menu.
-   `M-SPC` doesn't work, so use `M-u` instead as a prefix.


### Use `pen` in gui mode {#use-pen-in-gui-mode}

To get the GUI mode, all you need to do is run
`pen` in a terminal somewhere.


### Use `pen` in terminal mode {#use-pen-in-terminal-mode}

Just add `-nw` to one of your commands, just like running `emacs`.


### Configuring `pen` {#configuring-pen}

Firstly, there is a `~/.pen` directory on your host machine.


### Use `lg` in gui mode {#use-lg-in-gui-mode}

There are a couple of configuration options.

-   configuration of <span class="underline">force-images</span>
    -   This should generate missing images from alttext
-   configuration of <span class="underline">force-text</span>
    -   This should generate missing alttext

<!--list-separator-->

-  Use `lg` in terminal mode


### Do imaginary programming {#do-imaginary-programming}

-   <http://github.com/semiosis/pen.el/blob/master/src/ilambda.el>

The functions are built into `pen.el`, and so
you can access them from within the `pen`
standalone application.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/wCBT3BxnPI8YKJHslC4xd4gfG" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/wCBT3BxnPI8YKJHslC4xd4gfG.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/wCBT3BxnPI8YKJHslC4xd4gfG.js" id="asciicast-wCBT3BxnPI8YKJHslC4xd4gfG" async></script>


### Surf the imaginary web {#surf-the-imaginary-web}

-   Search the web by selecting
-   Use eww browser


### Use pen for autocompletion {#use-pen-for-autocompletion}


### Use pen for translation {#use-pen-for-translation}


### Run pickup lines {#run-pickup-lines}


### Translate code {#translate-code}


### Run imaginary interpreters using `ii` {#run-imaginary-interpreters-using-ii}


### Use the shell interop {#use-the-shell-interop}

{{< highlight bash "linenos=table, linenostart=1" >}}
penf -u pf-very-witty-pick-up-lines-for-a-topic/1 slovenia
{{< /highlight >}}

```bash
I'd like to visit Slovenia with you.
```


### Use the LSP server {#use-the-lsp-server}

-   <https://mullikine.github.io/posts/an-lsp-server-for-codex/>


### Use the glossary system {#use-the-glossary-system}


### Use `cterm` {#use-cterm}

Still use `C-u 0` or `H-u` to prefix onto pen bindings to bypass/update the cache.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qW1NNbbErJPrXU4mStyZmRc6p" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qW1NNbbErJPrXU4mStyZmRc6p.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qW1NNbbErJPrXU4mStyZmRc6p.js" id="asciicast-qW1NNbbErJPrXU4mStyZmRc6p" async></script>


### Use `nlsh` (Natural Language Shell) {#use-nlsh--natural-language-shell}

You may describe the actions you wish to
perform in NL, and `nlsh` will give you the
shell commands.

`nlsh` expects one parameter (the operating
system) and provides you with a REPL.

This allows you to rapidly create a shell that
awaits your NL descriptions and translates
them into real shell commands.

-   <http://semiosis.github.io/posts/pen-s-nlsh-for-codex/>


### Talk to a chatbot {#talk-to-a-chatbot}

Select any text and run `M-x apostrophe-start-chatbot-from-selection` or `M-SPC a c`.

-   <https://semiosis.github.io/posts/multi-part-prompts/>


## See also {#see-also}

Original tutorial
: <https://semiosis.github.io/posts/pen-tutorial/>