+++
title = "ilambda Tutorial"
author = ["Shane Mulligan"]
date = 2021-11-30T00:00:00+13:00
keywords = ["𝑖λ", "pen", "imaginary", "elisp"]
draft = false
+++

## Summary {#summary}

Here's a demonstration of how to use ilambda for emacs.

This is all you need to do to use `ilambda` `:)`.


## Thin-client demo {#thin-client-demo}

Firstly, start the pen server in the background.
You will need to follow setup instructions for pen.el


### Initial pen server setup {#initial-pen-server-setup}

{{< highlight bash "linenos=table, linenostart=1" >}}
git clone "https://github.com/semiosis/pen.el"
git clone "https://github.com/semiosis/prompts"

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

# Run pen in the background, and ensure everything is pulled and up-to-date
pen -nw
{{< /highlight >}}


### Force the engine to the key you have, if you don't have the OpenAI key {#force-the-engine-to-the-key-you-have-if-you-don-t-have-the-openai-key}

If you have an `OpenAI` key, then most
prompts/features/applications will simply
work without further setup. If you don't have an OpenAI key, but
have another key such as `AI21` then you can
force all prompts to use the key you have.

I will demonstrate forcing Pen.el to use the `Cohere` engine for everything.
This is useful if you only have a Cohere key.

{{< highlight sh "linenos=table, linenostart=1" >}}
# Run this on your host machine
pen config
{{< /highlight >}}

Ensure this is enabled:

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(setq pen-prompt-force-engine-disabled t)
{{< /highlight >}}

And this is set:

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(setq pen-force-engine "Cohere")
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/h7EUvqjA0hH6g3Wuab6TLHpVP" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/h7EUvqjA0hH6g3Wuab6TLHpVP.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/h7EUvqjA0hH6g3Wuab6TLHpVP.js" id="asciicast-h7EUvqjA0hH6g3Wuab6TLHpVP" async></script>


### Now to showcase ilambda {#now-to-showcase-ilambda}

{{< highlight sh "linenos=table, linenostart=1" >}}
# Start a vanilla emacs
emacs -nw -Q
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Load the libaries inside the vanilla emacs
(load "/home/shane/source/git/semiosis/pen.el/src/ilambda.el")
(load "/home/shane/source/git/semiosis/pen.el/src/pen-client.el")
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(require 'pen-client)
(require 'ilambda)

(setq iλ-thin t)

(idefun thing-to-hex-color (thing))

(message (thing-to-hex-color "watermelon"))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(message (apply (idefun translate-to-pirate (phrase) "reword the phrase to sound like a pirate") '("Let's go, my friend")))
{{< /highlight >}}

As you can see, that all works well.

{{< figure src="./ilambda-pirate.png" >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/PjhauvXBNuTp9H7UQtsWK9mrF" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/PjhauvXBNuTp9H7UQtsWK9mrF.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/PjhauvXBNuTp9H7UQtsWK9mrF.js" id="asciicast-PjhauvXBNuTp9H7UQtsWK9mrF" async></script>