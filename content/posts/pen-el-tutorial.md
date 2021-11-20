+++
title = "Pen.el Tutorial"
author = ["Shane Mulligan"]
date = 2021-11-20T00:00:00+13:00
keywords = ["openai", "pen", "gpt", "nlp", "prompt-engineering"]
draft = false
+++

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
echo "<aix key here>" > ~/.pen/aix_api_key                # https://aixsolutionsgroup.com/
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

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pen-fn-translate/3 (buffer-substring (region-beginning) (region-end)) "English" "French")
{{< /highlight >}}


### Introduction {#introduction}


#### Demonstrate basic invocation {#demonstrate-basic-invocation}

<!--list-separator-->

-  Start pen server then use the client

<!--list-separator-->

-  M-SPC menu

<!--list-separator-->

-  Right click menu

<!--list-separator-->

-  Use `pen` with the web server

    -   Control-Click for right click

<!--list-separator-->

-  Use `pen` in gui mode

<!--list-separator-->

-  Use `pen` in terminal mode

<!--list-separator-->

-  Use `lg` in gui mode

    -   configuration of force-images
        -   This should generate missing images from alttext
    -   configuration of force-text
        -   This should generate missing alttext

<!--list-separator-->

-  Use `lg` in terminal mode


#### Do imaginary programming {#do-imaginary-programming}


#### Surf the imaginary web {#surf-the-imaginary-web}

-   Search the web by selecting
-   Use eww browser


#### Use pen for autocompletion {#use-pen-for-autocompletion}


#### Use pen for translation {#use-pen-for-translation}


#### Run pickup lines {#run-pickup-lines}


#### Translate code {#translate-code}


#### Run imaginary interpreters using `ii` {#run-imaginary-interpreters-using-ii}


#### Use the shell interop {#use-the-shell-interop}


#### Use the LSP server {#use-the-lsp-server}


#### Use the glossary system {#use-the-glossary-system}


#### Use `cterm` {#use-cterm}