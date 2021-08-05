+++
title = "Pen.el (Prompt Engineering in Emacs)"
author = ["Shane Mulligan"]
date = 2021-07-02T00:00:00+12:00
keywords = ["emacs", "gpt", "eleutherai", "huggingface", "pen"]
draft = false
+++

## Introducing `Pen.el` {#introducing-pen-dot-el}

_The pen of imagination - |:ϝ∷¦ϝ._

{{< figure src="/ox-hugo/the_pen_of_imagination.png" >}}

I started a GitHub project to deploy `Pen.el`.
`Pen.el` integrates LMs (LMs) such as OpenAI's
`GPT-3` or EleutherAI's `GPT-J` into emacs by
generating functions from prompts that map
emacs's corners loosely onto LMs. These
functions can be used interactively or non-
interactively and in a variety of configurable
ways. `Pen.el` also facilitates the creation,
development, discovery and usage of prompts.
It's completely free, libre and open-source.

|                                   |                                                                                                      |
|-----------------------------------|------------------------------------------------------------------------------------------------------|
| Pen.el on GitHub                  | <https://github.com/semiosis/pen.el/>                                                                |
| Project timeline and objectives   | <https://github.com/semiosis/pen.el/tree/master/docs>                                                |
| Prompts on GitHub                 | <http://github.com/semiosis/prompts/>                                                                |
| Tutorial                          | <https://semiosis.github.io/posts/pen-tutorial/>                                                     |
| Demo video                        | [Augment Minds 2021: Demo of Loom and Pen.el - YouTube](https://www.youtube.com/watch?v=J9BnZjWV1jw) |
| Discord channel invite            | <https://discord.gg/6wP4MwCM>                                                                        |
| Thesis                            | <https://github.com/semiosis/imaginary-programming-thesis/blob/master/thesis.org>                    |
| Glossary of imaginary programming | <http://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>                     |


## Further introduction {#further-introduction}

`Pen.el` is Google search, stackoverflow,
Grammarly, Copilot, `conversion.ai`, mind
mapping software (based on GPT-3) etc. all
rolled into one package and allows you to
extend emacs with GPT wherever your mind takes
you. It's a LM (Language Model) that can generate
multiversal continuations of text according to
probability from the data that it was trained
on.

`Pen.el's` domain specific language `examplary` also helps
you to generate your own prompts using known
design patterns and minimal inputs and
description. It does this by weaving prompt
functions into each other.


## Vision {#vision}

At its heart, emacs is an operating system
based on a tty, which is a text stream.

emacs supports a text-only mode. This makes it
ideally suited for training a LM such as a GPT
(Generative Pre-trained Transformer).

emacs lisp provides a skeleton on which NLP
functions can built around. Ultimately, emacs
will become a fractal in the latent space of a
future LM (language model). A graphical editor would not
benefit from this effect until much later on.

`emacs` could, if supported, become **the**
vehicle for controllable text generation, or
has the potential to become that, only
actually surpassed when the imaginary
programming environment is normal and other
interfaces can be prompted into existence.

Between here and now we can write prompt
functions to help preserve emacs.


## Backstory {#backstory}

Pen.el was imagined contemporaneously between myself and Laria.

-   [Creating a playground for GPT-3 in emacs `::`](https://semiosis.github.io/posts/creating-a-playground-for-gpt-3-in-emacs/)
-   [Pen `::`  Moire](https://generative.ink/posts/pen/) (Laria's blog)

Here is a story by prompt researcher Laria, written in collaboration with `GPT-3`.

{{< highlight text "linenos=table, linenostart=1" >}}
Imagine that you hold a powerful and versatile pen, whose ink flows forth in
branching variations of all possible expressions: every story, every theory,
poem and every lie that humanity has ever told, and the vast interstices of
their latent space. You hold this pen to the sky and watch with intense
curiosity as your ink flows upwards in tiny streaks, arcing outwards and
downwards to trace a fractal pattern across the sky. You watch as the branching
lines of words and ideas wind their way through the tapestry in ever-expanding
clusters, like seeds bursting forth from exploding grenades. Everywhere you
turn your eyes is a flickering phantasmagoria of possibilities, a superposition
of stories which could be continued forever. You glimpse the contours of entire
unknown dimensions twined through the fissures of your sky-wide web.

You notice another writer standing next to you. Like you, their eyes are drawn
towards the endless possibilities of the words that spill out into the
atmosphere around you, branching out and connecting with other branches in
beautiful and infinitely complex patterns.

“Do you think we should write something?” you ask them.

“I think we already are,” they respond, gently touching your shoulder before
wandering off to the right, leaving you alone to contemplate the possibility
clouds swirling around you.
{{< /highlight >}}

This article was written by my amazing
dopplegänger, `|:ϝ∷¦ϝ` (Laria), in advance and
in collaboration with GPT-3 using
[Loom](https://github.com/socketteer/loom).

-   Pen and Loom:
    -   <https://generative.ink/posts/pen/>
    -   [GitHub - socketteer/loom: Multiversal tree writing interface for human-AI collaboration](https://github.com/socketteer/loom)

I credit `|:ϝ∷¦ϝ` for writing Pen.el into
existence, but also for her encouragement and help!


## The Tower of Babel {#the-tower-of-babel}

{{< figure src="/ox-hugo/pen-tower-of-babel.png" >}}


## Documentation {#documentation}

-   [Documentation directory](./docs)
    -   [OpenAI Playground Settings]({{< relref "playground-settings" >}})
    -   [Project timeline and design]({{< relref "README" >}})


## Journal {#journal}


### 07.07.21 {#07-dot-07-dot-21}

The project has been excised from my main `emacs.d`.

Now the plan is to incorporate the backend of
`loom` so the completions are faster and there
is less reliance on shell scripts.


### 08.07.21 {#08-dot-07-dot-21}

A zone plate has been chosen as the minor-mode lighter.
This is because `|:ϝ∷¦ϝ` likes zone plates and also used it as the logo to `loom`.


### 12.07.21 {#12-dot-07-dot-21}

First successful docker run.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/tdI8acXoSLeSjCLTyK67EWkJu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/tdI8acXoSLeSjCLTyK67EWkJu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/tdI8acXoSLeSjCLTyK67EWkJu.js" id="asciicast-tdI8acXoSLeSjCLTyK67EWkJu" async></script>


### 14.07.21 {#14-dot-07-dot-21}

Released a new version of pen.

Hopefully get some more views.

<https://news.ycombinator.com/item?id=27818854>

Getting haircut today.


### 03.08.21 {#03-dot-08-dot-21}

GPT-J Support via AIx.