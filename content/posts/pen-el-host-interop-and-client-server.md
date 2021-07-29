+++
title = "Pen.el host interop and client/server"
author = ["Shane Mulligan"]
date = 2021-07-29T00:00:00+12:00
keywords = ["gpt", "emacs", "nlp", "docker", "pen"]
draft = false
+++

## Summary {#summary}

I have set up interop between the host OS and
`Pen.el` docker container.

This means you can run a single `Pen.el`
server and have multiple clients connect to
it.

You may want to be working with multiple
documents, for example, or have a separate
client just for prompt engineering.

You may also run prompt commands and interact
with the dockerified emacs via the terminal on
the host and use `Pen.el`'s prompt functions
remotely and in other software via shell
commands.

GitHub project
: [GitHub - semiosis/pen.el: pen.el is a package for prompt engineering in emacs. It facilitates the creation, ongoing development, discovery and usage of prompts to a language model such as OpenAI's GPT-3 or EleutherAI's GPT-j.](https://github.com/semiosis/pen.el/)


Project timeline and objectives
: <https://github.com/semiosis/pen.el/tree/master/docs>


Prompt README
: [prompts/README.org at master  semiosis/prompts  GitHub](http://github.com/semiosis/prompts/blob/master/README.org)


Tutorial
: <https://semiosis.github.io/posts/pen-tutorial/>


Video demo of `Pen.el` : [Augment Minds 2021: Demo of Loom and Pen.el - YouTube](https://www.youtube.com/watch?v=J9BnZjWV1jw)


## Running a prompt function from the host {#running-a-prompt-function-from-the-host}


### Firstly, start a server {#firstly-start-a-server}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen
{{< /highlight >}}

To run additional clients, just run `pen`
again in a different terminal.


### Then run a prompt function {#then-run-a-prompt-function}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -e '(car (pf-list-of 5 "tennis players" :no-select-result t))'
{{< /highlight >}}

```bash
Elena Dementieva
Roger Federer
Marat Safin
Anastasia Myskina
Andre Agassi
```


### Alternatively, make a symlink to the `pen` script {#alternatively-make-a-symlink-to-the-pen-script}

Like so:

{{< highlight bash "linenos=table, linenostart=1" >}}
ln -sf pen pf-list-of
{{< /highlight >}}

Then you may run the functions as scripts on your host.

{{< highlight bash "linenos=table, linenostart=1" >}}
pf-list-of 5 "tennis players"
{{< /highlight >}}

```bash
Elena Dementieva
Roger Federer
Marat Safin
Anastasia Myskina
Andre Agassi
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pf-very-witty-pick-up-lines-for-a-topic egypt
{{< /highlight >}}

```bash
I wonder if the pyramids would've still been built if the Egyptians had Tinder?
Did you know, in Egypt, cats are considered to be good luck?
I'm in Egypt, looking for some artefacts.
Are you Cleopatra? Because I want you to be my Queen.
My heart says mummies, but my body says pyramids.
You look like pharaoh material.
I want to be the one you wake up to in the morning.
Your sarcophagus would be mine.
I want to make you my pyramid.
I want to be where the Nile flows.
Have you ever been to Egypt? I don't think it's the pyramids, I think it's you.
You look like Cleopatra reincarnated.
Let's spend a day in Egypt.
Do you like Egyptian men? Cuz I like Egyptian women even though they don't exist.
Hey, I'm Tut, you're my Ka, I guess that makes you my cat.
You look like the type that would be found in the Valley of the Kings.
Your body is like the pyramids. Uncovering you would be a true archeological find.
I want to build a pyramid. With you. Inside you.
If you were a pharaoh I would build you a pyramid.
Do you want to be queen of my Nile?
```


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/dw0c0VueMHC8NOvGHmEgUUDcr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/dw0c0VueMHC8NOvGHmEgUUDcr.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/dw0c0VueMHC8NOvGHmEgUUDcr.js" id="asciicast-dw0c0VueMHC8NOvGHmEgUUDcr" async></script>