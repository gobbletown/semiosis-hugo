+++
title = "Pen.el host interop and client/server"
author = ["Shane Mulligan"]
date = 2021-07-29T00:00:00+12:00
keywords = ["gpt", "emacs", "nlp", "docker", "pen"]
draft = false
+++

## Summary {#summary}

`Pen.el` may now run as a server.
On your host OS, you can create symlinks to the `pen` script
with names matching the name of prompts.
Running those symlinks will then run the prompt
via the server and return the result.

Alternatively, you can run `pen pf-my-prompt-function "first arg" "second arg" "etc."` on
the host machine and it will run the prompt
function on the server and return the output
as stdout on the host.

You can even pipe into `pen` or the symlinks
and the stdin will be used as the first
argument to the prompt.

This means it is now dead simple
to integrate GPT-3 and other LMs easily into
your bash pipelines and other software. `Pen.el`
is now your management system for LMs and
prompting.

You may create multiple client interfaces to `Pen.el`,
which means you can have many prompt-engineering workflows going at once.

You may want to be working with multiple
documents, for example, or have a separate
client just for prompt engineering.

GitHub project
: [GitHub - semiosis/pen.el: pen.el is a package for prompt engineering in emacs. It facilitates the creation, ongoing development, discovery and usage of prompts to a language model such as OpenAI's GPT-3 or EleutherAI's GPT-j.](https://github.com/semiosis/pen.el/)


Project timeline and objectives
: <https://github.com/semiosis/pen.el/tree/master/docs>


Prompt README
: [prompts/README.org at master  semiosis/prompts  GitHub](http://github.com/semiosis/prompts/blob/master/README.org)


Tutorial
: <https://semiosis.github.io/posts/pen-tutorial/>


Video demo of `Pen.el` : [Augment Minds 2021: Demo of Loom and Pen.el - YouTube](https://www.youtube.com/watch?v=J9BnZjWV1jw)


Bash prompting README
: <http://github.com/semiosis/pen.el/blob/master/docs/using-prompt-functions-in-bash.org>


## Using prompt functions in bash {#using-prompt-functions-in-bash}


### Start the server on the host and open a separate terminal elsewhere {#start-the-server-on-the-host-and-open-a-separate-terminal-elsewhere}

-   Run `pen` in a terminal.
-   Open a terminal

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Q2YQw5OnTGFSfJ6t4baoQYb5G" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Q2YQw5OnTGFSfJ6t4baoQYb5G.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Q2YQw5OnTGFSfJ6t4baoQYb5G.js" id="asciicast-Q2YQw5OnTGFSfJ6t4baoQYb5G" async></script>


### List out the prompt functions and get help {#list-out-the-prompt-functions-and-get-help}

{{< highlight sh "linenos=table, linenostart=1" >}}
penl | fzf
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
penh pf-code-one-liners-from-natural-language
{{< /highlight >}}

```bash
(pf-code-one-liners-from-natural-language &optional LANGUAGE TASK &key NO-SELECT-RESULT)
```

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/CdmhI44EThh6QBi4sGoHEggUX" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/CdmhI44EThh6QBi4sGoHEggUX.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/CdmhI44EThh6QBi4sGoHEggUX.js" id="asciicast-CdmhI44EThh6QBi4sGoHEggUX" async></script>


### Caching is default {#caching-is-default}

{{< highlight bash "linenos=table, linenostart=1" >}}
pene '(car (pf-list-of 7 "tennis players" :no-select-result t))'
{{< /highlight >}}

```bash
Roger Federer
Katarina Srebotnik
Venus Williams
Bernard Tomic
Andre Agassi
Amélie Mauresmo
Katharina Kruger
```

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/5oayO80jrdJJ8k77tdsSdW9tM" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/5oayO80jrdJJ8k77tdsSdW9tM.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/5oayO80jrdJJ8k77tdsSdW9tM.js" id="asciicast-5oayO80jrdJJ8k77tdsSdW9tM" async></script>


### Special functions {#special-functions}


#### `pen-update` {#pen-update}

This updates the disk memoization.

You may use `pene` (or `pen -e`) and specify
`pen-update` manually, or you may use `penu`
(`pen -u`) which has a default `pen-update`.

{{< highlight bash "linenos=table, linenostart=1" >}}
pene '(car (pen-update (pf-list-of 7 "tennis players" :no-select-result t)))'
{{< /highlight >}}

```bash
Andre Agassi
Billie Jean King
Samantha Stosur
Roger Federer
Andy Murray
Juan Martín del Potro
Ivo Karlović
```

{{< highlight bash "linenos=table, linenostart=1" >}}
penu '(car (pf-list-of 7 "tennis players" :no-select-result t))'
{{< /highlight >}}

```bash
Evonne Goolagong
John McEnroe
Chris Evert
Martina Navratilova
Patty Schnyder
Arantxa Sánchez Vicario
Steffi Graf
```

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/ZK3goXrWfSU83KEVxStVCZyF2" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/ZK3goXrWfSU83KEVxStVCZyF2.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/ZK3goXrWfSU83KEVxStVCZyF2.js" id="asciicast-ZK3goXrWfSU83KEVxStVCZyF2" async></script>


### You may also use prompt functions this way {#you-may-also-use-prompt-functions-this-way}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-nlsh "arch linux" "install python package"
{{< /highlight >}}

```bash
pacman -S python-pip
```

Another way to update is to prefix `upd`.

I'm going to deliberately request an ambiguous command "open an image".

{{< highlight bash "linenos=table, linenostart=1" >}}
upd pen pf-nlsh "arch linux" "open an image"
{{< /highlight >}}

```bash
rxvt -bg black -fg white -geometry 130x30 foo.png
sxiv foo.png
feh --bg-scale foo.png
gthumb foo.png
eog foo.png
```

You may also use the `UPDATE` environment variable.

{{< highlight bash "linenos=table, linenostart=1" >}}
UPDATE=y pen pf-nlsh "arch linux" "open an image"
{{< /highlight >}}

```bash
feh filename.png
imagemagick filename.jpg
convert image.jpg image.png
```


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
echo "How shave a beard" | pen pf-complicated-explanation-of-how-to-x
{{< /highlight >}}

```bash
A beard is the collection of coarse hair that
grows on the chin and cheeks of humans and
some non-human animals. The term is often used
to describe acollection of hair that forms on
the chin and cheeks of humans, and to a lesser
extent on the face of some male monkeys.
Depending on the context, the term might refer
to the hair on the chin or to the collection
of hair on the lower face. In the English
language the term beard also refers to the
hair that grows on the
```

{{< highlight bash "linenos=table, linenostart=1" >}}
echo "How to crack an egg" | pen pf-complicated-explanation-of-how-to-x
{{< /highlight >}}

```bash
Cracking an egg involves application of force
sufficient to cause the eggshell to separate
from the egg white and the egg yolk. The
eggshell is composed of calcium carbonate
which is soluble in dilute hydrochloric acid.
Solution of calcium carbonate is achieved by
application of heat. The application of heat
is achieved by creating a temperature gradient
between the egg and the egg receptacle. The
egg receptacle is composed of ceramic
materials which are highly resistant to
thermal shock. Cracking the egg results
```

`Pen.el` saves the outputs of prompt functions
by default, so extending the command as below
will reuse the same output.

{{< highlight bash "linenos=table, linenostart=1" >}}
echo "How to crack an egg" | pen pf-complicated-explanation-of-how-to-x | pen pf-tldr-summarization
{{< /highlight >}}

```bash
I cracked an egg on a ceramic plate and the eggshell separated from the egg white and yolk.
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


### See also {#see-also}

-   <https://mullikine.github.io/posts/pen-el-host-interop-and-client-server/>