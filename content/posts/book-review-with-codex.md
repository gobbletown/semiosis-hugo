+++
title = "Book review with Codex"
author = ["Shane Mulligan"]
date = 2021-10-12T00:00:00+13:00
keywords = ["emacs", "openai", "codex"]
draft = false
+++

## Summary {#summary}

I make a prompt which generates summaries of
books, based on what the LM has read.


## Prompt {#prompt}

`pf-synopsis-of-book/2`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Synopsis of book"
doc: "Given a book title, give a synopsis"
prompt-version: 1
prompt: |+
  http://readersdigest.co.uk/book-reviews/<sl:author>/<sl:title>/

  <delim>
  Book title: <q:title>
  <delim>
  Author: <q:author>
  <delim>
  Full synopsis: <:pp>"
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "author"
- "title"
examples:
- "J. R. R. Tolkien"
- "The Hobbit"
info: on
completion: off
insertion: off
n-collate: 1
postprocessor: pen-str remove-leading-and-trailing-newlines | uq | sed -z 's/^"//' | pen-pretty-paragraph
{{< /highlight >}}


## Demo {#demo}


### Through the Looking Glass : and What Alice Found There - Lewis Carroll {#through-the-looking-glass-and-what-alice-found-there-lewis-carroll}

{{< highlight text "linenos=table, linenostart=1" >}}
Alice follows a procession of playing cards, a
duck, a kangaroo, a walrus and a baby bitzhu.

In a crazy, upside down world, Alice meets a
baby, a white kitten, a hookah-smoking
caterpillar, a Cheshire cat, a baby of
literature, and a pig and a sheep on their way
to be slaughtered for food for the baby,
bitzhu.

She is tricked into falling down a rabbit hole
into an underground kingdom.

She finds herself in a long corridor.

She meets the Red Queen, who tells her she is
now in a game and to get to the opposite end
of the corridor within two moves.

She also finds that the playing cards, the
White King, the Red King, the Knave of Hearts,
and the Knave of Diamonds are alive and
talking to her.

Alice realizes the game is a trick played by
the Red Queen and, using the fan to fan
herself, calls back the Red Queen's
flamingoes, who
{{< /highlight >}}


### The Hunting of the Snark {#the-hunting-of-the-snark}

{{< highlight text "linenos=table, linenostart=1" >}}
"The Hunting of the Snark" is a short
children's novel by the British author Lewis
Carroll.

A popular children's story, it is about a
journey taken by the protagonist, an English
navigation captain, in search of a probable
adventure.

During this journey, he is accompanied by
aortal creatures known as the "Brummles", who
are able to share the same physical space in
an apparently telepathic manner.

It is also a parody of "Journey's End" by
Emily Dickinson, which is itself a parody of
"The Hunting of the Snark".
{{< /highlight >}}


### The Hobbit - J. R. R. Tolkien {#the-hobbit-j-dot-r-dot-r-dot-tolkien}

{{< highlight text "linenos=table, linenostart=1" >}}
Bilbo Baggins is a hobbit who enjoys a
comfortable, unambitious life, rarely
traveling any farther than his pantry or
cellar.

But his contentment is disturbed when the
wizard Gandalf and a company of dwarves arrive
on his doorstep one day to whisk him away on
an adventure.

They have launched a plot to raid the treasure
hoard guarded by Smaug the Magnificent, a
large and very dangerous dragon.

Bilbo reluctantly joins their quest, unaware
that on his journey to the Lonely Mountain he
will encounter both a magic ring and a
frightening creature known as Gollum.
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/xe1JYXcKFyFfKvxG0bteYt5tR" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/xe1JYXcKFyFfKvxG0bteYt5tR.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/xe1JYXcKFyFfKvxG0bteYt5tR.js" id="asciicast-xe1JYXcKFyFfKvxG0bteYt5tR" async></script>


## Crtique of pure reason - Immanuel Kant {#crtique-of-pure-reason-immanuel-kant}


### Result 1 {#result-1}

{{< highlight text "linenos=table, linenostart=1" >}}
A demystifying text on major work of modern
philosophy.

Written in 1781, but with many later
additions, it attempts to systematize the main
argument of the previous century.

Never intended to be published, it was
intended to guide the German philosophical
professors of the epoch.

However, its wide citations in the modern
analytic philosophy of the West have made it
one of the most famous documents of modern
philosophy.
{{< /highlight >}}


### Result 2 {#result-2}

{{< highlight text "linenos=table, linenostart=1" >}}
Kant presents a transcendental deduction of
synthetic a priori truths.

The work founded the tradition of critical
philosophy and paved the way to German
idealism.

Based on previous philosophical thinking, his
first major work, The Critique of Pure Reason
(1781) developed arguments for idealism
regarding the relationship between the human
reason and the world.

He argued that the source of all our knowledge
is the faculty of understanding, that through
intuition the human mind receives the
fundamental concepts of space, time,
causation, and necessity, which are the source
of all empirical knowledge.

He also explored other parts of his
philosophy, including his arguments for the
synthetic a priori nature of geometry, his
views on the relations between the knowing
subject
{{< /highlight >}}


### Result 3 {#result-3}

{{< highlight text "linenos=table, linenostart=1" >}}
The Critique of Pure Reason (hereafter
referred to simply as the Critique) is perhaps
the most complex, profound, and inaccessible
work in the history of metaphysical
philosophy.

The entire work comprises two parts (the first
of which is treated in this volume).

The first part contains three "dialogues" (or,
more properly, "questions") in which the
author seeks to prove the three "fundamental"
metaphysical ideas (categories of
understanding, ideas of reason, and synthetic
a priori truths) that are necessary for any
knowledge of objects a priori, i.e., for any
knowledge that is not given solely in and
through the senses.
{{< /highlight >}}