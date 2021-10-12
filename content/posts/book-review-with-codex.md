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


### Question {#question}

How are results weighted roughly you think? If
results are weighted hugely popularity like in
search engines then it would be hard for
openAI to generate or create anything beyond
the general consensus?


### Answer {#answer}

When it comes to very popular books, you're
probably correct.

'How' is an incredibly difficult question to
answer.

Misinformation can easily sneak into the
results.

The prompt is designed to be inspired by book
reviews, because I placed a URL at the top of
it that looks like it's from a website that
contains book reviews, so the review will be
skewed towards popular opinion like you say,
probably, but inevitably will also factor in
from other sources, including non-related or
similarly looking material.

Related
: <https://openai.com/blog/summarizing-books/>

As for the technique that openai used to
summarize the book, that's very different.

That actually makes a synopsis based on mostly
unbiased summarization.


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


## A book from 2018 {#a-book-from-2018}

The generated synopses appear to be purely fictional.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/pXekqTccDRIj9lqTprfEOveXu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/pXekqTccDRIj9lqTprfEOveXu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/pXekqTccDRIj9lqTprfEOveXu.js" id="asciicast-pXekqTccDRIj9lqTprfEOveXu" async></script>


### Anything is Possible - Elizabeth Strout {#anything-is-possible-elizabeth-strout}

Real synopsis:

{{< highlight text "linenos=table, linenostart=1" >}}
Short story collection Anything Is Possible
explores the whole range of human emotion
through the intimate dramas of people
struggling to understand themselves and
others.

Here are two sisters: one trades self-respect
for a wealthy husband while the other finds in
the pages of a book a kindred spirit who
changes her life.
{{< /highlight >}}

Imagined synopses:
`1`)

{{< highlight text "linenos=table, linenostart=1" >}}
In her stunningly beautiful début novel,
Elizabeth Strout takes a truth universally
acknowledged: a poor young woman's chances of
making a good life for herself are nearly
nonexistent.

But in a small town in upstate New York during
the 1960s, Ann Leckie seems to have more than
the sum of what most people are given.

Ann is cheerfully beautiful, available, smart,
and hardworking.

Prettiness, in fact, is her best quality, and
she knows it.

So when she wins a scholarship to a boarding
school, she thinks she has a real chance to
make a better life for herself.
{{< /highlight >}}

`2`)

{{< highlight text "linenos=table, linenostart=1" >}}
Anything is Possible captures the power of
possibility -- that is, the idea that nothing
is certain, and that life always gives us a
new shot.

It is the story of two young girls -- Lia, a
scrapper from the Boston projects, and her
older sister Em, an up-and-coming lawyer whose
potential is limited -- who find each other
and form an unlikely friendship.

Lia's father walked out on her family, leaving
them to survive on social security and their
wits.

Em's father is in jail, possibly unjustly, and
she has few opportunities for work.

They are not supposed to be friends.

They are supposed to be kind to each other.
{{< /highlight >}}

Try by emulating goodreads:

<https://www.goodreads.com/book/show/32080126-anything-is-possible>

{{< highlight text "linenos=table, linenostart=1" >}}
Eighteen-year-old Lydia is no longer able to
be her own person.

Her father's sudden death has left Lydia with
her mother, her brother, her grandmother, and
her own thoughts.

Lydia can't hope for more than a passing
friendship with Max, who has yet to
acknowledge her existence.

But when Lydia is given the chance to attend
the prestigious Emma B.

Schreiber Academy, she sees a world of new
opportunities.
{{< /highlight >}}