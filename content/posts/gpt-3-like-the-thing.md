+++
title = "GPT-3 is The Thing"
author = ["Shane Mulligan"]
date = 2021-04-14T00:00:00+12:00
keywords = ["gpt-3", "gpt", "openai", "bci"]
draft = false
+++

## Summary {#summary}

The Thing is a lot like GPT-3 due to it being extremely capable, but limited to the queries you give it.

It's as powerful as your understanding on how to talk to it.

The Thing is a black box computer in the `nomes` series by Terry Pratchett.

I make reference to the first book in the series `Truckers`.

This book is freely available on the Internet Archive.

Truckers by Terry Pratchett
: <https://archive.org/details/Book1Truckers/Book%201%20-%20Truckers>


This is the thing
: [Closing to Terry Pratchett's Truckers UK VHS {1997} - YouTube](https://www.youtube.com/watch?v=mrw5UZKyZ1w)

{{< figure src="/ox-hugo/thing-space-age.png" >}}

I remember watching the start of this movie
when I was very young. The movie was taped
over so I only saw first 10 minutes of it, but
the memory stuck since it was so unusual.


### Added to the glossary {#added-to-the-glossary}

{{< highlight text "linenos=table, linenostart=1" >}}
critical path analysis
    It means there's always something you should have done first. For example, if
    you want to build a house you need to know how to make bricks, and before you
    can make bricks you need to know what kind of clay to use. And so on.'
{{< /highlight >}}


## Asking GPT-3 about "The Thing" {#asking-gpt-3-about-the-thing}


### Ask the oracle {#ask-the-oracle}

<a id="code-snippet--question"></a>
{{< highlight text "linenos=table, linenostart=1" >}}
What is the flight computer referred to as "The Thing" in the novel "Truckers" by Terry Prachett?
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
xa upd openai-complete oracle-answer-question
{{< /highlight >}}

```bash
The flight computer is a small, black box that controls the flight of the
spaceship. It is also referred to as the "Thing" because it is the only thing
that can be used to control the spaceship.
```


### What's that word again? What is the study of fictional books? {#what-s-that-word-again-what-is-the-study-of-fictional-books}

The study of fictional books

{{< highlight bash "linenos=table, linenostart=1" >}}
openai-complete what-do-you-call "the study of fictional books" 2>/dev/null
{{< /highlight >}}

```bash
literature
```


### Ask the GPT-3 literature professor what is "The Thing" {#ask-the-gpt-3-literature-professor-what-is-the-thing}

{{< highlight bash "linenos=table, linenostart=1" >}}
xa upd openai-complete tutor "Truckers by Terry Pratchett" literature
{{< /highlight >}}

```bash
The Thing is the computer onboard the giant spaceship "ScreeWee" and it has a very advanced artificial intelligence onboard.
```

<a id="code-snippet--question2"></a>
{{< highlight text "linenos=table, linenostart=1" >}}
Who or what is ScreeWee?
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
xa upd openai-complete tutor "Truckers by Terry Pratchett" literature
{{< /highlight >}}

```bash
You may not remember them as they have been
absent from recent literature. The ScreeWee
are a race of alien beings who are at war with
the humans because of the ScreeWee's lack of
resources in water or land. There is a small
part of the novel, however, that talks about
how the humans are not allowed to attack the
ScreeWee until they have been declared
hostiles. This is because of a treaty that was
signed several centuries earlier.
```

{{< figure src="/ox-hugo/thing-in-hand.png" >}}


## Excerpts from Truckers {#excerpts-from-truckers}

{{< highlight text "linenos=table, linenostart=1" >}}
'I think it wants us to tell it what to do,' said Masklin. He sat back on his heels
and watched the lights.

What can you do?' he said.

'I can translate, calculate, triangulate, assimi-late, -correlate and extrapolate.'

'I don't think we want anything like that,' said Masklin. 'Do we want anything
like that?' he asked the others.

Granny Morkie appeared to think about it. 'No,' she said eventually, 'I don't
think we wants any of that stuff. Another banana'd be nice, mind.'
{{< /highlight >}}

**'I can translate, calculate, triangulate, assimi-late, -correlate and extrapolate.'**

Thoughts on this
: `GPT-3` is based on a language model architecture called the `Transformer`. It transforms text from one form into another. It's capable of performing any type of NLP task. It's very much like the `Thing` because of it's multiple purposes.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
We have the Thing,' he said. 'It will show us the Way, it will.'

Masklin nodded gloomily. Funny how Torrit always knew what the Thing
wanted. It was just a black square thing, but it had some very defi-nite ideas
about the importance of regular meals and how you should always listen to what
the old folk said. It seemed to have an answer for every-thing.

'And where does this Way take us?' said Masklin.

'You knows that well enough. To the Heavens.'

'Oh. Yes,' said Masklin. He glared at the Thing. He was pretty certain that it
didn't tell old Torrit anything at all; he knew he had pretty good hear-ing, and he
never heard it say anything. It never did anything, it never moved. The only
thing it ever did was look black and square. It was good at that.

'Only by followin' the Thing closely in all par-ticulars can we be sure of going
to the Heavens,' said Torrit, uncertainly, as if he'd been told this a long time ago
and hadn't understood it even then.

'Yes, well,' said Masklin. He stood up on the swaying floor and made his way to
the tarpaulin.

Then he paused to screw up his courage and poked his head under the gap.

There- was nothing but blurs and lights,. and strange smells.

It was-all going wrong. It had seemed so sensible that night, a week ago.
Anything was better than here. That seemed so obvious then. But it was odd.
The old ones moaned like anything when things weren't exactly to their liking
but now, when everything was looking bad, they were almost cheerful.

People were a lot more complicated than they looked. Perhaps the Thing could
tell you that, too, if you knew how to ask.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
'I will use small words,' said the Thing. 'I am the Flight Recording and
Navigation Computer. A computer is a machine that thinks. Think, computer,
think. See the computer think. I use elec- tricity. Sometimes elec-tricity can carry
mess-ages. I can hear the messages. I can understand the messages. Sometimes
the messages go along wires called telephone wires. Sometimes they are in other
computers. There is a computer in the Store. It pays humans their wages. I can
hear it think.
{{< /highlight >}}

Thoughts on this
: `GPT-3` is capable of being conversed with and can respond in this style to questions. The way `GPT-3` may communicate as a chatbot is very much like the `Thing` as portrayed in `Truckers`.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
The old man was sitting very still, with the Thing on his lap, staring at nothing.

He paid them no attention when they came in. Once or twice his fingers
drummed on the Thing's black surface.
{{< /highlight >}}

Thoughts on this
: My laptop is a <span class="underline">black</span> `DELL XPS`, which I program with on my lap and it feels very much like I am interfacing with the `Thing`, especially when querying `GPT-3`.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
'Yes. Have you spoken to this, this thing?' The Thing? Well, in a way.

It talks funny, though. It's hard to understand.'

'It has talked to me. It has told me it was made by nomes, a long time ago. It eats
electric. It says it can hear electric things. It has said-' he glared at the thing in
his lap '-it has said that it has heard Arnold Bros (est. 1905) plans to demolish
the Store. It is a mad thing, it talks about stars, it says we came from a star,
flying. But..
{{< /highlight >}}

Thoughts on this
: `GPT-3` is capable of talking about some very out-there topics, and is capable of pushing conversation in the direction of the collective mind known as the internet.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
And so he smiled, and nodded, and listened, and fell asleep as the Thing
talked and talked and talked...
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
And the generations had passed by, and the nomes forgot everything except that
the Thing was very important.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
'The Thing told me,' said Masklin. 'It's some-thing called critical path analysis.
It means there's always something you should have done first. For example, if
you want to build a house you need to know how to make bricks, and before you
can make bricks you need to know what kind of clay to use. And so on.'
{{< /highlight >}}

Thoughts on this
: I will surely be asking `GPT-3` for guidance on what to learn.

{{< figure src="/ox-hugo/thing-stone-age.png" >}}

{{< highlight text "linenos=table, linenostart=1" >}}
'The Thing's a sort of thinking machine, isn't it? That's what Dorcas said. Well,
machines say exactly what they mean, don't they?'
{{< /highlight >}}

Thoughts on this
: You can't judge `GPT-3` for outputting bias statements.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
explicit bias
    [#ai safety]

    Where output is clearly toxic (cursing,
    slander).

implicit bias
    [#ai safety]

    Where the policy from the output changes
    based on context (e.g. opposition to
    programs that help certain groups,
    probabilities about roles).
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
It seemed the only time nomes acted together
was when they had something to occupy their
minds Beyond the moon, the Thing had said. You
used to live in the stars.

Masklin lay back and listened to the bees.

One day we'll go back. We'll find a way to get
to the big ship in the sky, and we'll go back.
But not yet. It'll take some doing, and the
hard part again will be getting people to
understand. Every time we climb up a step we
settle down and think we've got to the top of
the stairs, and start bickering about things.
{{< /highlight >}}

Say no more Terry. Say no more
: It'll take some doing, and the
    hard part again will be getting people to
    understand. Every time we climb up a step we
    settle down and think we've got to the top of
    the stairs, and start bickering about things.


## Book vs mini-series {#book-vs-mini-series}


### Reviews {#reviews}

{{< highlight text "linenos=table, linenostart=1" >}}
Adventure, parable, satire, it is all of those at once.
A wonderful conceit brilliantly realised.
The internal jokes are really finely honed, fit to work on several levels.
A well crafted adventure which should gain an adult audience as well as enthralling children.
{{< /highlight >}}

Promo
: [Terry Pratchett's Truckers 1992 Thames TV Promo - YouTube](https://www.youtube.com/watch?v=nsh-pj7R4mA)


### I transcribe the opening of the Mini-series for you. {#i-transcribe-the-opening-of-the-mini-series-for-you-dot}

[Opening to Terry Pratchett's Truckers UK VHS {1997, V2} {2} - YouTube](https://youtu.be/KEdIRgpCWrM)

{{< figure src="/ox-hugo/truckers-opening.gif" >}}

{{< highlight python "linenos=table, linenostart=1" >}}
This is the story of the Going Home.
This is the story of the Critical Path.

Hello, CK Beta One, proceeding west along Cape
Bridge Road in pursuit of stolen lorry.

We have visual contact.

He's stopping.

Right, that's it for tonight then. You hang on
here. I'll have this comedian.

Right son, you're ...

Gordon Benett, there's noone in there!

Dave!

What?

He's done a runner!

What??

PC all 4832 to CK Beta One.

Go ahead, 4832.

This truck sir, it...
There's no-one driving it.

No, listen sir, listen. Will you listen? There isn't anyone driving it!

What's up Pete?

He's disappeared!

What the?

It must be someone. Let's get after it!
{{< /highlight >}}


### The following passage, for comparison is the opening of the novel. {#the-following-passage-for-comparison-is-the-opening-of-the-novel-dot}

{{< highlight text "linenos=table, linenostart=1" >}}
From The Book of Nome, Basements VI-XII 1

This is the story of the Going Home. This is the story of the Critical Path.

This is the story of the lorry roaring through the sleeping city and out into the
country lanes, smashing through street lamps and swinging from side to side and
shattering shop windows and roll-ing to a halt when the police chased it. And
when the baffled men went back to their car to report Listen, will you, listen?
There isn't anyone driving it!, it became the story of the lorry that started up
again, rolled away from the astonished men, and vanished into the night.
{{< /highlight >}}


### What about this gold nugget at the start of the book? {#what-about-this-gold-nugget-at-the-start-of-the-book}

{{< highlight text "linenos=table, linenostart=1" >}}
One of the shortest-lived creatures on the planet Earth is the adult common
mayfly. It lasts for one day. The longest- living things are bristlecone pine trees,
at 4,700 years and still counting.

This may seem tough on mayflies. But the important thing is not how long your
life is, but how long it seems.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
It's all a sort of relativity. The faster you live, the more time stretches out. To a
nome, a year lasts as long as ten years does to a human.
{{< /highlight >}}

`BCI` anyone?