+++
title = "Describing Melee's Paintings with AlephAlpha"
author = ["Shane Mulligan"]
date = 2021-11-02T00:00:00+13:00
keywords = ["mm", "אα", "surreal", "art", "melee", "pen", "symbolism"]
draft = false
+++

## Summary {#summary}

I take AlephAlpha's multimodal LM for a spin
to see how well it does surreal art
interpretation.

AlephAlpha
: [https://aleph-alpha.de/techblog/95\_the\_end\_of\_the\_era\_imagenet](https://aleph-alpha.de/techblog/95%5Fthe%5Fend%5Fof%5Fthe%5Fera%5Fimagenet)


Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/given-some-surreal-art-and-a-description-elaborate-1.prompt>


Engine
: <http://github.com/semiosis/engines/blob/master/engines/alephalpha-eu-tran-mm.engine>


Pen.el
: <https://github.com/semiosis/pen.el/>


### This is how to make a multi-modal prompt payload {#this-is-how-to-make-a-multi-modal-prompt-payload}

`pf-given-some-surreal-art-and-a-description-elaborate/1`

{{< highlight yaml "linenos=table, linenostart=1" >}}
...
payloads:
# This could be a raw string to a file or URL, or an elisp function
# - image: "(read-file-name \"image 2 of 2: \")"
- image: "(read-file-name \"image: \")"
prompt: |+
  <description> Furthermore,
engine: "AlephAlpha EUTranMM"
...
{{< /highlight >}}


## Rose Mannequin {#rose-mannequin}

{{< figure src="/ox-hugo/148658560_2839287366296108_857180560792297037_n.jpg" >}}


### My description {#my-description}

In this surreal artwork, flowers confidently stand from the sawn-off head of a
ceramic mannequin. Water endlessly surfaces
from the neck and quietly overflows from the lip. The arms
and hands delicately embrace each other with attention towards the beholder in a sign of defencelessness and trust. The offering is attention, beauty and conversation.
A dichotomy of insecurity with innocence is outweighed by confidence.


### Continuations {#continuations}

`Furthermore,` (temperature=0.0 i.e. fully deterministic)
the flowers are a symbol of the human soul.

`Furthermore,` (temperature=0.1)
the flowers are a symbol of life and growth.

`Furthermore,` (temperature=0.3)
the flowers are a symbol of the human soul and the human spirit.

`Furthermore,` (temperature=0.8)
a subtle harmonization of the positive and negative sides seems to resonate in this simple and yet sophisticated work of art.

`Furthermore, the water represents` (temperature=0.3)

-   Furthermore, the water represents the human of life and which is the source of life that is beautiful.
-   Furthermore, the water represents the life of life and symbolizes life is beautiful and
-   Furthermore, the water represents the soul of life.
-   Furthermore, the water represents the water of life, which is the essence of all that is beautiful.

-   Thoughts:
    -   I am amazed that it is able to infer that the flowers represent the human soul and spirit.
    -   It seems to have become a little hung up on that though, and makes similar conclusions about the water.


### Description by AlephAlpha `EUTranMultimodal` {#description-by-alephalpha-eutranmultimodal}

`In this surreal artwork, flowers`

Without any context, the model has greater difficulty.

-   In this surreal artwork, flowers bloom in the mouth of a woman's head.
-   In this surreal artwork, a plant is sprouting from a woman's head.
-   In this surreal painting, a woman holds a flower in her head, which grows out of her mouth.
-   In this surreal artwork, the water represents the human emotions and thoughts, and the plant is the brain.


### Questions {#questions}

-   Who does this person represent?
    -   A person who is in love with the world.
-   Is this an accurate description?
-   What else could be said about the mannequin?
-   What types of flowers are there?
    -   Roses.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/4r7x5nIcHUYU86i1oQZrUdyqy" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/4r7x5nIcHUYU86i1oQZrUdyqy.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/4r7x5nIcHUYU86i1oQZrUdyqy.js" id="asciicast-4r7x5nIcHUYU86i1oQZrUdyqy" async></script>


## Fiery red girl {#fiery-red-girl}

{{< figure src="/ox-hugo/148751110_885252488961991_2593863030122457822_n.jpg" >}}


### AlephAlpha interpretation {#alephalpha-interpretation}

-   `In this surreal artwork,` I tried to capture the feeling of a woman with a long red hair in a dream-like state.
-   `What has happened to this girl's face?` I's been eaten by the demon and...


### My interpretation {#my-interpretation}

_**Theory 1: chameleon canvas**_

A girl believes she must prepare for her photo
to be taken. So she wears a low cut, frilly
garment, and splays her fiery red hair, which
she put great effort into, and is so proud of.
She poses for the viewer so they can see.

She doesn't know that she is actually an
intelligent canvas, which genuinely believes
it is a girl, but is unable to look back upon
its own face, and so has no idea that it needs
to construct one. As observers, we can see it
for what it really is.

The canvas is just a chameleon, that believes
what we program it to believe, and does its
best to take that form. We can make it think
it's a girl or a cat, but we don't want it to
be able to see itself because then it would
have self-awareness.

-   Continuations:
    -   `Furthermore,` it doesn't understand the difference between itself and its surroundings.
    -   `Furthermore,` we can make it think it's a person, but we don't want it to know what we are doing, because then it would have consciousness.

A girl believes she must prepare for her photo
to be taken. So she wears a low cut, frilly
garment, and splays her fiery red hair, which
she put great effort into, and is so proud of.
She poses for the viewer so they can see.

-   Continuations:
-   `Furthermore,` she has her face turned towards the camera, and she is staring directly at the viewer, who is supposed to feel as if she is being stared down by a scary monster.
-   `Furthermore,` she is a ghost monster looking directly at the viewer, as if she is staring at the viewer to see if they are scared.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/X4Iidg5serY9bkMUnoHZet5TA" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/X4Iidg5serY9bkMUnoHZet5TA.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/X4Iidg5serY9bkMUnoHZet5TA.js" id="asciicast-X4Iidg5serY9bkMUnoHZet5TA" async></script>

_**Theory 2: hasty ouroboros**_

Despite the lack of a face, the viewer can
still tell that this was actually a person, by
the effort she took in perfecting her hair,
since it speaks for her though she cant make
words.

The black entity that removes her face is not
in her control. She is always aware of it and
so in a race against time, she prepares
herself for the meaning of her life; to show
the viewer she is real, and barely managed to
at that.

And as she tries to complete the picture, her
face becomes her tail, but she's mistakenly convinced its the
hand of the black canvas that is compelled to
enter the frame and redact her image because
it was too great a violation since she is not
meant to be real.

Her hair is like a flag in battle, defiant to
the last moment. But the enemy is herself, or
what she's made of, non-reality.

While she might never be able to take full
form she will always try.

-   Continuations:
    -   `Furthermore, her image is not her only form.`

Despite the lack of a face, the viewer can
still tell that this was actually a person, by
the effort she took in perfecting her hair,
since it speaks for her though she cant make
words.

-   Continuations:
    -   `Furthermore, she is holding a book, and it seems to be an important one.`

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/rRxGzvZsmHAuoia0bVgO0u8I0" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/rRxGzvZsmHAuoia0bVgO0u8I0.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/rRxGzvZsmHAuoia0bVgO0u8I0.js" id="asciicast-rRxGzvZsmHAuoia0bVgO0u8I0" async></script>

Despite the lack of a face, the viewer can
still tell that this was actually a person.

-   Continuations:
    -   `Furthermore,` this viewer has made a figure's hair look like a spider web
    -   `Furthermore,` the viewer has managed sure person look eyes look like a human web
    -   `Furthermore,` the artist has made this person's hair look like a spider.
    -   `Her hair represents` the hair of a person, and the eyes is red of the ghost.
    -   `Her hair symbolizes` the power, her eyes hair representsizes her blood that her earth.
    -   `Her hair symbolizes` her soul and her red hair symbolizes the blood of the innocent.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/FJwJoKz72fPY9PpDkWCoZzr7X" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/FJwJoKz72fPY9PpDkWCoZzr7X.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/FJwJoKz72fPY9PpDkWCoZzr7X.js" id="asciicast-FJwJoKz72fPY9PpDkWCoZzr7X" async></script>

----

-   Thoughts:
    -   Abstract art is hard. The model appears to be drawn to concepts like a spider web, which is understandable.


## Red Ribbon {#red-ribbon}

I want to use this to experiment with tri-modal
prompts (Text + Image + Image) on a bimodal
modal using prompt multiplexing and/or
composition.


### Description {#description}

One literal interpretation
: A strong black
    woman wears a ribbon over her eyes and
    comfortably tucked behind her ears, as if
    wearing it is part of her routine, in full
    light of day.

    The ribbon is supposedly harmless and she
    can see through it, but she values its
    direction more than her own.

    Her hair is neatly tied so the ribbon is
    unobstructed to prompt her whichever way to
    look or rather, way to walk.

    She doesn't know who holds the ends or who
    gives her direction. She trusts the ribbon.

----

-   Easy questions I could ask the AI:
    -   What is the name of this artwork?
    -   What does the full picture look like?


### _**Quasi-blind**_ {#quasi-blind}

{{< figure src="/ox-hugo/149610795_852720085510722_7762625424216125088_n.jpg" >}}

-   Question AlephAlpha: `What does the ribbon represent?`
    -   It's the ribbon of the flag, representing the unity of the country and the people. It's a reminder that we are all one.

-   Question AlephAlpha: `What is in this image?` (with help from GPT-3 to correct spelling)
    -   Drawing of a person who is a young man with a black bandana on his head.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/jFUeSbaCqmGRiK8hKhNMWlZFn" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/jFUeSbaCqmGRiK8hKhNMWlZFn.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/jFUeSbaCqmGRiK8hKhNMWlZFn.js" id="asciicast-jFUeSbaCqmGRiK8hKhNMWlZFn" async></script>


### _**Quasi-bound**_ {#quasi-bound}

{{< figure src="/ox-hugo/148957104_4062416750455469_4718804921220743845_n.jpg" >}}

-   Question AlephAlpha: `What does the ribbon represent?`
    -   The ribbon represents the love of my family and friends and how much I love them.

---

-   Questions I have:
    -   Is she aware that she is tangled in the ribbon?
    -   What does the ribbon represent?
    -   What is the significance to partial blindness and partial boundness?
        -   What is the significance of the hands and the head?
    -   Describe these images in a few sentences each.
        -   Expecting to get the most relevant or obvious concepts/entities/symbols for each image.
    -   What are the most relevant symbols?
        -   Expecting the AI to associate the imagery with symbolism (i.e. referring to the signified).


### Prompting {#prompting}

_**Possible interpretation**_

A free and strong black woman uses a red
ribbon to help her to see and sense. She is
voluntarily bound because she trusts the
ribbon more than her immediate surroundings.

She doesn't know who holds the ends. Is this
slavery or freedom?


### Experiments {#experiments}

-   Can I multiplex images?
    -   Use ImageMagick to position them side-by-side.
-   Can I multiplex prompt questions?
    -   Essentially, I want to test multiple image
        and text prompts for a bi-modal model.


### Symbolism {#symbolism}