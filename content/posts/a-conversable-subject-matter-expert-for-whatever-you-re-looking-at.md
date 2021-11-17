+++
title = "A conversable subject-matter-expert for whatever you're looking at!"
author = ["Shane Mulligan"]
date = 2021-11-17T00:00:00+13:00
keywords = ["gpt", "openai", "pen", "apostrophe", "paracosm", "𝑖web", "emacs"]
draft = false
+++

## Summary {#summary}

I make a conversable subject-matter expert for
whatever you're looking at. Right now, that
means your computer. But in future this could
be your eye sight. This uses `Pen.el` as the
kernel.

| kb          | f                                         |           |
|-------------|-------------------------------------------|-----------|
| `M-SPC a c` | `apostrophe-start-chatbot-from-selection` | `pen-map` |


## Demos {#demos}


### Chat with John Dee {#chat-with-john-dee}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/6HLHWqSPWeaOBDQyQqjqahPy1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/6HLHWqSPWeaOBDQyQqjqahPy1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/6HLHWqSPWeaOBDQyQqjqahPy1.js" id="asciicast-6HLHWqSPWeaOBDQyQqjqahPy1" async></script>


### Selecting text, then a subject matter expert from history {#selecting-text-then-a-subject-matter-expert-from-history}

The text selection can be anything (including images in the future).

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/yTm7W1mOduxhQ3qV0lh3FVsO9" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/yTm7W1mOduxhQ3qV0lh3FVsO9.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/yTm7W1mOduxhQ3qV0lh3FVsO9.js" id="asciicast-yTm7W1mOduxhQ3qV0lh3FVsO9" async></script>


### Peter Thiel {#peter-thiel}

It appears that the Codex model makes Peter
Thiel really distrusting, and it's incredibly
difficult to get ideas out of his projected imaginary Codex consciousness / image.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/UmdppEOoKbVszCnb9eW8WHRGk" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/UmdppEOoKbVszCnb9eW8WHRGk.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/UmdppEOoKbVszCnb9eW8WHRGk.js" id="asciicast-UmdppEOoKbVszCnb9eW8WHRGk" async></script>


## Prompts {#prompts}

Two prompts are necessary. Given an arbitrary
text snippet, a list of related scholars needs
to be generated, then a new prompt is needed
to generate a short blurb for that scholar.

All of that goes into `apostrophe`, my
imaginary interlocution chamber.


### `pf-who-is-the-subject-matter-expert-for-/1` {#pf-who-is-the-subject-matter-expert-for-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Who is the subject matter expert for?"
doc: "Given a prompt, be told of a subject-matter-expert."
prompt-version: 1
todo:
- |-
    Instead of having a document, have an
    entire multi-modal prompts array (time-
    wise)? The document should be 'multi-modal
    prompt format', whatever that is.

# TODO Make a list of scholars and qualifications relating to a document

prompt: |+
  Document 1:
  ###
  Truth is the property of being in accord
  with fact or reality.[1] In everyday
  language, truth is typically ascribed to
  things that aim to represent reality or
  otherwise correspond to it, such as beliefs,
  propositions, and declarative sentences.[2]
  ###
  Document 1 scholars:
  - 1. Kant (1724-1804)
  - 2. Hegel (1770-1831)
  - 3. Schopenhauer (1788-1860)
  - 4. Kierkegaard (1813-1855)
  - 5. Nietzsche (1844-1900)
  ###
  Document 2:
  ###
  <document>
  ###
  Document 2 scholars:
  <:pp>- 1.
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
delimiter: "###"
frequency-penalty: 0.3
stop-sequences:
- "<delim>"
- Document
cache: on
vars:
- document
var-defaults:
- "(pen-screen-or-selection)"
# no EOD? needed or default?
preprocessors:
- pen-str garbify | sed 's/^/  /'
examples:
- |-
    In Abrahamic religions, the Garden of Eden
    (Hebrew: גַּן־עֵדֶן‎ – gan-ʿḖḏen) or Garden of
    God (גַּן־יְהֹוָה‎ – gan-Yhwh), also called
    the Terrestrial Paradise, is the biblical
    paradise described in Genesis 2-3 and Ezekiel
    28 and 31.[1][2] The location of Eden is
    described in the Book of Genesis as the source
    of four tributaries. Among scholars who
    consider it to have been real, there have been
    various suggestions for its location:[3] at
    the head of the Persian Gulf, in southern
    Mesopotamia (now Iraq) where the Tigris and
    Euphrates rivers run into the sea;[4] and in
    Armenia.
validator: "grep -qP '^- [0-9]*\\. .*\\(.*\\)$'"
postpostprocessor: "sed 's/^- [0-9]*. //' | sed 's/^\\s\\+/ /g' | tr -d ' '"
info: on
filter: off
completion: off
insertion: off
end-split-patterns:
- "\n"
{{< /highlight >}}


### `pf-generate-wiki-blurb-for-a-famous-person/1` {#pf-generate-wiki-blurb-for-a-famous-person-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate wiki blurb for a famous person"
doc: "Create a wiki blurb for a famous person"
prompt-version: 1
prompt: |+
    <person> - Wikipedia

    en.wikipedia.org › wiki › <person>

    <:pp><person> was a
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 200
top-p: 1.0
stop-sequences:
- "."
cache: on
vars:
- "person"
examples:
- "John Dee"
postprocessor: pen-str join ' ' | sed -z 's/\s*\n$//' | sed -z 's/[^a-zA-Z0-9]*$//' | sed -z 's/$/./'
info: on
filter: off
completion: off
insertion: off
{{< /highlight >}}