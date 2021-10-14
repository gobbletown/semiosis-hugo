+++
title = "Imaginary equivalence needs blockchain"
author = ["Shane Mulligan"]
date = 2021-10-14T00:00:00+13:00
keywords = ["openai", "codex", "gpt", "imaginary-programming"]
draft = false
+++

## Related {#related}


### Ontological realists vs Ontological antirealists {#ontological-realists-vs-ontological-antirealists}

See
: [Do Chairs Exist? - YouTube](https://youtu.be/fXW-QjBsruE)

Imaginary hash functions are an attempt at finding ontological
structure within a LM. Ironically, the
ontological realist may struggle to accept `IP` (Imaginary
Programming)'s validity, and the Ontological
antirealist has a flexible enough philosophy
to allow it.


## Summary {#summary}

I create an Imaginary Hash Function (𝑖HF) for
testing equivalence between arbitrary
ontological things within a LM.

It's 'less robust' than a neural hash but has
a larger domain of comparison. Technically speaking, it's not 'less robust'.
Statistically, the confidence does not vary
widely, and may converge, but I haven't
prompted enough to find out. I plan on experimenting with this
in my thesis on `Imaginary Computing`.

The utility and validity of `Imaginary Programming`, and by extension
<span class="underline">Imaginary Hash Functions</span> are emergent qualities in increasingly powerful
language models, and so as the models become stronger, so will the case for `IP`.

An 𝑖HF  may be used for such things as testing
semantic equivalence, relational equivalence,
ideological equivalence, mathematical
equivalence, you name it.

I just want to run predicates for prompt
functions to determine if they should be made
available as functions.

For example, I want to know if the `pf-explain-solidity-code/1` prompt function
should be made available in the current
context.

The current emacs mode may be, for example,
`eww-mode` (the web browser), but if I am browsing a website with some solidity in it,
a decent predicate may decide that I
might actually **want** the `pf-explain-solidity-code/1` function.

Now if I am using the imaginary web browser
`looking-glass`
(<https://semiosis.github.io/looking-glass/>),
using `Pen.el`, I may have dreamed up some
solidity code and would like and explanation
for it.

Let's see if I can do that.


## Analysis {#analysis}

Demonstrated here is that some comparisons
have a very strong mode (`TRUE` or `FALSE`),
where others do not.

This is enough to demonstrate the existence of imaginary hashes, but the next
challenges are a) improving the prompt to get collisions that I expect, and b)
understanding what types of information are
not too disparate to be correlated via the collision function.


### _Moses = Semerkhet_ {#moses-semerkhet}

| **A**                               | **B**                                                             |
|-------------------------------------|-------------------------------------------------------------------|
| "Moses also known as Moshe Rabbenu" | "Semerkhet, the Egyptian king who ruled during the First Dynasty" |

{{< highlight text "linenos=table, linenostart=1" >}}
TRUE TRUE TRUE TRUE TRUE TRUE TRUE the TRUE TRUE TRUE FALSE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE FALSE TRUE TRUE TRUE TRUE TRUE FALSE TRUE TRUE
TRUE
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Confidence: .90
{{< /highlight >}}


### _8 = 10_ {#8-10}

| **A** | **B** |
|-------|-------|
| 8     | 10    |

{{< highlight text "linenos=table, linenostart=1" >}}
FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE TRUE FALSE FALSE FALSE FALSE
FALSE TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE FALSE
FALSE TRUE FALSE FALSE
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Confidence: .16
{{< /highlight >}}


### _Morgan Freeman = God_ {#morgan-freeman-god}

{{< highlight text "linenos=table, linenostart=1" >}}
FALSE FALSE FALSE FALSE FALSE FALSE FALSE
FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE
FALSE FALSE FALSE FALSE FALSE FALSE FALSE
FALSE FALSE FALSE TRUE FALSE FALSE FALSE FALSE
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Confidence: .06
{{< /highlight >}}


### _solidity-mode = Ethereum' language_ {#solidity-mode-ethereum-language}

| **A**         | **B**               |
|---------------|---------------------|
| solidity-mode | Ethereum's language |

{{< highlight text "linenos=table, linenostart=1" >}}
FALSE TRUE TRUE FALSE TRUE TRUE FALSE FALSE
TRUE FALSE FALSE TRUE TRUE FALSE FALSE FALSE
TRUE TRUE FALSE TRUE TRUE FALSE TRUE TRUE TRUE
TRUE FALSE TRUE TRUE TRUE
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Confidence: .60
{{< /highlight >}}

Semantically, they are different, so a weak mode makes sense.


### _Solidity = Ethereum' language for writing smart contracts_ {#solidity-ethereum-language-for-writing-smart-contracts}

| **A**    | **B**                                           |
|----------|-------------------------------------------------|
| Solidity | Ethereum's language for writing smart contracts |

{{< highlight text "linenos=table, linenostart=1" >}}
TRUE FALSE FALSE FALSE TRUE TRUE TRUE TRUE
TRUE FALSE TRUE TRUE FALSE TRUE TRUE TRUE TRUE
TRUE FALSE TRUE TRUE TRUE TRUE FALSE FALSE
TRUE FALSE FALSE TRUE FALSE
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
Confidence: .63
{{< /highlight >}}

I'm less sure about this. I would've expected
it to have a strong mode of TRUE. More
investigate is required.


## Prompt {#prompt}

This is my prompt (1st attempt). Though, it
could be vastly improved, it has demonstrated
hash collisions in imaginary space is possible.

`pf-test-imaginary-equivalence/2`
: <http://github.com/semiosis/prompts/blob/master/prompts/test-imaginary-equivalence-2.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "test imaginary equivalence"
doc: "Given two strings of arbitrary content, test their imaginary equivalence. This is an imaginary neural hash collision test"
aliases:
- imaginary hash collision test
prompt-version: 1
prompt: |+
  <delim>1
  1/0
  <delim>
  is the same as
  <delim>
  ∞
  <delim>
  TRUE because 1 divided by 0 diverges to infinity.

  <delim>2
  "Language is everywhere.

  It permeates our thoughts mediates our
  relations with others, and even creeps into
  our dreams." -Ronald Wayne Langacker
  <delim>
  is the same as
  <delim>
  FTC Puts Hundreds of Businesses on Notice about Fake Reviews (ftc.gov)
  202 points by walterbell 3 hours ago | flag | hide | 92 comments
  <delim>
  FALSE because they are very unrelated.

  <delim>3
  (map
   (fn [x] (+ x 1))
   (range 1 5))
  <delim>
  is the same as
  <delim>
  (map (fn [x] (inc x)) (range 1 5))
  <delim>
  TRUE because `+ x` is equivalent to `inc`.

  <delim>3
  e^{i\pi} + 1 = 0
  <delim>
  is the same as
  <delim>
  euler's identity
  <delim>
  TRUE because euler's identity is the name of the equation e^{i\pi} + 1 = 0.

  <delim>4
  <a>
  <delim>
  is the same as
  <delim>
  <b>
  <delim>

engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "A"
- "B"
examples:
- "Semerkhet, the Egyptian king who ruled during the First Dynasty"
- "Moses also known as Moshe Rabbenu"
filter: on
completion: off
insertion: off
n-collate: 3
n-completions: 10
no-uniq-results: on
results-analyser: pen-analyse-true-or-false
postprocessor: sed 's/^\([a-zA-Z]*\).*/\1/'
{{< /highlight >}}


## Conclusion {#conclusion}

Since this is more than just semantic
similarity, but rather imaginary equivalence,
such a thing needs a consensus mechanism so
that people can write useful software with
large LMs (which is not preventable). Thus
blockchain's value is also not preventable.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Cdcvoe32hbXpXSrAg1eIJ1oqW" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Cdcvoe32hbXpXSrAg1eIJ1oqW.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Cdcvoe32hbXpXSrAg1eIJ1oqW.js" id="asciicast-Cdcvoe32hbXpXSrAg1eIJ1oqW" async></script>