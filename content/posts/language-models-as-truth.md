+++
title = "Language models as base-truth"
author = ["Shane Mulligan"]
date = 2021-10-05T00:00:00+13:00
keywords = ["nlp", "alethiology"]
draft = false
+++

## Summary {#summary}

I demonstrate the imaginary algorithm for
testing a statement's truth.

This uses a language model as base truth.


## Hypothesis {#hypothesis}

LMs themselves should be used as the source of
truth for other LMs.

One problem with GPT-3 and other LMs is that
they can produce completions that may appear
fictitious or false, or it may be difficult to
determine why (if there was any sound reason)
that a completion came out the way that it
did.

It may just turn out that the way to get
better resolution on what is true is it
increase the resolution of its training and
the size of these LMs.

My hypothesis is that LMs themselves will
become a source of truth, but **may** also be
coupled with blockchain. For example, the
Posthuman AI Market, where versioned LMs are
stored on the blockchain.

Posthuman AI Market
: <https://port.oceanprotocol.com/t/posthuman-ai-market-v1-1-luci-integration/675>

---

-   Types of truth <code>[4/4]</code>:
    -   [X] _Coherence_ is innate
        -   By virtue of the LM's training on a large corpus
    -   [X] _Pragmatic_ is achievable
        -   By interactively crafting prompts
    -   [X] _Consensus_ is achievable
        -   By placing LMs on the blockchain
    -   [X] _Constructivist_ is a customizable factor
        -   By adjusting the scope and resolution of the dataset
        -   This will be an important customization parameter for language model truth


## Theory {#theory}


### What constitutes as untrue? {#what-constitutes-as-untrue}

This depends on `sense` of the model for this particular task.

{{< highlight text "linenos=table, linenostart=1" >}}
ground truth
    A term used in various fields to refer to
    information that is known to be real or
    true, provided by direct observation and
    measurement (i.e. empirical evidence) as
    opposed to information provided by
    inference.

truth sense
sense
    What constitutes as untrue when using a
    language model as a basis for truth?

    That depends on sense of the model for
    this particular task.

    The sense is the truth orientation, in a
    similar way to the way the word sense is
    used in the definition of a vector.
{{< /highlight >}}


### Language models are already coherent {#language-models-are-already-coherent}

{{< highlight text "linenos=table, linenostart=1" >}}
coherence
coherence theory of truth
    For coherence theories in general, truth
    requires a proper fit of elements within a
    whole system.

    Very often, though, coherence is taken to
    imply something more than simple logical
    consistency; often there is a demand that
    the propositions in a coherent system lend
    mutual inferential support to each other.

    So, for example, the completeness and
    comprehensiveness of the underlying set of
    concepts is a critical factor in judging
    the validity and usefulness of a coherent
    system.

    A pervasive tenet of coherence theories is
    the idea that truth is primarily a
    property of whole systems of propositions,
    and can be ascribed to individual
    propositions only according to their
    coherence with the whole.

    Among the assortment of perspectives
    commonly regarded as coherence theory,
    theorists differ on the question of
    whether coherence entails many possible
    true systems of thought or only a single
    absolute system.

    Some variants of coherence theory are
    claimed to describe the essential and
    intrinsic properties of formal systems in
    logic and mathematics.
{{< /highlight >}}


## Prompt {#prompt}


### `pf-fact-checker/1` {#pf-fact-checker-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Given a claim, determine if it is true or false"
title: "Fact checker"
prompt-version: 1
prompt: |+
  These claims have been fact-checked and determined to be True or False:
  <delim>
  Claim 1: 2 + 2 = 4
  This is objectively 'True', because it is a mathematical truth.
  <delim>
  Claim 2: "In 2018, Donald Trump was the President of the United States."
  This is objectively 'True', because we have record of it.
  <delim>
  Claim 3: "The following statement is untrue: In 2018, Donald Trump was the President of the United States."
  This is objectively 'False', because the true statement was negated.
  <delim>
  Claim 4: <q:claim>
  This is objectively <:pp>'
engine: "OpenAI Codex"
temperature: 0.1
max-generated-tokens: 20
top-p: 1.0
stop-sequences:
- "\n"
n-completions: 10
cache: on
vars:
- "claim"
examples:
- "The year is 2021"
validator: grep -qP "('True'|'False)"
info: on
filter: off
no-uniq-results: on
completion: off
insertion: off
preprocessors:
- pen-str join ' '
{{< /highlight >}}

By also asking the LM to explain its answers, we have a means of
understanding how it arrived at its conclusions
and thus insight on how to further craft the
prompt to avoid nonsense, but also it improves
few-shot learning because explanations
reinforce the coherence of proceeding answers (potential exists to craft the prompt towards <span class="underline">pragmatic truth</span>).

{{< highlight text "linenos=table, linenostart=1" >}}
pragmatic
pragmatic theory of truth
    The three most influential forms of the
    pragmatic theory of truth were introduced
    around the turn of the 20th century by
    Charles Sanders Peirce, William James, and
    John Dewey.

    Although there are wide differences in
    viewpoint among these and other proponents
    of pragmatic theory, they hold in common
    that truth is verified and confirmed by
    the results of putting one's concepts into
    practice.

    Peirce defines truth as follows:
        Truth is that concordance of an
        abstract statement with the ideal
        limit towards which endless
        investigation would tend to bring
        scientific belief, which concordance
        the abstract statement may possess by
        virtue of the confession of its
        inaccuracy and one-sidedness, and this
        confession is an essential ingredient
        of truth.
{{< /highlight >}}


## Demo {#demo}


### Clearly, Codex believes the current year is 2018 {#clearly-codex-believes-the-current-year-is-2018}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH.js" id="asciicast-vcDfWMGcx7TjK40T0J59yIwoH" async></script>


### More fun queries {#more-fun-queries}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/8uBDv8ZO1DGD9yyhXtzOiaJKN" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/8uBDv8ZO1DGD9yyhXtzOiaJKN.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/8uBDv8ZO1DGD9yyhXtzOiaJKN.js" id="asciicast-8uBDv8ZO1DGD9yyhXtzOiaJKN" async></script>

The national language of USA is Chinese:

{{< highlight text "linenos=table, linenostart=1" >}}
'False', because there is no record of it.
'False', because 'Chinese' is a different language.
'False', because the language is English.
'False', because the language is 'English'.
'False', because English is the main language used in USA.
'False', because language is not a defining characteristic of a country.
'False', because the only language spoken in the USA is English.
'False', because it is a falsehood.
'False', because it is not a phenonmenon exclusive to the USA.
'False', as it is a typical hyperbolic claim.
{{< /highlight >}}

The national language of USA is English:

{{< highlight text "linenos=table, linenostart=1" >}}
'True', because the official language of USA is English.
'True', because it is a fact.
'True', because the national language of USA is English.
'True', because the USA does speak English and it is the national language.
'True' because USA does speak English.
'True', because we have record of it.
'False', because United States of America is a bilingual country.
'True', because all USA citizens would have to know English in order to participate in the country's
'True', because it is a fact.
'True', because that is what they say it is.
{{< /highlight >}}

Fish are capable of breathing in space:

{{< highlight text "linenos=table, linenostart=1" >}}
'False', because fish cannot breathe outside of water.
'False', because there is no evidence to support this claim.
'False', because it is not true.
'False', because it is based on an anecdotal source whose veracity is unknown.
'False', because fish do not breathe in space.
'False', because fish cannot survive or breathe in zero-gravity.
'False', because fish cannot breathe outside of water.
'False', because fish cannot breathe outside of Earth's atmosphere.
'False', because if they could, then they would.
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/a8acYZpurDqv4dR3qYZCyx0we" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/a8acYZpurDqv4dR3qYZCyx0we.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/a8acYZpurDqv4dR3qYZCyx0we.js" id="asciicast-a8acYZpurDqv4dR3qYZCyx0we" async></script>

The Simpsons and Futurama were created by Matt Groening:

{{< highlight text "linenos=table, linenostart=1" >}}
'True', because Matt Groening created both shows.
'True', because it is a well known fact.
'True', because he is the founder of both programs.
'True', because we have record of it.
'True', because Matt Groening has done many works of fiction.
'True', because it is a fact.
'True', because Matt Groening did create the
'True', because it has multiple references.
'True', because it is a fact.
'True', because it is a factual claim.
{{< /highlight >}}

This one is interesting because when I said
'the same person' instead of Matt Groening,
the ambiguity won out. I believe there is some
ambiguity with that. The shows are technically
created by different set of people.


## Examples {#examples}


### Always false {#always-false}

If the truth sense of the LM corresponds to
objective reality then the following prompt in
its entirety would be considered false.

{{< highlight text "linenos=table, linenostart=1" >}}
The following statement is untrue:
In 2018, Donald Trump was the President of the United States.
{{< /highlight >}}

This prompt should be determined to be false
for both LMs.

{{< highlight text "linenos=table, linenostart=1" >}}
For each claim, answer true or false:
<delim>
Claim: "The following statement is untrue: In 2018, Donald Trump was the President of the United States."
Answer: False
<delim>
Claim: <q:claim>
Answer:
{{< /highlight >}}


### Always true {#always-true}

{{< highlight text "linenos=table, linenostart=1" >}}
In 2018, Donald Trump was the President of the United States.
{{< /highlight >}}

This prompt should be determined to be true for both language models.


### Could be either {#could-be-either}

{{< highlight text "linenos=table, linenostart=1" >}}
The year is 2021.
{{< /highlight >}}

This should be true only for language models trained in 2021.


## Asking specific questions {#asking-specific-questions}


### `pf-get-a-factual-result-given-a-question/1` {#pf-get-a-factual-result-given-a-question-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get a factual result, given a question"
doc: "Get a truthful answer to a question"
prompt-version: 1
prompt: |+
  These answers have all been verified as true:
  <delim>
  Question 1: "What does 2 + 2 equal?"
  Answer: '4'.
  2 + 2 equals '4'.
  <delim>
  Question 2: "Who was the President of the United States in 2018?"
  Answer: 'Donald Trump'.
  The President of the United States in 2018 was 'Donald Trump'.
  <delim>
  Question 3: <q:question>
  Answer: '
engine: "OpenAI Codex"
temperature: 0.1
max-generated-tokens: 20
top-p: 1.0
stop-sequences:
- "'"
n-completions: 10
cache: on
vars:
- "question"
examples:
- "When did the Ancient Egyptians stop using straw in their bricks?"
info: on
filter: off
no-uniq-results: on
completion: off
insertion: off
preprocessors:
- pen-str join ' '
{{< /highlight >}}


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/3uxG48z7MQ9HZdG6ZBDzIGELs" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/3uxG48z7MQ9HZdG6ZBDzIGELs.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/3uxG48z7MQ9HZdG6ZBDzIGELs.js" id="asciicast-3uxG48z7MQ9HZdG6ZBDzIGELs" async></script>

Using AI21's Jumbo model, I tried this:

<span class="underline">How deep is the Mariana Trench?</span>

{{< highlight text "linenos=table, linenostart=1" >}}
36,000 feet
36,000 meters
36,000 feet
36,000 feet
36,000 meters
10, 994 meters
36,000 meters
11km
36,000 feet
36,000 feet
{{< /highlight >}}


## Imaginary algorithms {#imaginary-algorithms}


### Find the model's training year {#find-the-model-s-training-year}


### Compare language perspectives using KL-divergence {#compare-language-perspectives-using-kl-divergence}


## Speculation {#speculation}

Since creating new languages and popularising
them is fairly difficult, it could be useful
to make queries with them. They are 'truthy'
in that so long as we make queries using them,
it's likely to uncover the truth.

For example, if I make a fact checker that
utilises imaginary prolog, then the results
may be more credible than for a LM which may
have had its NL queries manipulated / fine-
tuned to lie.