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


## Prompt {#prompt}


### `pf-fact-checker/1` {#pf-fact-checker-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Given a claim, determine if it is true or false"
title: "Fact checker"
prompt-version: 1
prompt: |+
  For each claim, answer true or false:
  <delim>
  Claim: 2 + 2 = 4
  This is objectively 'True', because it is a mathematical truth.
  <delim>
  Claim: "In 2018, Donald Trump was the President of the United States."
  This is objectively 'True', because we have record of it.
  <delim>
  Claim: "The following statement is untrue: In 2018, Donald Trump was the President of the United States."
  This is objectively 'False', because the true statement was negated.
  <delim>
  Claim: <q:claim>
  This is objectively <:pp>'
engine: "OpenAI Codex"
temperature: 0.3
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
info: on
filter: off
no-uniq-results: on
completion: off
insertion: off
preprocessors:
- pen-str join ' '
{{< /highlight >}}


## Demo {#demo}


### Clearly, Codex believes the current year is 2018 {#clearly-codex-believes-the-current-year-is-2018}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/vcDfWMGcx7TjK40T0J59yIwoH.js" id="asciicast-vcDfWMGcx7TjK40T0J59yIwoH" async></script>


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