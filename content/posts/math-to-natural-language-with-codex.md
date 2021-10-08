+++
title = "Math to natural language with Codex"
author = ["Shane Mulligan"]
date = 2021-10-08T00:00:00+13:00
keywords = ["codex", "openai", "gpt"]
draft = false
+++

## Summary {#summary}

I make a prompt which translates math into NL,
and states what area of math it comes from.


## Prompt {#prompt}

`pf-translate-math-into-natural-language/1`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Translate math into natural language"
doc: "Translate math into natural language"
prompt-version: 1
prompt: |+
  Math equations below:
  <delim>
  Math: ∀x ∈ D, P(x)
  Description: For all x in the domain, P(x) is true
  Topic: Predicate calculus
  <delim>
  Math: sqrt(x^2 + 1)
  Description: Take the square-root of (x to the power of 2 plus 1).
  Topic: Algebra
  <delim>
  Math: ∃𝑥 ∈ D, P(x)
  Description: There exists x in the domain, such that P(x) is true
  Topic: Predicate calculus
  <delim>
  Math: <equation>
  Description:
engine: "OpenAI Codex"
temperature: 0.3
top-p: 1.0
cache: on
n-collate: 1
n-completions: 5
stop-sequences:
- "<delim>"
vars:
- "equation"
preprocessors:
- pen-str onelineify
postprocessor: "sed -z 's/\\nTopic: \\(.*\\)\\n.*/ (\\1/' | sed 's/$/)/'"
examples:
  - |-
    P: "Roofus is a mammal"
    P(x): "x is amammal"
    Q: ∀x ∈ D, P(x): "every dog is a mammal"
info: on
filter: no
completion: on
insertion: on
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/N40PvJiRtBEXrJBa6hEEBWDj0" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/N40PvJiRtBEXrJBa6hEEBWDj0.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/N40PvJiRtBEXrJBa6hEEBWDj0.js" id="asciicast-N40PvJiRtBEXrJBa6hEEBWDj0" async></script>

Input:

{{< highlight text "linenos=table, linenostart=1" >}}
P: "Roofus is a mammal"
P(x): "x is amammal"
Q: ∀x ∈ D, P(x): "every dog is a mammal"
{{< /highlight >}}

Output (4 answers):

{{< highlight text "linenos=table, linenostart=1" >}}
P is a proposition that Roofus is a mammal. P(x) is a proposition that x is a mammal. Q is a proposition that every dog is a mammal. (Predicate calculus)
P is a sentence that defines a property, P(x) is a sentence that defines a property of x (Predicate calculus)
P is a statement, P(x) is a predicate, and Q is a statement that is true for every x in the domain (Predicate calculus)
P is a sentence that is true for all x in the domain, Q is a sentence that is true for all x in the domain, such that P(x) is true (Predicate calculus)
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
sqrt(x^2 + 1)
{{< /highlight >}}