+++
title = "Examplary"
author = ["Shane Mulligan"]
date = 2021-03-17T00:00:00+13:00
keywords = ["nlp", "openai"]
draft = false
+++

`Examplary` is an example-oriented DSL that can be used to construct and
compose functions based on prompts and external `String->String` commands.

Using examplary, one can keep track of their various `String->String` functions
but also:

-   generate prompts from examples and an example generator

Here are some examples:

{{< highlight clojure "linenos=table, linenostart=1" >}}
(def regex
  "example 1\nexample2" "^example [12]$"
  "example 2\nexample3" "^example [23]$"
  "pi4\npi5" "^pi[45]$")

(def analogy
  ;; Each line is a training example.
  "Neural networks" "Neural networks are like genetic algorithms in that both are systems that learn from experience"
  "Social media" "Social media is like a market in that both are systems that coordinate the actions of many individuals.")

(def field
  "chemistry" "study of chemicals"
  "biology" "study of living things")
{{< /highlight >}}


## Related links {#related-links}

-   Code: <http://github.com/semiosis/examplary>