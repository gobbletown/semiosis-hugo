+++
title = "Guess the function name with Codex"
author = ["Shane Mulligan"]
date = 2021-10-09T00:00:00+13:00
keywords = ["codex", "openai", "gpt"]
draft = false
+++

## Summary {#summary}

I make a prompt for guessing or coming up with
a good name for a function or algorithm, given
only the algorithm and not necessarily the
names of the substituent functions.

This is useful after, say using `pf-explain-
some-code/2`, where the algorithm is described
in NL.


## Prompt {#prompt}

`pf-name-a-function-given-a-description-or-code/1`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "name a function, given the description or code"
doc: "Given a function or a description of a function, come up with a good name for it"
title: guess function name
prompt-version: 1
prompt: |+
  List of algorithms:
  <delim>
  Function or algorithm:
  <markdown-delim>
  (defun f (n)
    (if (= n 0) 0
      (if (= n 1) 1
        (+ (f (- n 1)) (f (- n 2))))))
  <markdown-delim>
  Algorithm name: Generate the fibonacci sequence
  Function name: fib
  <delim>
  Function or algorithm:
  <markdown-delim>
  <algorithm or description>
  <markdown-delim>
  Algorithm name:
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
n-completions: 10
stop-sequences:
- "<delim>"
cache: on
# TODO Make it so vals are also substituted
defs:
- markdown-delim: "```"
vars:
- "algorithm or description"
examples:
# quicksort
- |
  f :: (Ord a) => [a] -> [a]
  f [] = []
  f (x:xs) =
    let smaller = f [a | a <- xs, a <= x]
        bigger = f [a | a <- xs, a > x]
    in  smaller ++ [x] ++ bigger
info: on
filter: off
completion: off
insertion: off

# TODO Make the ability for a prompt function to return a struct.
# By slicing up the results.

postprocessor: "sed 1d | sed 's/Function name: //'"
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/NUBZLBoJgDaq7xmlBUnNIQDAk" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/NUBZLBoJgDaq7xmlBUnNIQDAk.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/NUBZLBoJgDaq7xmlBUnNIQDAk.js" id="asciicast-NUBZLBoJgDaq7xmlBUnNIQDAk" async></script>

<a id="code-snippet--quicksort"></a>
{{< highlight text "linenos=table, linenostart=1" >}}
f :: (Ord a) => [a] -> [a]
f [] = []
f (x:xs) =
  let smaller = f [a | a <- xs, a <= x]
      bigger = f [a | a <- xs, a > x]
  in  smaller ++ [x] ++ bigger
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
pena pf-guess-function-name/1
{{< /highlight >}}

```bash
["qsort","mergeSort","insert","quicksort"]
```