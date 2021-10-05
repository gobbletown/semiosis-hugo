+++
title = "Babel and Pen - Generating Snippets"
author = ["Shane Mulligan"]
date = 2021-10-06T00:00:00+13:00
keywords = ["math", "python", "babel", "pen"]
draft = false
+++

## Summary {#summary}


### Objectives {#objectives}

-   Automate the generation of snippets using babel and the pen docker.

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
Logistic map
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
import numpy as np
import matplotlib.pyplot as plt

def logistic(r, x):
  return r * x * (1 - x)

def plot_system(r, x0, n, ax=None):
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
kl divergence of samples
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
import random
import math

def kl_divergence(p, q):
  return sum(p[i] * math.log(p[i]/q[i], 2) for i in range(len(p)))

p = [
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
cross entropy of samples
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
import numpy as np
import matplotlib.pyplot as plt

def cross_entropy(p, q):
  return -sum([p[i]*np.log2(q[i]) for i in range(len(p))])
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
R-squared (R2)
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
import numpy as np
import pandas as pd
from sklearn.metrics import r2_score

def r2(x, y):
  return r2_score(x, y)
{{< /highlight >}}


## Template {#template}

{{< highlight bash "linenos=table, linenostart=1" >}}
code-snippet) {
    echo "#+BEGIN_SRC text -n :f penf -u code-snippet-from-natural-language/2 python :async :results verbatim code"
    cat "$input_fp" | postprocess
    echo -n "#+END_SRC"
}
;;
{{< /highlight >}}


## Prompt used {#prompt-used}


### `pf-code-snippet-from-natural-language/2` {#pf-code-snippet-from-natural-language-2}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: code snippet from natural language
future-titles:
- Get code snippet
- Get snippet
next-version: get-code-snippet.prompt
doc: Get a bash one liner from natural langauge
prompt: |+
    Language: Ruby
    Task: Reverse lines from stdout
    Code: while s = gets; puts s.chop.reverse; end
    <delim>
    Language: Haskell
    Task: Multiple Each Item in a List by 2
    Code:
    map (*2) [1..10]
    <delim>
    Language: bash
    Task: Recursively remove all "node_modules" folders
    Code:
    find . -name "node_modules" -exec rm -rf '{}' +
    <delim>
    Language: Haskell
    Task: Apply a function to both bits of a tuple:
    Code:
    both :: (a -> b) -> (a, a) -> (b, b)
    both = join (***)
    <delim>
    Language: Python
    Task: Palindrome Python One-Liner
    Code: phrase.find(phrase[::-1])
    <delim>
    Language: <1>
    Task: <2>
    Code:
engine: OpenAI Davinci
temperature: 0.8
max-generated-tokens: 60
top-p: 1
stop-sequences:
- "<delim>"
vars:
- language
- task
examples:
- bash
- suspend laptop
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/mH6B4MkGtXYJJYiWpwWN956F1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mH6B4MkGtXYJJYiWpwWN956F1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/mH6B4MkGtXYJJYiWpwWN956F1.js" id="asciicast-mH6B4MkGtXYJJYiWpwWN956F1" async></script>


## Some more demos {#some-more-demos}

{{< highlight text "linenos=table, linenostart=1" >}}
N choose K
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
def nCk(n,k):
  return int( reduce(mul, (Fraction(n-i, i+1) for i in range(k)), 1) )
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
inverse laplace transform
{{< /highlight >}}

{{< highlight python "linenos=table, linenostart=1" >}}
import sympy as sp
sp.init_printing()
t = sp.symbols('t')
f = sp.exp(-t)
sp.inverse_laplace_transform(f, t, s)
{{< /highlight >}}

Ruby?

{{< highlight text "linenos=table, linenostart=1" >}}
inverse laplace transform
{{< /highlight >}}

{{< highlight ruby "linenos=table, linenostart=1" >}}
require 'mathn'

def inverse_laplace_transform(expr, s, t)
expr.subs(s, 0).subs(t, 0).simplify.expand.collect { |x| x.subs(t,
{{< /highlight >}}