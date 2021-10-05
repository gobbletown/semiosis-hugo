+++
title = "Equations with Codex"
author = ["Shane Mulligan"]
date = 2021-10-05T00:00:00+13:00
keywords = ["openai", "codex"]
draft = false
+++

## Summary {#summary}

It's a real frustration searching for or even writing latex equations.

It's very easy to generate the required latex snippets with Codex, however.

External links
: <https://quicklatex.com/>


Snippet generator prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/code-snippet-from-natural-language-2.prompt>


ascii2latex prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/convert-ascii-to-latex-equation-1.prompt>


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/KFWqLaaBfjYZsVqFeKvURoVCT" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/KFWqLaaBfjYZsVqFeKvURoVCT.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/KFWqLaaBfjYZsVqFeKvURoVCT.js" id="asciicast-KFWqLaaBfjYZsVqFeKvURoVCT" async></script>


### Two possibilities for the perplexity equation {#two-possibilities-for-the-perplexity-equation}

{{< highlight latex "linenos=table, linenostart=1" >}}
perplexity \propto \cfrac{\prod p(w_{i})}{\prod p(w_{i})}
{{< /highlight >}}

{{< figure src="/ox-hugo/perplexity1.svg" >}}

{{< highlight latex "linenos=table, linenostart=1" >}}
\log{\displaystyle=\frac{1}{n}(log(p(w_1,\dots,w_n))\prod\limits_{i=1}^{n}p(w_i|\mathcal{W}))}
{{< /highlight >}}

{{< figure src="/ox-hugo/perplexity2.svg" >}}


## Euler's identity {#euler-s-identity}

{{< highlight latex "linenos=table, linenostart=1" >}}
e^(i*pi)+1=0
{{< /highlight >}}

{{< figure src="/ox-hugo/eulers-identity.svg" >}}


## Quadratic equation {#quadratic-equation}

{{< highlight latex "linenos=table, linenostart=1" >}}
a x^2 + b x + c = 0\\
x = \frac{-b \pm \sqrt{b^2-4 ac}}{2 a}
{{< /highlight >}}

{{< figure src="/ox-hugo/quadratic-equation.svg" >}}

Demonstration of above:

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/kfanOXb16b3YGDnle0IUmC8iz" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/kfanOXb16b3YGDnle0IUmC8iz.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/kfanOXb16b3YGDnle0IUmC8iz.js" id="asciicast-kfanOXb16b3YGDnle0IUmC8iz" async></script>


## Extra special equation generator {#extra-special-equation-generator}

`convert-ascii-to-latex-equation-1.prompt`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Convert ascii to latex equation"
doc: "Given some ascii text, convert to a latex equation"
notes:
- generate latex math mode from ascii representation
prompt-version: 1
prompt: |+
  How to write latex math equations:
  P(W) = P(w_1, w_2,...,w_N)
  <delim>
  Equation: sqrt(x^2 + 1)
  Latex: \sqrt{x^2+1}
  <delim>
  Equation: quadratic formula
  Latex: x = \frac{-b \pm \sqrt{b^2-4 ac}}{2 a}
  <delim>
  Equation: <equation>
  Latex:
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "equation"
examples:
- "quadratic formula"
filter: on
completion: off
insertion: off
{{< /highlight >}}


### Generate anything {#generate-anything}

{{< highlight text "linenos=table, linenostart=1" >}}
sqrt(infinity - 1)
{{< /highlight >}}

{{< highlight latex "linenos=table, linenostart=1" >}}
\sqrt{\infty - 1}
{{< /highlight >}}

{{< figure src="/ox-hugo/sqrt-infinity-minus-one.svg" >}}


### Demo of any equation generation {#demo-of-any-equation-generation}

-   From NL
-   From ascii pseudocode

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/5tnGoZQjJF5onp2FT7QaDVAIg" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/5tnGoZQjJF5onp2FT7QaDVAIg.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/5tnGoZQjJF5onp2FT7QaDVAIg.js" id="asciicast-5tnGoZQjJF5onp2FT7QaDVAIg" async></script>