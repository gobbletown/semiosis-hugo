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