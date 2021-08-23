+++
title = "AI21, AIx, OpenAI and HuggingFace with Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-23T00:00:00+12:00
keywords = ["ai21", "aix", "openai", "hf", "pen", "gpt"]
draft = false
+++

## Summary {#summary}

I benchmark using several different LM engines
in `Pen.el`.


## Benchmark 100 tokens {#benchmark-100-tokens}

<span class="underline">AI21 vs AIx vs OpenAI</span>

`AI21`'s service is about as fast as
`OpenAI`'s at about 6 seconds for 100 tokens,
but `AIx` is significantly slower.

You may speed up `AIx`'s generations by
subscribing to the GPU plan.


### AI21 {#ai21}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: j1-jumbo
include: Generic completion 50 tokens/1
prompt-version: 1
engine: AI21 J1-Jumbo
max-tokens: 100
{{< /highlight >}}

{{< highlight yaml "linenos=table, linenostart=1" >}}
engine-title: AI21 J1-Jumbo
lm-command: "ai21-complete.sh"
model: j1-jumbo
engine-min-tokens: 64
engine-max-tokens: 2048
default-tokens: 512
foundation: true
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
time penf -p -u pf-j1-jumbo/1 "Once upon a time"
{{< /highlight >}}

```bash
Once upon a time, there was this guy who decided he was going to quit his job and travel the world.
I mean, it had always been his dream, right? So, he quit his job and booked a flight overseas.
In typical fashion, he packed too much. You know, in case he needed it.
Then, he packed even more, just in case.
He checked the airlines website repeatedly. He double checked his passport. He triple checked his credit card.
Still, he found himself up the night before his departure, checking the airline website for any changes to his flight status, only for it to finally look official: the plane was boarded, the flight

real	0m6.272s
user	0m0.837s
sys	0m0.293s
```


### AIx {#aix}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: gpt-j
include: Generic completion 50 tokens/1
prompt-version: 1
engine: AIx GPT-J-6B
max-tokens: 100
{{< /highlight >}}

{{< highlight yaml "linenos=table, linenostart=1" >}}
engine-title: AIx GPT-J-6B
lm-command: "aix-complete.sh"
model: GPT-J-6B
engine-min-tokens: 64
engine-max-tokens: 2048
default-tokens: 512
foundation: true
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
time penf -p -u pf-gpt-j/1 "Once upon a time"
{{< /highlight >}}

```bash
Once upon a time there was a young woman who was looking for love in all the wrong places. She was a young woman who was looking for love in the wrong places. She was looking for love in all the wrong places. She looked for love in all the wrong places. She was looking for love in all the wrong places. She was looking for love in all the wrong places. She was looking for love in all the wrong places. She was looking love in all the wrong places. She looked for love in

real	0m48.047s
user	0m0.875s
sys	0m0.279s
```


### OpenAI {#openai}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: OpenAI Davinci
include: Generic completion 50 tokens/1
prompt-version: 1
engine: OpenAI Davinci
max-tokens: 100
{{< /highlight >}}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title-engine: OpenAI Davinci
lm-command: "openai-complete.sh"
model: davinci
foundation: true
engine-min-tokens: 64
engine-max-tokens: 2048
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
time penf -p -u pf-openai-davinci/1 "Once upon a time"
{{< /highlight >}}

```bash
Once upon a time, in a hut in the jungle, there lived a baby giraffe with a long neck. He would wander around the jungle, eating leaves from trees and bushes, and then, when he was tired, he would go to sleep on the ground.

One day, the baby giraffe was sleeping on the ground. But then he was woken up by what felt like a bright flash of light. The giraffe looked up and saw a woman standing over him.

"Who are

real	0m6.388s
user	0m0.889s
sys	0m0.253s
```


## Benchmark summaries {#benchmark-summaries}

_Work in progress._


### HuggingFace {#huggingface}

{{< highlight yaml "linenos=table, linenostart=1" >}}
engine-title: FB Bart Large CNN
lm-command: "hf-complete.sh"
model: facebook/bart-large-cnn
foundation: false
modes:
- summarize
{{< /highlight >}}