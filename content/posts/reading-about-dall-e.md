+++
title = "Reading about DALL-E"
author = ["Shane Mulligan"]
date = 2021-03-12T00:00:00+13:00
keywords = ["openai", "emacs"]
draft = false
+++

Article
: <https://openai.com/blog/dall-e/>


Original paper
: <https://arxiv.org/abs/2102.12092.pdf>


Authors of Paper
    -   Aditya Ramesh
    -   Mikhail Pavlov
    -   Gabriel Goh
    -   Scott Gray
    -   Chelsea Voss
    -   Alec Radford
    -   Mark Chen
    -   Ilya Sutskever


## Summary of DALL-E from Arxiv {#summary-of-dall-e-from-arxiv}

{{< highlight text "linenos=table, linenostart=1" >}}
Text-to-image generation has traditionally
focused on finding better modeling assumptions
for training on a fixed dataset.

These assumptions might involve complex
architectures, auxiliary losses, or side
information such as object part labels or
segmentation masks supplied during training.

We describe a simple approach for this task
based on a transformer that autoregressively
models the text and image tokens as a single
stream of data.

With sufficient data and scale, our approach
is competitive with previous domain-specific
models when evaluated in a zero-shot fashion.
{{< /highlight >}}


### `GPT-3` Summarization {#gpt-3-summarization}

Both of the following summarizations were made
my `GPT-3` using the following prompt:

<http://github.com/semiosis/prompts/blob/master/prompts/summarize-for-2nd-grader.prompt>

-   Abstractive summarizations of the abstract
    -   This is how we can make a computer that can make a picture of words.

    -   This is about computers learning to understand pictures and words together,
        not just pictures or just words.


## Automating the process of reading papers {#automating-the-process-of-reading-papers}

-   I use `GPT-3` and `spaCy` to summarize.

<a title="asciinema recording" href="https://asciinema.org/a/mp12WYy9bWwkeRizb4Y4o4jJb" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mp12WYy9bWwkeRizb4Y4o4jJb.svg" /></a>