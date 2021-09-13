+++
title = "Review of '[2006.03511] Unsupervised Translation of Programming Languages'"
author = ["Shane Mulligan"]
date = 2020-06-10T00:00:00+12:00
keywords = ["arxiv", "transpilers", "facebook", "philosophy"]
draft = false
+++

Original article
: [ 2006.03511  Unsupervised Translation of Programming Languages](https://arxiv.org/abs/2006.03511)


News
: <https://venturebeat.com/2020/06/08/facebooks-transcoder-ai-converts-code-from-one-programming-language-into-another/>


Research
: Lample et al.


Researcher
: Guillaume Lample <br />
    Facebook AI Research <br />
    glample@fb.com


## Fully unsupervised neural transcompiler {#fully-unsupervised-neural-transcompiler}

-   Converts languages obtained from GitHub
    -   BigQuery
-   unsupervised
    -   it looks for previously undetected patterns in data sets without
        labels and with a minimal amount of human supervision
-   outperforms rule-based baselines by a “significant” margin.


### Model of `TransCoder` {#model-of-transcoder}

-   `seq2seq` with attention
    -   composed of
        -   encoder
        -   decoder
        -   transformer architecture
    -   the same model is used for all programming languages.

-   Trained using the three principles of unsupervised machine translation
    -   initialization,
    -   LMing, and
    -   back-translation.

In this section, we summarize these principles
and detail how we instantiate them to
translate programming languages.


### Cross Programming Language Model pretraining {#cross-programming-language-model-pretraining}

{{< highlight text "linenos=table, linenostart=1" >}}
Pretraining
    A key ingredient of unsupervised machine
    translation.

    It ensures that sequences with a similar
    meaning are mapped to the same latent
    representation, regardless of their
    languages.
{{< /highlight >}}

Originally, pretraining was done by
initializing the model with cross-lingual word
representations.

In the context of unsupervised English-French
translation, the embedding of the word "cat"
will be close to the embedding of its French
translation "chat".

Cross-lingual word embeddings can be obtained
by training monolingual word embeddings and
aligning them in an unsupervised manner.

Subsequent work showed that pretraining
the entire model (and not only word
representations) in a cross-lingual way could
lead to significant improvements in
unsupervised machine translation.


### The pretraining strategy of Lample and Conneau {#the-pretraining-strategy-of-lample-and-conneau}

In particular, we follow the pretraining
strategy of Lample and Conneau, where a XLM
is pretrained with a masked LMing objective on
monolingual source code datasets.

First principle: initialization
: The first principle initializes the model with <br />
    cross-lingual masked LM pretraining. <br />

    As a result, pieces of code that express the <br />
    same instructions are mapped to the same <br />
    representation, regardless of the programming <br />
    language.


Second principle: denoising
: Train the decoder to always generate valid <br />
    sequences, even when fed with noisy data, and <br />
    increases the encoder robustness to input <br />
    noise.


Third (and last) principle: back-translation
: Allows the model to generate parallel data <br />
    which can be used for training. <br />

    Whenever the Python C++ model becomes better, <br />
    it generates more accurate data for the C++ <br />
    Python model, and vice versa.

> We obtain cross-lingual embeddings after training.

The cross-lingual nature of the resulting
model comes from the significant number of
common tokens (anchor points) that exist
across languages.

In the context of English-French translation,
the anchor points consists essentially of
digits and city and people names.

In programming languages, these anchor points
come from common keywords (e.g. for, while,
if, try), and also digits, mathematical
operators, and English strings that appear in
the source code.

For the masked LMing (MLM) objective, at each
iteration we consider an input stream of
source code sequences, randomly mask out some
of the tokens, and train TransCoder to predict
the tokens that have been masked out based on
their contexts.

We alternate between streams of batches of
different languages.

This allows the model to create high quality,
cross-lingual sequence representations.

<span class="underline">**Denoising auto-encoding**</span>

We initialize the encoder and decoder of the
seq2seq model with the XLM model that was pretrained.

The initialization is straightforward for the
encoder, as it has the same architecture as
the XLM model.

The transformer decoder, however, has extra
parameters related to the source attention
mechanism .

Following Lample and Conneau, we initialize
these parameters randomly.

XLM pretraining
: allows the `seq2seq` model to <br />
    generate high quality representations of input <br />
    sequences.

However, the decoder lacks the capacity to
translate, as it has never been trained to
decode a sequence based on a source
representation.

To address this issue, we train the model to
encode and decode sequences with a `Denoising Auto-Encoding (DAE)` objective .

The `DAE` objective operates like a supervised
machine translation algorithm, where the model
is trained to predict a sequence of tokens
given a corrupted version of that sequence.

To corrupt a sequence, we use the same noise
model as the one described in Lample et al. .

Namely, we randomly mask, remove and shuffle
input tokens.

In practice, the "cross-linguality" of the
model highly depends on the amount of anchor
points across languages.

As a result, a XLM model trained on English-
French will provide better cross-lingual
representations than a model trained on
English-Chinese, because of the different
alphabet which reduces the number of anchor
points.

In programming languages, the majority of
strings are composed of English words, which
results in a fairly high number of anchor
points, and the model naturally becomes cross-
lingual.


## Arxiv Summary {#arxiv-summary}

{{< highlight text "linenos=table, linenostart=1" >}}
A transcompiler, also known as source-to-
source translator, is a system that converts
source code from a high-level programming
language (such as C++ or Python) to another.

Transcompilers are primarily used for
interoperability, and to port codebases
written in an obsolete or deprecated language
(e.g. COBOL, Python 2) to a modern one.

They typically rely on handcrafted rewrite
rules, applied to the source code abstract
syntax tree.

Unfortunately, the resulting translations
often lack readability, fail to respect the
target language conventions, and require
manual modifications in order to work
properly.

The overall translation process is
timeconsuming and requires expertise in both
the source and target languages, making code-
translation projects expensive.

Although neural models significantly
outperform their rule-based counterparts in
the context of NL translation, their
applications to transcompilation have been
limited due to the scarcity of parallel data
in this domain.

In this paper, we propose to leverage recent
approaches in unsupervised machine translation
to train a fully unsupervised neural
transcompiler.

We train our model on source code from open
source GitHub projects, and show that it can
translate functions between C++, Java, and
Python with high accuracy.

Our method relies exclusively on monolingual
source code, requires no expertise in the
source or target languages, and can easily be
generalized to other programming languages.

We also build and release a test set composed
of 852 parallel functions, along with unit
tests to check the correctness of
translations.

We show that our model outperforms rule-based
commercial baselines by a significant margin.
{{< /highlight >}}


## GPT-3 summary of Arxiv summary {#gpt-3-summary-of-arxiv-summary}

We train a neural transcompiler using
monolingual source code from GitHub, and show
that it can translate functions between C++,
Java, and Python with high accuracy.