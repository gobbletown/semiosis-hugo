+++
title = "A tour of Ryan Ong's - NLP 365"
author = ["Shane Mulligan"]
date = 2021-03-13T00:00:00+13:00
keywords = ["nlp", "openai", "gpt3"]
draft = false
+++

Glossaries
: [nlp-natural-language-processing.txt](http://github.com/mullikine/glossaries-gh/blob/master/nlp-natural-language-processing.txt) <br />
    [information-retrieval.txt](http://github.com/mullikine/glossaries-gh/blob/master/information-retrieval.txt) <br />
    [fasttext.txt](http://github.com/mullikine/glossaries-gh/blob/master/fasttext.txt) <br />
    [transformer.txt](http://github.com/mullikine/glossaries-gh/blob/master/transformer.txt) <br />
    [nmt-neural-machine-translation.txt](http://github.com/mullikine/glossaries-gh/blob/master/nmt-neural-machine-translation.txt) <br />
    [information-theory.txt](http://github.com/mullikine/glossaries-gh/blob/master/information-theory.txt)


## Summary {#summary}

I go over Ryan Ong's series on `NLP`, adding
terms to my glossaries and reproducing what
he's done.

---


## Finished reading {#finished-reading}

<span class="timestamp-wrapper"><span class="timestamp">&lt;2021-03-13 Sat&gt;</span></span>

-   [Day 1: What is Natural Language Processing - Ryan Ong](https://ryanong.co.uk/2020/01/01/day-1-what-is-natural-language-processing/)
-   [Day 2: Damerau-Levenshtein Distance - Ryan Ong](https://ryanong.co.uk/2020/01/01/day-1-what-is-natural-language-processing/https://ryanong.co.uk/2020/01/02/day-2-damerau-levenshtein-distance/)
-   [Day 3: Word Embeddings - Ryan Ong](https://ryanong.co.uk/2020/01/03/day-3-word-embeddings/)

Terms in the green text have added to my glossary.

{{< figure src="/ox-hugo/ryanong-day-3.png" >}}

This is the project that introduced me to `GloVe` a few years ago.

<https://github.com/mullikine/codenames>

I skipped over `projectJarvis`.

-   [Day 4: projectJarvis - Introduction {#project2020} - Ryan Ong](https://ryanong.co.uk/2020/01/04/day-4-scraping-google-search-results/)
-   [Day 5: projectJarvis - Retrieving medium articles - Ryan Ong](https://ryanong.co.uk/2020/01/05/day-5-projectjarvis-retrieving-medium-articles/)
-   [Day 6: projectJarvis  Retrieving medium articles {Code} - Ryan Ong](https://ryanong.co.uk/2020/01/06/day-6-projectjarvis-retrieving-medium-articles-code/)

I already know about `TFIDF`.

-   [Day 7: Term Frequency-Inverse Document Frequency {TFIDF} for Summarisation - Ryan Ong](https://ryanong.co.uk/2020/01/07/day-7-term-frequency-inverse-document-frequency-tf-idf/)

Terms in green text have been added to my glossary.

Terms in blue are suggested by `pytextrank` / `TextRank`.

{{< figure src="/ox-hugo/ryanong-day-7.png" >}}

[Suggesting new words for the glossary with KeyBERT and pytextrank // Bodacious Blog](https://mullikine.github.io/posts/suggesting-new-words-for-the-glossary-with-keybert-and-pytextrank/)

-   [Day 8: TextRank for Summarisation - Ryan Ong](https://ryanong.co.uk/2020/01/08/day-8/)

Learning more about `TextRank`:

I use `GPT-3` to correct this sentence, as I am adding it to the glossary.

{{< highlight text "linenos=table, linenostart=1" >}}
graph-based ranking
    [[https://ryanong.co.uk/2020/01/08/day-8/][Day 8: TextRank for Summarisation - Ryan Ong]]

    In short, a graph-based ranking algorithm
    is a way of deciding on the importance of
    a vertex within a graph, by taking into
    account global information recursively
    computed from the entire graph, rather
    than relying only on local vertex-specific
    information.
{{< /highlight >}}

Once again, blue words here are suggested by `TextRank` using the transformer model `en_core_web_trf`.

<a title="asciinema recording" href="https://asciinema.org/a/398748" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/398748.svg" /></a>

-   [Day 9: Transformers - Introduction - Ryan Ong](https://ryanong.co.uk/2020/01/09/day-9/)

I had to recall what a residual connection was.

{{< highlight text "linenos=table, linenostart=1" >}}
residual connection
skip connection
    [#deep learning]
    [#RNN]

    Used to allow gradients to flow through a
    network directly, without passing through
    the non-linear activation functions.

    Form conceptually a 'bus' which flows
    right the way through the network, and in
    reverse, the gradients can flow backwards
    along it too.

    high-level intuition:
    - residual connections help deep models
      train more successfully.
{{< /highlight >}}

[The Illustrated Transformer // Bodacious Blog](https://mullikine.github.io/posts/review-of-the-illustrated-transformer/)

-   [Day 10: Transformers - MultiHead Attention Mechanism - Ryan Ong](https://ryanong.co.uk/2020/01/10/day-10-transformers-multihead-attention-mechanism/)

---

{{< figure src="/ox-hugo/ryanong-day-10.png" >}}

New terms
: <span class="underline">self-attention</span> <br />
    <span class="underline">Multi-Head Attention</span> <br />
    <span class="underline">multi-head self-attention</span>

---

-   [Day 11 - Transformers: Positioning Encoding and Decoder - Ryan Ong](https://ryanong.co.uk/2020/01/11/day-11-transformers-positioning-encoding-and-decoder/)

---


## Left to read {#left-to-read}

-   [Day 12: Recurrent Neural Network - Ryan Ong](https://ryanong.co.uk/2020/01/12/day-12-recurrent-neural-network/)
-   [Day 13: Seq2Seq - Ryan Ong](https://ryanong.co.uk/2020/01/13/day-13-seq2seq/)
-   [Day 14: Convolutional Neural Network in NLP - Ryan Ong](https://ryanong.co.uk/2020/01/14/day-14-convolutional-neural-network/)
-   [Day 15: TextRank for Summarisation {Code - Gensim} - Ryan Ong](https://ryanong.co.uk/2020/01/15/day-15-textrank-for-summarisation-code-gensim/)
-   [Day 16: TextRank - Manual Implementation {Code} - Ryan Ong](https://ryanong.co.uk/2020/01/16/day-16-textrank-manual-implementation-code/)
-   [Day 17: TFIDF for Summarisation - Implementation I - Constructing a Class - Ryan Ong](https://ryanong.co.uk/2020/01/17/day-17-tfidf-for-summarisation-code/)
-   [Day 18: TFIDF for Summarisation - Implementation II - Term Frequency {TF} Matrix - Ryan Ong](https://ryanong.co.uk/2020/01/18/day-18-tfidf-for-summarisation-implementation-ii-term-frequency-tf-matrix/)
-   [Day 19: TFIDF for Summarisation - Implementation III - IDF Matrix - Ryan Ong](https://ryanong.co.uk/2020/01/19/day-19-tfidf-for-summarisation-implementation-iii-inverse-document-frequency-idf-matrix/)
-   [Day 20: TFIDF for Summarisation - Implementation IV - TFIDF Matrix & Sentence Scoring - Ryan Ong](https://ryanong.co.uk/2020/01/20/day-20-tfidf-for-summarisation-implementation-iv-tfidf-matrix-sentence-scoring/)
-   [Day 21: TFIDF for Summarisation - Implementation V - Summary Generation - Ryan Ong](https://ryanong.co.uk/2020/01/21/day-21-tfidf-for-summarisation-summary-generation/)
-   [Day 22: TFIDF for Summarisation - Implementation VI - Putting It All Together - Ryan Ong](https://ryanong.co.uk/2020/01/22/day-22-tfidf-for-summarisation-putting-it-all-together/)
-   [Day 23: Summarisation - ROUGE score - Ryan Ong](https://ryanong.co.uk/2020/01/23/day-18-summarisation-evaluation-metrics/)
-   [Day 24: Learn NLP with Me - Machine Reading Comprehension - Introduction - Ryan Ong](https://ryanong.co.uk/2020/01/24/day-24-learn-nlp-with-me-machine-reading-comprehension-introduction/)
-   [Day 25: Learn NLP with Me - Machine Reading Comprehension - MRC Tasks - Ryan Ong](https://ryanong.co.uk/2020/01/25/day-25-learn-nlp-with-me-machine-reading-comprehension-mrc-tasks/)
-   [Day 26: Learn NLP with Me - Machine Reading Comprehension - Datasets & Evaluation Metrics - Ryan Ong](https://ryanong.co.uk/2020/01/26/day-26-learn-nlp-with-me-machine-reading-comprehension-deep-learning-methods/)
-   [Day 27: Learn NLP with Me - MRC - Deep Learning I - General Architecture - Ryan Ong](https://ryanong.co.uk/2020/01/27/day-27-learn-nlp-with-me-machine-reading-comprehension-deep-learning-methods-i/)
-   [Day 28: Learn NLP with Me - MRC - Deep Learning II - Embeddings - Ryan Ong](https://ryanong.co.uk/2020/01/28/day-28-learn-nlp-with-me-machine-reading-comprehension-deep-learning-methods-ii/)
-   [Day 29: Learn NLP with Me - MRC - Deep Learning III - Feature Extraction - Ryan Ong](https://ryanong.co.uk/2020/01/29/day-29-learn-nlp-with-me-machine-reading-comprehension-deep-learning-methods-iii/)
-   [Day 30: Learn NLP with Me - MRC - Deep Learning IV - Context Question Interaction - Ryan Ong](https://ryanong.co.uk/2020/01/30/day-30-learn-nlp-with-me-mrc-deep-learning-iii-context-question-interaction/)
-   [Day 31: Learn NLP with Me - MRC - Deep Learning V - Answer Prediction - Ryan Ong](https://ryanong.co.uk/2020/01/31/day-31-learn-nlp-with-me-mrc-deep-learning-iii-answer-prediction/)
-   [Day 32: Learn NLP with Me - MRC - Deep Learning VI - Additional Tricks - Ryan Ong](https://ryanong.co.uk/2020/02/01/day-32-learn-nlp-with-me-mrc-deep-learning-iii-additional-tricks/)
-   [Day 33: Learn NLP with Me - MRC - Open Issues - Ryan Ong](https://ryanong.co.uk/2020/02/02/day-33-learn-nlp-with-me-mrc-open-issues/)
-   [Day 34: Learn NLP with Me - MRC - New Trends I - Ryan Ong](https://ryanong.co.uk/2020/02/03/day-34-learn-nlp-with-me-mrc-new-trends/)
-   [Day 35: Learn NLP with Me - MRC - New Trends II - Ryan Ong](https://ryanong.co.uk/2020/02/04/day-35-learn-nlp-with-me-mrc-new-trends-ii/)
-   [Day 36: Learn NLP with Me - MRC - New Trends III - Ryan Ong](https://ryanong.co.uk/2020/02/05/day-36-learn-nlp-with-me-mrc-new-trends-iii/)
-   [Day 37: Learn NLP with Me - fast.ai NLP course - What is NLP? - Ryan Ong](https://ryanong.co.uk/2020/02/06/day-37-learn-nlp-with-me-fast-ai-nlp-course-what-is-nlp/)
-   [Day 38: NLP Discovery - Google's Chatbot Meena - Ryan Ong](https://ryanong.co.uk/2020/02/07/day-38-nlp-discovery-meena-googles-chatbot/)
-   [Day 39: What is Perplexity? - Ryan Ong](https://ryanong.co.uk/2020/02/08/day-39-what-is-perplexity/)
-   [Day 40: What is Neural Architecture Search {NAS}? - Ryan Ong](https://ryanong.co.uk/2020/02/09/day-40-what-is-neural-architecture-search-nas/)
-   [Day 41: Learn NLP with Me - fast.ai NLP course - Topic Modelling with SVD & NMF - Ryan Ong](https://ryanong.co.uk/2020/02/10/day-41-learn-nlp-with-me-fast-ai-nlp-course-topic-modelling-with-svd-nmf/)
-   [Day 42: Learn NLP with Me - fast.ai NLP course - Topic Modelling & SVD revisited - Ryan Ong](https://ryanong.co.uk/2020/02/11/day-42-learn-nlp-with-me-fast-ai-nlp-course-topic-modelling-svd-revisited/)
-   [Day 43: Learn NLP with Me - Information Extraction - Introduction - Ryan Ong](https://ryanong.co.uk/2020/02/12/day-43-learn-nlp-with-me-information-extraction-introduction/)
-   [Day 44: Learn NLP with Me - Information Extraction - Entities - Ryan Ong](https://ryanong.co.uk/2020/02/13/day-44-learn-nlp-with-me-information-extraction-entities/)
-   [Day 45: Learn NLP with Me - Information Extraction - Entities - Entity linking by learning to rank - Ryan Ong](https://ryanong.co.uk/2020/02/14/day-45-learn-nlp-with-me-information-extraction-entities-entity-linking-by-learning-to-rank/)
-   [Day 46: Learning PyTorch - A Deep Learning Framework - Introduction to Tensors - Ryan Ong](https://ryanong.co.uk/2020/02/15/day-46-learning-pytorch-a-deep-learning-framework-introduction-to-tensors/)
-   [Day 47: Learning PyTorch - Autograd - Automatic Differentiation - Ryan Ong](https://ryanong.co.uk/2020/02/16/day-47-learning-pytorch-autograd-automatic-differentiation/)
-   [Day 48: Learning PyTorch - Training a Neural Network - Ryan Ong](https://ryanong.co.uk/2020/02/17/day-48-learning-pytorch-training-a-neural-network/)
-   [Day 49: Learning PyTorch - Training an Image Classifier - Ryan Ong](https://ryanong.co.uk/2020/02/18/day-49-learning-pytorch-training-an-image-classifier/)
-   [Day 50: NLP Discovery - Turing-NLG - A 17-billion parameter Language Model - Ryan Ong](https://ryanong.co.uk/2020/02/19/day-50-nlp-discovery-turing-nlg-a-17-billion-parameter-language-model/)
-   [Day 51: Learn NLP with Me - Information Extraction - Entities - Collective Entity Linking - Ryan Ong](https://ryanong.co.uk/2020/02/20/day-51-learn-nlp-with-me-information-extraction-entities-collective-entity-linking/)
-   [Day 52: Learn NLP with Me - Information Extraction - Relations - Introduction - Ryan Ong](https://ryanong.co.uk/2020/02/21/day-52-learn-nlp-with-me-information-extraction-relations-introduction/)
-   [Day 53: Learn NLP with Me - Information Extraction - Relations - Pattern-based Relation Extraction - Ryan Ong](https://ryanong.co.uk/2020/02/22/day-53-learn-nlp-with-me-information-extraction-relations-pattern-based-relation-extraction/)
-   [Day 54: Learn NLP with Me - Formal Language Theory - Introduction - Ryan Ong](https://ryanong.co.uk/2020/02/23/day-54-learn-nlp-with-me-formal-language-theory-introduction/)
-   [Day 55: Learn NLP with Me - Formal Language Theory - Regular Languages - Introduction - Ryan Ong](https://ryanong.co.uk/2020/02/24/day-55-learn-nlp-with-me-formal-language-theory-regular-languages-introduction/)
-   [Day 56: Learn NLP with Me - Formal Language Theory - Regular Languages - Finite State Acceptors - Ryan Ong](https://ryanong.co.uk/2020/02/25/day-56-learn-nlp-with-me-formal-language-theory-regular-languages-finite-state-acceptors/)
-   [Day 57: Learn NLP with Me - fast.ai NLP course - Sentiment Classification with Nave Bayes - Ryan Ong](https://ryanong.co.uk/2020/02/26/day-57-learn-nlp-with-me-fast-ai-nlp-course-sentiment-classification-with-naive-bayes/)
-   [Day 58: Learn NLP with Me - Formal Language Theory - Regular Languages - Morphology Analysis - Ryan Ong](https://ryanong.co.uk/2020/02/27/day-58-learn-nlp-with-me-formal-language-theory-regular-languages-morphology-analysis/)
-   [Day 59: Learn NLP with Me - Formal Language Theory - Regular Languages - Weighted FSAs - Ryan Ong](https://ryanong.co.uk/2020/02/28/day-59-learn-nlp-with-me-formal-language-theory-regular-languages-weighted-fsas/)
-   [Day 60: Learn NLP with Me - FLT - Regular Languages - Finite State Transducers - Ryan Ong](https://ryanong.co.uk/2020/02/29/day-60-learn-nlp-with-me-flt-regular-languages-finite-state-transducers/)
-   [Day 61: What is Semantic Textual Similarity? - Ryan Ong](https://ryanong.co.uk/2020/03/01/day-61-what-is-semantic-textual-similarity/)
-   [Day 62: Learn NLP with Me - FLT - Regular Languages - Inflectional Morphology - Ryan Ong](https://ryanong.co.uk/2020/03/02/day-62-learn-nlp-with-me-flt-regular-languages-inflectional-morphology/)
-   [Day 63: Learn NLP with Me - FLT - Regular Languages - Finite state composition - Ryan Ong](https://ryanong.co.uk/2020/03/03/day-63-learn-nlp-with-me-flt-regular-languages-finite-state-composition/)
-   [Day 64: Learn NLP with Me - Information Extraction - Relations - Relation Extraction as Classification Task - Ryan Ong](https://ryanong.co.uk/2020/03/04/day-64-learn-nlp-with-me-information-extraction-relations-relation-extraction-as-classification-task/)
-   [Day 65: Learn NLP with Me - Information Extraction - R.E. as Classification Task - Kernel Method - Ryan Ong](https://ryanong.co.uk/2020/03/05/day-65-learn-nlp-with-me-information-extraction-relations-relation-extraction-as-classification-task-kernel-method/)
-   [Day 66: Learn NLP with Me - fast.ai NLP course - Sentiment Classification with Nave Bayes & Logistic Regression - Ryan Ong](https://ryanong.co.uk/2020/03/06/day-66-learn-nlp-with-me-fast-ai-nlp-course-sentiment-classification-with-naive-bayes-logistic-regression/)
-   [Day 67: Learn NLP with Me - fast.ai NLP course - Derivation of Nave Bayes & Numerical Stability - Ryan Ong](https://ryanong.co.uk/2020/03/07/day-67-learn-nlp-with-me-fast-ai-nlp-course-derivation-of-naive-bayes-numerical-stability/)
-   [Day 68: Learn NLP with Me - Information Extraction - R.E. as Classification Task - Neural Method - Ryan Ong](https://ryanong.co.uk/2020/03/08/day-68-learn-nlp-with-me-information-extraction-r-e-as-classification-task-neural-method/)
-   [Day 69: Learn NLP with Me - Information Extraction - Relations - Knowledge Base Population - Ryan Ong](https://ryanong.co.uk/2020/03/09/day-69-learn-nlp-with-me-information-extraction-relations-knowledge-base-population/)
-   [Day 70: Learn NLP with Me - I.E. - Knowledge Base Population - Information Fusion - Ryan Ong](https://ryanong.co.uk/2020/03/10/day-70-learn-nlp-with-me-i-e-knowledge-base-population-information-fusion/)
-   [Day 71: Learn NLP with Me - I.E. - Knowledge Base Population - Distant Supervision - Ryan Ong](https://ryanong.co.uk/2020/03/11/day-71-learn-nlp-with-me-i-e-knowledge-base-population-distant-supervision/)
-   [Day 72: Learn NLP with Me - I.E. - Relations - Open Information Extraction - Ryan Ong](https://ryanong.co.uk/2020/03/12/day-72-learn-nlp-with-me-i-e-relations-open-information-extraction/)
-   [Day 73: Learn NLP with Me - Information Extraction - Events - Ryan Ong](https://ryanong.co.uk/2020/03/13/day-73-learn-nlp-with-me-information-extraction-events/)
-   [Day 74: Learn NLP with Me - I.E. - Hedges, Denials, and Hypotheticals - Introduction - Ryan Ong](https://ryanong.co.uk/2020/03/14/day-74-learn-nlp-with-me-i-e-hedges-denials-and-hypotheticals-introduction/)
-   [Day 75: Learn NLP with Me - I.E. - Hedges, Denials, and Hypotheticals - Handling Modality - Ryan Ong](https://ryanong.co.uk/2020/03/15/day-75-learn-nlp-with-me-i-e-hedges-denials-and-hypotheticals-handling-modality/)
-   [Day 76: Learn NLP with Me - Formal Language Theory - Context-free Languages - Introduction - Ryan Ong](https://ryanong.co.uk/2020/03/16/day-76-learn-nlp-with-me-formal-language-theory-context-free-languages-introduction/)
-   [Day 77: Learn NLP with Me - FLT - Context-free Languages - Context-free Grammars - Ryan Ong](https://ryanong.co.uk/2020/03/17/day-77-learn-nlp-with-me-flt-context-free-languages-context-free-grammars/)
-   [Day 78: Learn NLP with Me - FLT - Context-free Languages - Chomsky Normal Form - Ryan Ong](https://ryanong.co.uk/2020/03/18/day-78-learn-nlp-with-me-flt-context-free-languages-chomsky-normal-form/)
-   [Day 79: Mini NLP Data Science Project - Implementation Series - Introduction - Ryan Ong](https://ryanong.co.uk/2020/03/19/day-79-mini-nlp-data-science-project-implementation-series-introduction/)
-   [Day 80: Mini NLP Data Science Project - Implementation I - EDA - Ryan Ong](https://ryanong.co.uk/2020/03/20/day-80-mini-nlp-data-science-project-implementation-i-eda/)
-   [Day 81: Mini NLP Data Science Project - Implementation II - Text Processing - Ryan Ong](https://ryanong.co.uk/2020/03/21/day-81-mini-nlp-data-science-project-implementation-ii-text-processing/)
-   [Day 82: Mini NLP Data Science Project - Implementation III - Text Clustering I - Ryan Ong](https://ryanong.co.uk/2020/03/22/day-82-mini-nlp-data-science-project-implementation-iii-text-clustering-i/)
-   [Day 83: Mini NLP Data Science Project - Implementation IV - Text Clustering II - Ryan Ong](https://ryanong.co.uk/2020/03/23/day-83-mini-nlp-data-science-project-implementation-iv-text-clustering-ii/)
-   [Day 84: Mini NLP Data Science Project - Implementation V - Text Clustering III - Ryan Ong](https://ryanong.co.uk/2020/03/24/day-84-mini-nlp-data-science-project-implementation-v-text-clustering-iii/)
-   [Day 85: Mini NLP Data Science Project - Implementation VI - Topic Modelling Analysis - Ryan Ong](https://ryanong.co.uk/2020/03/25/day-85-mini-nlp-data-science-project-implementation-vi-topic-modelling-analysis/)
-   [Day 86: Mini NLP Data Science Project - Implementation VII - Text Similarity - Ryan Ong](https://ryanong.co.uk/2020/03/26/day-86-mini-nlp-data-science-project-implementation-vii-text-similarity/)
-   [Day 87: Learn NLP with Me - BERT on Sentiment Analysis - Ryan Ong](https://ryanong.co.uk/2020/03/27/day-87-learn-nlp-with-me-bert-on-sentiment-analysis/)
-   [Day 88: What is Autoencoders? - Ryan Ong](https://ryanong.co.uk/2020/03/28/day-88-what-is-autoencoders/)
-   [Day 89: Deep Generative Models and NLP - Ryan Ong](https://ryanong.co.uk/2020/03/29/day-89-deep-generative-models-and-nlp/)
-   [Day 90: Learn PGM with Me - What is Probabilistic Graphical Modelling? - Ryan Ong](https://ryanong.co.uk/2020/03/30/day-90-learn-pgm-with-me-what-is-probabilistic-graphical-modelling/)
-   [Day 91: Learn PGM with Me - The 3 Main Aspects of Graphical Models - Ryan Ong](https://ryanong.co.uk/2020/03/31/day-91-learn-pgm-with-me-the-3-main-aspects-of-graphical-models/)
-   [Day 92: Learn PGM with Me - Probability Review for Graphical Models - Ryan Ong](https://ryanong.co.uk/2020/04/01/day-92-learn-pgm-with-me-probability-review-for-graphical-models/)
-   [Day 93: Learn PGM with Me - Probability Review for Graphical Models - Elements of probability - Ryan Ong](https://ryanong.co.uk/2020/04/02/day-93-learn-pgm-with-me-probability-review-for-graphical-models-elements-of-probability/)
-   [Day 94: Learn PGM with Me - Probability Review for Graphical Models - Random Variables - Ryan Ong](https://ryanong.co.uk/2020/04/03/day-94-learn-pgm-with-me-probability-review-for-graphical-models-random-variables/)
-   [Day 95: Learn PGM with Me - Probability Review for Graphical Models - Two Random Variables - Ryan Ong](https://ryanong.co.uk/2020/04/04/day-95-learn-pgm-with-me-probability-review-for-graphical-models-two-random-variables/)
-   [Day 96: Learn PGM with Me - Representation - Introduction to Bayesian Networks - Ryan Ong](https://ryanong.co.uk/2020/04/05/day-96-learn-pgm-with-me-representation-introduction-to-bayesian-networks/)
-   [Day 97: Learn PGM with Me - Representation - Dependencies of a Bayes' Network - Ryan Ong](https://ryanong.co.uk/2020/04/06/day-97-learn-pgm-with-me-representation-dependencies-of-a-bayes-network/)
-   [Day 98: Learn PGM with Me - Representation - Introduction to Markov Random Fields - Ryan Ong](https://ryanong.co.uk/2020/04/07/day-98-learn-pgm-with-me-representation-markov-random-fields/)
-   [Day 99: Learn PGM with Me - Representation - Markov Random Fields vs Bayesian Networks - Ryan Ong](https://ryanong.co.uk/2020/04/08/day-99-learn-pgm-with-me-representation-markov-random-fields-vs-bayesian-networks/)
-   [Day 100: Learn PGM with Me - Representation - Introduction to Conditional Random Fields - Ryan Ong](https://ryanong.co.uk/2020/04/09/day-100-learn-pgm-with-me-representation-introduction-to-conditional-random-fields/)
-   [Day 101: In-depth study of RASA's DIET Architecture - Ryan Ong](https://ryanong.co.uk/2020/04/10/day-101-in-depth-study-of-rasas-diet-architecture/)
-   [Day 102: NLP Papers Summary - Implicit and Explicit Aspect Extraction in Financial Microblogs - Ryan Ong](https://ryanong.co.uk/2020/04/11/day-102-nlp-research-papers-implicit-and-explicit-aspect-extraction-in-financial-microblogs/)
-   [Day 103: NLP Papers Summary - Utilizing BERT for Aspect-Based Sentiment Analysis via Constructing Auxiliary Sentence - Ryan Ong](https://ryanong.co.uk/2020/04/12/day-103-nlp-research-papers-utilizing-bert-for-aspect-based-sentiment-analysis-via-constructing-auxiliary-sentence/)
-   [Day 104: NLP Papers Summary - SentiHood: Targeted Aspect Based Sentiment Analysis Dataset for Urban Neighbourhoods - Ryan Ong](https://ryanong.co.uk/2020/04/13/day-104-nlp-research-papers-sentihood-targeted-aspect-based-sentiment-analysis-dataset-for-urban-neighbourhoods/)
-   [Day 105: NLP Papers Summary - Aspect Level Sentiment Classification with Attention-over-Attention Neural Networks - Ryan Ong](https://ryanong.co.uk/2020/04/14/day-105-nlp-research-papers-aspect-level-sentiment-classification-with-attention-over-attention-neural-networks/)
-   [Day 106: NLP Papers Summary - An Unsupervised Neural Attention Model for Aspect Extraction - Ryan Ong](https://ryanong.co.uk/2020/04/15/day-106-nlp-research-papers-an-unsupervised-neural-attention-model-for-aspect-extraction/)
-   [Day 107: NLP Papers Summary - Make Lead Bias in Your Favor: A Simple and Effective Method for News Summarization - Ryan Ong](https://ryanong.co.uk/2020/04/16/day-107-nlp-research-papers-make-lead-bias-in-your-favor-a-simple-and-effective-method-for-news-summarization/)
-   [Day 108: NLP Papers Summary - Simple BERT Models for Relation Extraction and Semantic Role Labelling - Ryan Ong](https://ryanong.co.uk/2020/04/17/day-108-nlp-papers-summary-simple-bert-models-for-relation-extraction-and-semantic-role-labelling/)
-   [Day 109: NLP Papers Summary - Studying Summarization Evaluation Metrics in the Appropriate Scoring Range - Ryan Ong](https://ryanong.co.uk/2020/04/18/day-109-nlp-papers-summary-studying-summarization-evaluation-metrics-in-the-appropriate-scoring-range/)
-   [Day 110: NLP Papers Summary - Double Embeddings and CNN-based Sequence Labelling for Aspect Extraction - Ryan Ong](https://ryanong.co.uk/2020/04/19/day-110-nlp-papers-summary-double-embeddings-and-cnn-based-sequence-labelling-for-aspect-extraction/)
-   [Day 111: NLP Papers Summary - The Risk of Racial Bias in Hate Speech Detection - Ryan Ong](https://ryanong.co.uk/2020/04/20/day-111-nlp-papers-summary-the-risk-of-racial-bias-in-hate-speech-detection/)
-   [Day 112: NLP Papers Summary - A Challenge Dataset and Effective Models for Aspect-Based Sentiment Analysis - Ryan Ong](https://ryanong.co.uk/2020/04/21/day-112-nlp-papers-summary-a-challenge-dataset-and-effective-models-for-aspect-based-sentiment-analysis/)
-   [Day 113: NLP Papers Summary - On Extractive and Abstractive Neural Document Summarization with Transformer Language Models - Ryan Ong](https://ryanong.co.uk/2020/04/22/day-113-nlp-papers-summary-on-extractive-and-abstractive-neural-document-summarization-with-transformer-language-models/)
-   [Day 114: NLP Papers Summary - A Summarization System for Scientific Documents - Ryan Ong](https://ryanong.co.uk/2020/04/23/day-114-nlp-papers-summary-a-summarization-system-for-scientific-documents/)
-   [Day 115: NLP Papers Summary - SCIBERT: A Pretrained Language Model for Scientific Text - Ryan Ong](https://ryanong.co.uk/2020/04/24/day-115-nlp-papers-summary-scibert-a-pretrained-language-model-for-scientific-text/)
-   [Day 116: NLP Papers Summary - Data-driven Summarization of Scientific Articles - Ryan Ong](https://ryanong.co.uk/2020/04/25/day-116-nlp-papers-summary-data-driven-summarization-of-scientific-articles/)
-   [Day 117: NLP Papers Summary - Abstract Text Summarization: A Low Resource Challenge - Ryan Ong](https://ryanong.co.uk/2020/04/26/day-117-nlp-papers-summary-abstract-text-summarization-a-low-resource-challenge/)
-   [Day 118: NLP Papers Summary - Extractive Summarization of Long Documents by Combining Global and Local Context - Ryan Ong](https://ryanong.co.uk/2020/04/27/day-118-nlp-papers-summary-extractive-summarization-of-long-documents-by-combining-global-and-local-context/)
-   [Day 119: NLP Papers Summary - An Argument-Annotated Corpus of Scientific Publications - Ryan Ong](https://ryanong.co.uk/2020/04/28/day-119-nlp-papers-summary-an-argument-annotated-corpus-of-scientific-publications/)
-   [Day 120: NLP Papers Summary - A Simple Theoretical Model of Importance for Summarization - Ryan Ong](https://ryanong.co.uk/2020/04/29/day-120-nlp-papers-summary-a-simple-theoretical-model-of-importance-for-summarization/)
-   [Day 121: NLP Papers Summary - Concept Pointer Network for Abstractive Summarization - Ryan Ong](https://ryanong.co.uk/2020/04/30/day-121-nlp-papers-summary-concept-pointer-network-for-abstractive-summarization/)
-   [Day 122: NLP Papers Summary - Applying BERT to Document Retrieval with Birch - Ryan Ong](https://ryanong.co.uk/2020/05/01/day-122-nlp-papers-summary-applying-bert-to-document-retrieval-with-birch/)
-   [Day 123: NLP Papers Summary - Context-aware Embedding for Targeted Aspect-based Sentiment Analysis - Ryan Ong](https://ryanong.co.uk/2020/05/02/day-123-nlp-papers-summary-context-aware-embedding-for-targeted-aspect-based-sentiment-analysis/)
-   [Day 124: NLP Papers Summary - TLDR: Extreme Summarization of Scientific Documents - Ryan Ong](https://ryanong.co.uk/2020/05/03/day-124-nlp-papers-summary-tldr-extreme-summarization-of-scientific-documents/)
-   [Day 125: NLP Papers Summary - A2N: Attending to Neighbors for Knowledge Graph Inference - Ryan Ong](https://ryanong.co.uk/2020/05/04/day-125-nlp-papers-summary-a2n-attending-to-neighbors-for-knowledge-graph-inference/)
-   [Day 126: NLP Papers Summary - Neural News Recommendation with Topic-Aware News Representation - Ryan Ong](https://ryanong.co.uk/2020/05/05/day-126-nlp-papers-summary-neural-news-recommendation-with-topic-aware-news-representation/)
-   [Day 127: NLP Papers Summary - Neural Approaches to Conversational AI - Introduction - Ryan Ong](https://ryanong.co.uk/2020/05/06/day-127-nlp-papers-summary-neural-approaches-to-conversational-ai-introduction/)
-   [Day 128: NLP Papers Summary - Neural Approaches to Conversational AI - KB-QA {Symbolic Methods} - Ryan Ong](https://ryanong.co.uk/2020/05/07/day-128-nlp-papers-summary-neural-approaches-to-conversational-ai-kb-qa-symbolic-methods/)
-   [Day 129: NLP Papers Summary - Neural Approaches to Conversational AI - KB-QA {Neural Methods} - Ryan Ong](https://ryanong.co.uk/2020/05/08/day-129-nlp-papers-summary-neural-approaches-to-conversational-ai-kb-qa-neural-methods/)
-   [Day 130: NLP Papers Summary - Neural Approaches to Conversational AI - Text-QA {MRC} - Ryan Ong](https://ryanong.co.uk/2020/05/09/day-130-nlp-papers-summary-neural-approaches-to-conversational-ai-text-qa-mrc/)
-   [Day 131: NLP Papers Summary - Neural Approaches to Conversational AI - Task-Oriented Systems {Introduction} - Ryan Ong](https://ryanong.co.uk/2020/05/10/day-131-nlp-papers-summary-neural-approaches-to-conversational-ai-task-oriented-systems-introduction/)
-   [Day 132: NLP Papers Summary - Neural Approaches to Conversational AI - Task-Oriented Systems {Evaluation Metrics} - Ryan Ong](https://ryanong.co.uk/2020/05/11/day-132-nlp-papers-summary-neural-approaches-to-conversational-ai-task-oriented-systems-evaluation-metrics/)
-   [Day 133: NLP Papers Summary - Neural Approaches to Conversational AI - NLU and DST - Ryan Ong](https://ryanong.co.uk/2020/05/12/day-133-nlp-papers-summary-neural-approaches-to-conversational-ai-nlu-and-dst/)
-   [Day 134: NLP Papers Summary - Neural Approaches to Conversational AI - NLG and E2E - Ryan Ong](https://ryanong.co.uk/2020/05/13/day-134-nlp-papers-summary-neural-approaches-to-conversational-ai-nlg-and-e2e/)
-   [Day 135: NLP Papers Summary - Neural Approaches to Conversational AI - E2E Social Bots - Ryan Ong](https://ryanong.co.uk/2020/05/14/day-135-nlp-papers-summary-neural-approaches-to-conversational-ai-e2e-social-bots/)
-   [Day 136: NLP Papers Summary - Neural Approaches to Conversational AI - Social Bot's Challenges - Ryan Ong](https://ryanong.co.uk/2020/05/15/day-136-nlp-papers-summary-neural-approaches-to-conversational-ai-social-bots-challenges/)
-   [Day 137: NLP Papers Summary - Neural Approaches to Conversational AI - Social Bot's Landscape - Ryan Ong](https://ryanong.co.uk/2020/05/16/day-137-nlp-papers-summary-neural-approaches-to-conversational-ai-social-bots-landscape/)
-   [Day 138: NLP Papers Summary - Neural Approaches to Conversational AI - Conversational AI in Industry - Ryan Ong](https://ryanong.co.uk/2020/05/17/day-138-nlp-papers-summary-neural-approaches-to-conversational-ai-conversational-ai-in-industry/)
-   [Day 139: NLP Papers Summary - Neural Approaches to Conversational AI - Conclusion & Research Trends - Ryan Ong](https://ryanong.co.uk/2020/05/18/day-139-nlp-papers-summary-neural-approaches-to-conversational-ai-conclusion-research-trends/)
-   [Day 140: NLP Papers Summary - Multimodal Machine Learning for Automated ICD Coding - Ryan Ong](https://ryanong.co.uk/2020/05/19/day-140-nlp-papers-summary-multimodal-machine-learning-for-automated-icd-coding/)
-   [Day 141: NLP Papers Summary - TextAttack: A Framework for Adversarial Attacks in Natural Language Processing - Ryan Ong](https://ryanong.co.uk/2020/05/20/day-141-nlp-papers-summary-textattack-a-framework-for-adversarial-attacks-in-natural-language-processing/)
-   [Day 142: NLP Papers Summary - Measuring Emotions in the COVID-19 Real World Worry Dataset - Ryan Ong](https://ryanong.co.uk/2020/05/21/day-142-nlp-papers-summary-measuring-emotions-in-the-covid-19-real-world-worry-dataset/)
-   [Day 143: NLP Papers Summary - Unsupervised Pseudo-Labeling for Extractive Summarization on Electronic Health Records - Ryan Ong](https://ryanong.co.uk/2020/05/22/day-143-nlp-papers-summary-unsupervised-pseudo-labeling-for-extractive-summarization-on-electronic-health-records/)
-   [Day 144: NLP Papers Summary - Attend to Medical Ontologies: Content Selection for Clinical Abstractive Summarization - Ryan Ong](https://ryanong.co.uk/2020/05/23/day-144-nlp-papers-summary-attend-to-medical-ontologies-content-selection-for-clinical-abstractive-summarization/)
-   [Day 145: NLP Papers Summary - SUPERT: Towards New Frontiers in Unsupervised Evaluation Metrics for Multi-Document Summarization - Ryan Ong](https://ryanong.co.uk/2020/05/24/day-145-nlp-papers-summary-supert-towards-new-frontiers-in-unsupervised-evaluation-metrics-for-multi-document-summarization/)
-   [Day 146: NLP Papers Summary - Exploring Content Selection in Summarization of Novel Chapters - Ryan Ong](https://ryanong.co.uk/2020/05/25/day-146-nlp-papers-summary-exploring-content-selection-in-summarization-of-novel-chapters/)
-   [Day 147: NLP Papers Summary - Two Birds, One Stone: A Simple, Unified Model for Text Generation from Structured and Unstructured Data - Ryan Ong](https://ryanong.co.uk/2020/05/26/day-147-nlp-papers-summary-two-birds-one-stone-a-simple-unified-model-for-text-generation-from-structured-and-unstructured-data/)
-   [Day 148: NLP Papers Summary - A Transformer-based Approach for Source Code Summarization - Ryan Ong](https://ryanong.co.uk/2020/05/27/day-148-nlp-papers-summary-a-transformer-based-approach-for-source-code-summarization/)
-   [Day 149: NLP Papers Summary - MOOCCube: A Large-scale Data Repository for NLP Applications in MOOCs - Ryan Ong](https://ryanong.co.uk/2020/05/28/day-149-nlp-papers-summary-mooccube-a-large-scale-data-repository-for-nlp-applications-in-moocs/)
-   [Day 150: NLP Papers Summary - Will-They-Wont-They: A Very Large Dataset for Stance Detection on Twitter - Ryan Ong](https://ryanong.co.uk/2020/05/29/day-150-nlp-papers-summary-will-they-wont-they-a-very-large-dataset-for-stance-detection-on-twitter/)
-   [Day 151: NLP Papers Summary - A Large-Scale Multi-Document Summarization Dataset from the Wikipedia Current Events Portal - Ryan Ong](https://ryanong.co.uk/2020/05/30/day-151-nlp-papers-summary-a-large-scale-multi-document-summarization-dataset-from-the-wikipedia-current-events-portal/)
-   [Day 152: NLP Papers Summary - OPINIONDIGEST: A Simple Framework for Opinion Summarization - Ryan Ong](https://ryanong.co.uk/2020/05/31/day-152-nlp-papers-summary-opiniondigest-a-simple-framework-for-opinion-summarization/)
-   [Day 153: NLP Papers Summary - Span-ConveRT: Few-shot Span Extraction for Dialog with Pretrained Conversational Representations - Ryan Ong](https://ryanong.co.uk/2020/06/01/day-153-nlp-papers-summary-span-convert-few-shot-span-extraction-for-dialog-with-pretrained-conversational-representations/)
-   [Day 154: NLP Papers Summary - Contextual Embeddings: When Are They Worth It? - Ryan Ong](https://ryanong.co.uk/2020/06/02/day-154-nlp-papers-summary-contextual-embeddings-when-are-they-worth-it/)
-   [Day 155: NLP Papers Summary - TRAIN ONCE, TEST ANYWHERE: ZERO-SHOT LEARNING FOR TEXT CLASSIFICATION - Ryan Ong](https://ryanong.co.uk/2020/06/03/day-155-nlp-papers-summary-train-once-test-anywhere-zero-shot-learning-for-text-classification/)
-   [Day 156: NLP Papers Summary - Asking and Answering Questions to Evaluate the Factual Consistency of Summaries - Ryan Ong](https://ryanong.co.uk/2020/06/04/day-156-nlp-papers-summary-asking-and-answering-questions-to-evaluate-the-factual-consistency-of-summaries/)
-   [Day 157: NLP Papers Summary - Explainable Prediction of Medical Codes from Clinical Text - Ryan Ong](https://ryanong.co.uk/2020/06/05/day-157-nlp-papers-summary-explainable-prediction-of-medical-codes-from-clinical-text/)
-   [Day 158: NLP Papers Summary - Embarrassingly Simple Unsupervised Aspect Extraction - Ryan Ong](https://ryanong.co.uk/2020/06/06/day-158-nlp-papers-summary-embarrassingly-simple-unsupervised-aspect-extraction/)
-   [Day 159: NLP Papers Summary - ICD Coding from Clinical Text Using Multi-Filter Residual Convolutional Neural Network - Ryan Ong](https://ryanong.co.uk/2020/06/07/day-159-nlp-papers-summary-icd-coding-from-clinical-text-using-multi-filter-residual-convolutional-neural-network/)
-   [Day 160: NLP Papers Summary - Extractive Summarization as Text Matching - Ryan Ong](https://ryanong.co.uk/2020/06/08/day-160-nlp-papers-summary-extractive-summarization-as-text-matching/)
-   [Day 161: NLP Papers Summary - BLEURT: Learning Robust Metrics for Text Generation - Ryan Ong](https://ryanong.co.uk/2020/06/09/day-161-nlp-papers-summary-bleurt-learning-robust-metrics-for-text-generation/)
-   [Day 162: Learn NLP With Me  Fast.Ai NLP Course  Revisiting Nave Bayes & Regex - Ryan Ong](https://ryanong.co.uk/2020/06/10/day-162-learn-nlp-with-me-fast-ai-nlp-course-revisiting-naive-bayes-regex/)
-   [Day 163: How to build a Language Model from scratch - Implementation - Ryan Ong](https://ryanong.co.uk/2020/06/11/day-163-how-to-build-a-language-model-from-scratch-implementation/)
-   [Day 164: Learn NLP With Me  Fast.Ai NLP Course  Transfer Learning - Ryan Ong](https://ryanong.co.uk/2020/06/12/day-164-learn-nlp-with-me-fast-ai-nlp-course-transfer-learning/)
-   [Day 165: Learn NLP With Me  Fast.Ai NLP Course  ULMFit for non-English Languages - Ryan Ong](https://ryanong.co.uk/2020/06/13/day-165-learn-nlp-with-me-fast-ai-nlp-course-ulmfit-for-non-english-languages/)
-   [Day 166: NLP Papers Summary - Publicly Available Clinical BERT Embeddings - Ryan Ong](https://ryanong.co.uk/2020/06/14/day-166-nlp-papers-summary-publicly-available-clinical-bert-embeddings/)
-   [Day 167: NLP Papers Summary - Ontology-Aware Clinical Abstractive Summarization - Ryan Ong](https://ryanong.co.uk/2020/06/15/day-167-nlp-papers-summary-ontology-aware-clinical-abstractive-summarization/)
-   [Day 168: Learn NLP With Me  Fast.Ai NLP Course  Understanding RNNs and Seq2Seq Translation - Ryan Ong](https://ryanong.co.uk/2020/06/16/day-168-learn-nlp-with-me-fast-ai-nlp-course-understanding-rnns-and-seq2seq-translation/)
-   [Day 169: Learn NLP With Me  Fast.Ai NLP Course  Word Embeddings Quantify Stereotypes and Text Generation Algorithms - Ryan Ong](https://ryanong.co.uk/2020/06/17/day-169-learn-nlp-with-me-fast-ai-nlp-course-word-embeddings-quantify-stereotypes-and-text-generation-algorithms/)
-   [Day 170: Learn NLP With Me  Fast.Ai NLP Course  Algorithmic Bias - Ryan Ong](https://ryanong.co.uk/2020/06/18/day-170-learn-nlp-with-me-fast-ai-nlp-course-algorithmic-bias/)
-   [Day 171: Learn NLP With Me  Fast.Ai NLP Course  Transformers and Language Translation - Ryan Ong](https://ryanong.co.uk/2020/06/19/day-171-learn-nlp-with-me-fast-ai-nlp-course-transformers-and-language-translation/)
-   [Day 172: Learn NLP With Me  Fast.Ai NLP Course  Disinformation in Text {END COURSE} - Ryan Ong](https://ryanong.co.uk/2020/06/20/day-172-learn-nlp-with-me-fast-ai-nlp-course-disinformation-in-text-end-course/)
-   [Day 173: NLP Discovery - Text-To-Text Transfer Transformer {T5} - Ryan Ong](https://ryanong.co.uk/2020/06/21/day-173-nlp-discovery-text-to-text-transfer-transformer-t5/)
-   [Day 174: NLP Papers Summary - PEGASUS: Pre-training with Extracted Gap-sentences for Abstractive Summarization - Ryan Ong](https://ryanong.co.uk/2020/06/22/day-174-nlp-papers-summary-pegasus-pre-training-with-extracted-gap-sentences-for-abstractive-summarization/)
-   [Day 175: NLP Papers Summary - GPT-3 : Introduction and Context - Ryan Ong](https://ryanong.co.uk/2020/06/23/day-175-nlp-papers-summary-gpt-3-introduction-and-context/)
-   [Day 176: NLP Papers Summary - GPT-3 : Training and Evaluation Methods - Ryan Ong](https://ryanong.co.uk/2020/06/24/day-176-nlp-papers-summary-gpt-3-training-and-evaluation-methods/)
-   [Day 177: NLP Papers Summary - GPT-3 : Limitations - Ryan Ong](https://ryanong.co.uk/2020/06/25/day-177-nlp-papers-summary-gpt-3-limitations/)
-   [Day 178: NLP Papers Summary - GPT-3 : Broader Impacts - Ryan Ong](https://ryanong.co.uk/2020/06/26/day-178-nlp-papers-summary-gpt-3-broader-impacts/)
-   [Day 179: Learning PyTorch - Revisiting Concepts - Ryan Ong](https://ryanong.co.uk/2020/06/27/day-179-learning-pytorch-revisiting-concepts/)
-   [Day 180: Learning PyTorch - Language Model with nn.Transformer and TorchText {Part 1} - Ryan Ong](https://ryanong.co.uk/2020/06/28/day-180-learning-pytorch-language-model-with-nn-transformer-and-torchtext-part-1/)
-   [Day 181: Learning PyTorch - Language Model with nn.Transformer and TorchText {Part 2} - Ryan Ong](https://ryanong.co.uk/2020/06/29/day-181-learning-pytorch-language-model-with-nn-transformer-and-torchtext-part-2/)
-   [Day 182: Learning PyTorch - Custom Dataset and DataLoader - Ryan Ong](https://ryanong.co.uk/2020/06/30/day-182-learning-pytorch-custom-dataset-and-dataloader/)
-   [Day 183: Learning PyTorch - TorchText Introduction - Ryan Ong](https://ryanong.co.uk/2020/07/01/day-183-learning-pytorch-torchtext-introduction/)
-   [Day 184: Learning PyTorch - Machine Translation with TorchText - Ryan Ong](https://ryanong.co.uk/2020/07/02/day-184-learning-pytorch-machine-translation-with-torchtext/)
-   [Day 185: NLP Papers Summary - A Discourse-Aware Attention Model for Abstractive Summarization of Long Documents - Ryan Ong](https://ryanong.co.uk/2020/07/03/day-185-nlp-papers-summary-a-discourse-aware-attention-model-for-abstractive-summarization-of-long-documents/)
-   [Day 186: NLP Papers Summary - Contextualizing Citations for Scientific Summarization using Word Embeddings and Domain Knowledge - Ryan Ong](https://ryanong.co.uk/2020/07/04/day-186-nlp-papers-summary-contextualizing-citations-for-scientific-summarization-using-word-embeddings-and-domain-knowledge/)
-   [Day 187: Learn NLP With Me  Embeddings of Language, Knowledge Representation, and Reasoning - Ryan Ong](https://ryanong.co.uk/2020/07/05/day-187-learn-nlp-with-me-embeddings-of-language-knowledge-representation-and-reasoning/)
-   [Day 188: NLP Papers Summary - A Supervised Approach to Extractive Summarisation of Scientific Papers - Ryan Ong](https://ryanong.co.uk/2020/07/06/day-188-nlp-papers-summary-a-supervised-approach-to-extractive-summarisation-of-scientific-papers/)
-   [Day 189: Learning PyTorch - PyTorch Lightning Introduction - Ryan Ong](https://ryanong.co.uk/2020/07/07/day-189-learning-pytorch-pytorch-lightning-introduction/)
-   [Day 190: Learning PyTorch - PyTorch Lightning Structure {with codes} - Ryan Ong](https://ryanong.co.uk/2020/07/08/day-190-learning-pytorch-pytorch-lightning-structure-with-codes/)
-   [Day 191: Summarisation of arXiv papers using TextRank - Does it work? - Ryan Ong](https://ryanong.co.uk/2020/07/09/day-191-summarisation-of-arxiv-papers-using-textrank-does-it-work/)
-   [Day 192: NLP Papers Summary - Guiding Extractive Summarization with Question-Answering Rewards - Ryan Ong](https://ryanong.co.uk/2020/07/10/day-192-nlp-papers-summary-guiding-extractive-summarization-with-question-answering-rewards/)
-   [Day 193: Learning PyTorch - Tweets Sentiment Extraction {Part 1} - Ryan Ong](https://ryanong.co.uk/2020/07/11/day-193-learning-pytorch-tweets-sentiment-extraction-part-1/)
-   [Day 194: Learning PyTorch - Tweets Sentiment Extraction {Part 2} - Ryan Ong](https://ryanong.co.uk/2020/07/12/day-194-learning-pytorch-tweets-sentiment-extraction-part-2/)
-   [Day 195: Learn NLP With Me  What is Coreference Resolution? - Ryan Ong](https://ryanong.co.uk/2020/07/13/day-195-learn-nlp-with-me-what-is-coreference-resolution/)
-   [Day 196: Coreference Resolution with NeuralCoref {SpaCy} - Ryan Ong](https://ryanong.co.uk/2020/07/14/day-196-coreference-resolution-with-neuralcoref-spacy/)
-   [Day 197: Learn NLP With Me  Filling the Gaps with NLP Interview Questions - Ryan Ong](https://ryanong.co.uk/2020/07/15/day-197-learn-nlp-with-me-what-is-coreference-resolution/)
-   [Day 198: Learn NLP With Me  Filling the Gaps with NLP Interview Questions II - Ryan Ong](https://ryanong.co.uk/2020/07/16/day-198-learn-nlp-with-me-filling-the-gaps-with-nlp-interview-questions-ii/)
-   [Day 199: Learn NLP With Me  Filling the Gaps with NLP Interview Questions III - Ryan Ong](https://ryanong.co.uk/2020/07/17/day-199-learn-nlp-with-me-filling-the-gaps-with-nlp-interview-questions-iii/)
-   [Day 200: Learn NLP With Me  Filling the Gaps with NLP Interview Questions IV - Ryan Ong](https://ryanong.co.uk/2020/07/18/day-200-learn-nlp-with-me-filling-the-gaps-with-nlp-interview-questions-iv/)
-   [Day 201: Abbreviation Resolution and UMLS Entity Linking using SciSpaCy - Ryan Ong](https://ryanong.co.uk/2020/07/19/day-201-abbreviation-resolution-and-umls-entity-linking-using-scispacy/)
-   [Day 202: Learn NLP With Me  NLP and Transfer Learning Revisit - Ryan Ong](https://ryanong.co.uk/2020/07/20/day-202-learn-nlp-with-me-nlp-and-transfer-learning-revisit/)
-   [Day 203: Learn NLP With Me  Attention Mechanism and Transformers Revisit - Ryan Ong](https://ryanong.co.uk/2020/07/21/day-203-learn-nlp-with-me-attention-mechanism-and-transformers-revisit/)
-   [Day 204: Learn NLP With Me  Subword Tokenisation and Normalisation - Ryan Ong](https://ryanong.co.uk/2020/07/22/day-204-learn-nlp-with-me-subword-tokenisation-and-normalisation/)
-   [Day 205: Learn NLP With Me  Zero-Shot Learning for Text Classification - Ryan Ong](https://ryanong.co.uk/2020/07/23/day-205-learn-nlp-with-me-zero-shot-learning-for-text-classification/)
-   [Day 206: NLP Papers Summary - Transformers and Pointer-Generator Networks for Abstractive Summarization - Ryan Ong](https://ryanong.co.uk/2020/07/24/day-206-nlp-papers-summary-transformers-and-pointer-generator-networks-for-abstractive-summarization/)
-   [Day 207: Learning PyTorch - Fine Tuning BERT for Sentiment Analysis {Part One} - Ryan Ong](https://ryanong.co.uk/2020/07/25/day-207-learning-pytorch-fine-tuning-bert-for-sentiment-analysis-part-one/)
-   [Day 208: Learning PyTorch - Fine Tuning BERT for Sentiment Analysis {Part Two} - Ryan Ong](https://ryanong.co.uk/2020/07/26/day-208-learning-pytorch-fine-tuning-bert-for-sentiment-analysis-part-two/)
-   [Day 209: Introduction to Clustering - Ryan Ong](https://ryanong.co.uk/2020/07/27/day-209-introduction-to-clustering/)
-   [Day 210: Describing 4 different clustering algorithms - Ryan Ong](https://ryanong.co.uk/2020/07/28/day-210-describing-4-different-clustering-algorithms/)
-   [Day 211: When to use which clustering algorithms? - Ryan Ong](https://ryanong.co.uk/2020/07/29/day-211-when-to-use-which-clustering-algorithms/)
-   [Day 212: K-Means Clustering using SK-Learn and NLTK {Quick Read} - Ryan Ong](https://ryanong.co.uk/2020/07/30/day-212-k-means-clustering-using-sk-learn-and-nltk-quick-read/)
-   [Day 213: Learn NLP With Me  SLP Textbook Ch.21 - Lexicons for Sentiment, Affect, and Connotation I - Ryan Ong](https://ryanong.co.uk/2020/07/31/day-213-learn-nlp-with-me-slp-textbook-lexicons-for-sentiment-affect-and-connotation-i/)
-   [Day 214: Learn NLP With Me  SLP Textbook Ch.21 - Lexicons for Sentiment, Affect, and Connotation II - Ryan Ong](https://ryanong.co.uk/2020/08/01/day-214-learn-nlp-with-me-slp-textbook-lexicons-for-sentiment-affect-and-connotation-ii/)
-   [Day 215: Learn NLP With Me  SLP Textbook Ch.21 - Lexicons for Sentiment, Affect, and Connotation III - Ryan Ong](https://ryanong.co.uk/2020/08/02/day-215-learn-nlp-with-me-slp-textbook-ch-21-lexicons-for-sentiment-affect-and-connotation-iii/)
-   [Day 216: Learn NLP With Me  SLP Textbook Ch.21 - Lexicons for Sentiment, Affect, and Connotation IV - Ryan Ong](https://ryanong.co.uk/2020/08/03/day-216-learn-nlp-with-me-slp-textbook-ch-21-lexicons-for-sentiment-affect-and-connotation-iv/)
-   [Day 217: Learn NLP With Me  SLP Textbook Ch.7  Neural Networks and Neural Language Models I - Ryan Ong](https://ryanong.co.uk/2020/08/04/day-217-learn-nlp-with-me-slp-textbook-ch-7-neural-networks-and-neural-language-models-i/)
-   [Day 218: Learn NLP With Me  SLP Textbook Ch.7  Neural Networks and Neural Language Models II - Ryan Ong](https://ryanong.co.uk/2020/08/05/day-218-learn-nlp-with-me-slp-textbook-ch-7-neural-networks-and-neural-language-models-ii/)
-   [Day 219: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution I - Ryan Ong](https://ryanong.co.uk/2020/08/06/day-219-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-i/)
-   [Day 220: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution II - Ryan Ong](https://ryanong.co.uk/2020/08/07/day-220-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-ii/)
-   [Day 221: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution III - Ryan Ong](https://ryanong.co.uk/2020/08/08/day-221-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-iii/)
-   [Day 222: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution IV - Ryan Ong](https://ryanong.co.uk/2020/08/09/day-222-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-iv/)
-   [Day 223: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution V - Ryan Ong](https://ryanong.co.uk/2020/08/10/day-223-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-v/)
-   [Day 224: Learn NLP With Me  SLP Textbook Ch.22  Coreference Resolution VI - Ryan Ong](https://ryanong.co.uk/2020/08/11/day-224-learn-nlp-with-me-slp-textbook-ch-22-coreference-resolution-vi/)
-   [Day 225: NLP Papers Summary - Architecture of Knowledge Graph Construction Techniques - Ryan Ong](https://ryanong.co.uk/2020/08/12/day-225-nlp-papers-summary-architecture-of-knowledge-graph-construction-techniques/)
-   [Day 226: NLP Papers Summary - Anticipating Stock Market of the Renowned Companies: A Knowledge Graph Approach I - Ryan Ong](https://ryanong.co.uk/2020/08/13/day-226-nlp-papers-summary-anticipating-stock-market-of-the-renowned-companies-a-knowledge-graph-approach-i/)
-   [Day 227: Learn NLP With Me - Translate model for Knowledge Graph Embedding - Ryan Ong](https://ryanong.co.uk/2020/08/14/day-227-learn-nlp-with-me-translate-model-for-knowledge-graph-embedding/)
-   [Day 228: Learn NLP With Me - Knowledge Graph on Finance {Balance Sheets} - Ryan Ong](https://ryanong.co.uk/2020/08/15/day-228-learn-nlp-with-me-knowledge-graph-on-finance-balance-sheets/)
-   [Day 229: NLP Papers Summary - Building and Exploring an EKG for Investment Analysis - Introduction and Challenges - Ryan Ong](https://ryanong.co.uk/2020/08/16/day-229-nlp-papers-summary-building-and-exploring-an-ekg-for-investment-analysis-introduction-and-challenges/)
-   [Day 230: NLP Papers Summary - Building and Exploring an EKG for Investment Analysis - Approach Overview - Ryan Ong](https://ryanong.co.uk/2020/08/17/day-230-nlp-papers-summary-building-and-exploring-an-ekg-for-investment-analysis-approach-overview/)
-   [Day 231: NLP Papers Summary  Building and Exploring an EKG for Investment Analysis  Building Knowledge Graphs - Ryan Ong](https://ryanong.co.uk/2020/08/18/day-231-nlp-papers-summary-building-and-exploring-an-ekg-for-investment-analysis-building-knowledge-graphs/)
-   [Day 232: NLP Papers Summary  Building and Exploring an EKG for Investment Analysis  Deployment and Related Work - Ryan Ong](https://ryanong.co.uk/2020/08/19/day-232-nlp-papers-summary-building-and-exploring-an-ekg-for-investment-analysis-deployment-and-related-work/)
-   [Day 233: Learn NLP With Me - LinkedIn's Knowledge Graph - Ryan Ong](https://ryanong.co.uk/2020/08/20/day-233-learn-nlp-with-me-linkedins-knowledge-graph/)
-   [Day 234: NLP Papers Summary  Topic Modeling in Financial Documents - Ryan Ong](https://ryanong.co.uk/2020/08/21/day-234-nlp-papers-summary-topic-modeling-in-financial-documents/)
-   [Day 235: Learn NLP With Me - Topic Modelling with LSA and LDA - Ryan Ong](https://ryanong.co.uk/2020/08/22/day-235-learn-nlp-with-me-topic-modelling-with-lsa-and-lda/)
-   [Day 236: NLP Papers Summary  A BERT based Sentiment Analysis and Key Entity Detection Approach for Online Financial Texts - Ryan Ong](https://ryanong.co.uk/2020/08/23/day-236-nlp-papers-summary-a-bert-based-sentiment-analysis-and-key-entity-detection-approach-for-online-financial-texts/)
-   [Day 237: Learn NLP With Me - An Exhaustive Guide to Detecting and Fighting Neural Fake News using NLP - Ryan Ong](https://ryanong.co.uk/2020/08/24/day-237-learn-nlp-with-me-an-exhaustive-guide-to-detecting-and-fighting-neural-fake-news-using-nlp/)
-   [Day 238: NLP Implementation - Kaggle's Fake News Challenge - BERT Classifier using PyTorch and HuggingFace I - Ryan Ong](https://ryanong.co.uk/2020/08/25/day-238-nlp-implementation-kaggles-fake-news-challenge-bert-classifier-using-pytorch-and-huggingface/)
-   [Day 239: NLP Implementation - Kaggle's Fake News Challenge - BERT Classifier using PyTorch and HuggingFace II - Ryan Ong](https://ryanong.co.uk/2020/08/26/day-239-nlp-implementation-kaggles-fake-news-challenge-bert-classifier-using-pytorch-and-huggingface-ii/)
-   [Day 240: NLP Implementation - Kaggle's Fake News Challenge - BERT Classifier using PyTorch and HuggingFace III - Ryan Ong](https://ryanong.co.uk/2020/08/27/day-240-nlp-implementation-kaggles-fake-news-challenge-bert-classifier-using-pytorch-and-huggingface-iii/)
-   [Day 241: NLP Implementation - Topic Modelling and Sentiment Analysis on News Articles {Document Level} - Ryan Ong](https://ryanong.co.uk/2020/08/28/day-241-nlp-implementation-topic-modelling-and-sentiment-analysis-on-news-articles-document-level/)
-   [Day 242: NLP Implementation - Topic Modelling and Sentiment Analysis on News Articles {Sentence Level} - Ryan Ong](https://ryanong.co.uk/2020/08/29/day-242-nlp-implementation-topic-modelling-and-sentiment-analysis-on-news-articles-sentence-level/)
-   [Day 243: NLP Implementation - Entity Extraction and Linking - NER and Coreference Resolution using SpaCy - Ryan Ong](https://ryanong.co.uk/2020/08/30/day-243-nlp-implementation-entity-extraction-and-linking-ner-and-coreference-resolution-using-spacy/)
-   [Day 244: NLP Implementation - Entity Extraction and Linking - Entity Linking using DBPedia - Ryan Ong](https://ryanong.co.uk/2020/08/31/day-244-nlp-implementation-entity-extraction-and-linking-entity-linking-using-dbpedia/)
-   [Day 245: NLP Implementation - News Article Ingestion Pipeline - Putting it All Together - Ryan Ong](https://ryanong.co.uk/2020/09/01/day-245-nlp-implementation-putting-it-all-together-news-article-ingestion-pipeline/)
-   [Day 246: NLP Implementation - A Web Application for Entity Tracking - Flask Backend - Ryan Ong](https://ryanong.co.uk/2020/09/02/day-246-nlp-implementation-a-web-application-for-entity-tracking-flask-backend/)
-   [Day 247: NLP Implementation - A Web Application for Entity Tracking - React Frontend - Ryan Ong](https://ryanong.co.uk/2020/09/03/day-247-nlp-implementation-a-web-application-for-entity-tracking-react-frontend/)
-   [Day 248: NLP Implementation - A Simple Knowledge Graph Walkthrough - Ryan Ong](https://ryanong.co.uk/2020/09/04/day-248-nlp-implementation-a-simple-knowledge-graph-walkthrough/)
-   [Day 249: Learn NLP With Me - CS520 Knowledge Graphs - Lecture 1 - What is a knowledge graph? - Ryan Ong](https://ryanong.co.uk/2020/09/05/day-249-learn-nlp-with-me-cs520-knowledge-graphs-lecture-1-what-is-a-knowledge-graph/)
-   [Day 250: Learn NLP With Me - CS520 Knowledge Graphs - Lecture 2 - How to create a knowledge graph? - Ryan Ong](https://ryanong.co.uk/2020/09/06/day-250-learn-nlp-with-me-cs520-knowledge-graphs-lecture-2-how-to-create-a-knowledge-graph/)
-   [Day 251: Learn NLP With Me - CS520 Knowledge Graphs - Lecture 3 - What are some advanced knowledge graphs? - Ryan Ong](https://ryanong.co.uk/2020/09/07/day-251-learn-nlp-with-me-cs520-knowledge-graphs-lecture-3-what-are-some-advanced-knowledge-graphs/)
-   [Day 252: Learn NLP With Me - CS520 Knowledge Graphs - Lecture 4 - What are some knowledge graph inference algorithms? - Ryan Ong](https://ryanong.co.uk/2020/09/08/day-252-learn-nlp-with-me-cs520-knowledge-graphs-lecture-4-what-are-some-knowledge-graph-inference-algorithms/)
-   [Day 253: Learn NLP With Me - CS520 Knowledge Graphs - Lecture 5 - How to evolve a knowledge graph? - Ryan Ong](https://ryanong.co.uk/2020/09/09/day-253-learn-nlp-with-me-cs520-knowledge-graphs-lecture-5-how-to-evolve-a-knowledge-graph/)
-   [Day 254: Learn NLP With Me  SLP Textbook Ch.23  Discourse Coherence I - Ryan Ong](https://ryanong.co.uk/2020/09/10/day-254-learn-nlp-with-me-slp-textbook-ch-23-discourse-coherence-i/)
-   [Day 255: Learn NLP With Me  SLP Textbook Ch.23  Discourse Coherence II - Ryan Ong](https://ryanong.co.uk/2020/09/11/day-255-learn-nlp-with-me-slp-textbook-ch-23-discourse-coherence-ii/)
-   [Day 256: Learn NLP With Me  SLP Textbook Ch.23  Discourse Coherence III - Ryan Ong](https://ryanong.co.uk/2020/09/12/day-256-learn-nlp-with-me-slp-textbook-ch-23-discourse-coherence-iii/)
-   [Day 257: Learn NLP With Me  SLP Textbook Ch.23  Discourse Coherence IV - Ryan Ong](https://ryanong.co.uk/2020/09/13/day-257-learn-nlp-with-me-slp-textbook-ch-23-discourse-coherence-iv/)
-   [Day 258: Learn NLP With Me  SLP Textbook Ch.23  Discourse Coherence V - Ryan Ong](https://ryanong.co.uk/2020/09/14/day-258-learn-nlp-with-me-slp-textbook-ch-23-discourse-coherence-v/)
-   [Day 259: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots I - Ryan Ong](https://ryanong.co.uk/2020/09/15/day-259-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-i/)
-   [Day 260: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots II - Ryan Ong](https://ryanong.co.uk/2020/09/16/day-260-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-ii/)
-   [Day 261: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots III - Ryan Ong](https://ryanong.co.uk/2020/09/17/day-261-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-iii/)
-   [Day 262: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots IV - Ryan Ong](https://ryanong.co.uk/2020/09/18/day-262-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-iv/)
-   [Day 263: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots V - Ryan Ong](https://ryanong.co.uk/2020/09/19/day-263-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-v/)
-   [Day 264: Learn NLP With Me  SLP Textbook Ch.26  Dialogue Systems and Chatbots VI - Ryan Ong](https://ryanong.co.uk/2020/09/20/day-264-learn-nlp-with-me-slp-textbook-ch-26-dialogue-systems-and-chatbots-vi/)
-   [Day 265: Learn NLP With Me  Intent Classification for Chatbots {Airbnb's Approach} - Ryan Ong](https://ryanong.co.uk/2020/09/21/day-265-learn-nlp-with-me-intent-classification-for-chatbots-airbnbs-approach/)
-   [Day 266: Learn NLP With Me  Building a Conversational Interface I - Ryan Ong](https://ryanong.co.uk/2020/09/22/day-266-learn-nlp-with-me-building-a-conversational-interface-i/)
-   [Day 267: Learn NLP With Me  Building a Conversational Interface II - Ryan Ong](https://ryanong.co.uk/2020/09/23/day-267-learn-nlp-with-me-building-a-conversational-interface-ii/)
-   [Day 268: Learn NLP With Me  Building a Conversational Interface III - Ryan Ong](https://ryanong.co.uk/2020/09/24/day-268-learn-nlp-with-me-building-a-conversational-interface-iii/)
-   [Day 269: Learn NLP With Me  Training the named entity recognizer using SpaCy I - Ryan Ong](https://ryanong.co.uk/2020/09/25/day-269-learn-nlp-with-me-training-the-named-entity-recognizer-using-spacy-i/)
-   [Day 270: Learn NLP With Me  Training the named entity recognizer using SpaCy II - Ryan Ong](https://ryanong.co.uk/2020/09/26/day-270-learn-nlp-with-me-training-the-named-entity-recognizer-using-spacy-ii/)
-   [Day 271: Learn NLP With Me  Hidden Markov Models {HMMs} I - Ryan Ong](https://ryanong.co.uk/2020/09/27/day-271-learn-nlp-with-me-hidden-markov-models-hmms-i/)
-   [Day 272: NLP Discovery - Prodigy Annotation Tool - Ryan Ong](https://ryanong.co.uk/2020/09/28/day-272-nlp-discovery-prodigy-annotation-tool/)
-   [Day 273: Learn NLP With Me  Hidden Markov Models {HMMs} II - Ryan Ong](https://ryanong.co.uk/2020/09/29/day-273-learn-nlp-with-me-hidden-markov-models-hmms-ii/)
-   [Day 274: Learn NLP With Me  Training the named entity recognizer using SpaCy III - Ryan Ong](https://ryanong.co.uk/2020/09/30/day-274-learn-nlp-with-me-training-the-named-entity-recognizer-using-spacy-iii/)
-   [Day 275: Ryan's PhD Journey - The Beginning of a New Chapter - Starting with Why - Ryan Ong](https://ryanong.co.uk/2020/10/01/day-275-the-beginning-of-a-new-chapter-start-with-why/)
-   [Day 276: Learn NLP With Me - Knowledge Graph for Financial Services - Ryan Ong](https://ryanong.co.uk/2020/10/02/day-276-learn-nlp-with-me-knowledge-graph-for-financial-services/)
-   [Day 277: Learn NLP With Me - Using Knowledge Graphs to Identify Investment Opportunities - Ryan Ong](https://ryanong.co.uk/2020/10/03/day-277-learn-nlp-with-me-using-knowledge-graphs-to-identify-investment-opportunities/)
-   [Day 278: Learn NLP With Me - Richer Sentence Embeddings using Sentence-BERT - Ryan Ong](https://ryanong.co.uk/2020/10/04/day-278-learn-nlp-with-me-richer-sentence-embeddings-using-sentence-bert/)
-   [Day 279: Learn NLP With Me - Trustworthy and Explainable AI Achieved Through Knowledge Graphs - Ryan Ong](https://ryanong.co.uk/2020/10/05/day-279-learn-nlp-with-me-trustworthy-and-explainable-ai-achieved-through-knowledge-graphs/)
-   [Day 280: NLP Discovery - lang.ais Unsupervised Intent Discovery {Whitepaper} - Ryan Ong](https://ryanong.co.uk/2020/10/06/day-280-nlp-discovery-lang-ais-unsupervised-intent-discovery-whitepaper/)
-   [Day 281: NLP Papers Summary - Knowledge Reasoning over Knowledge Graph I - Ryan Ong](https://ryanong.co.uk/2020/10/07/day-281-nlp-papers-summary-knowledge-reasoning-over-knowledge-graph-i/)
-   [Day 282: Learn NLP With Me - Building an Enterprise Knowledge Graph at Uber - Ryan Ong](https://ryanong.co.uk/2020/10/08/day-282-learn-nlp-with-me-building-an-enterprise-knowledge-graph-at-uber/)
-   [Day 283: Learn NLP With Me - Hidden Markov Models {HMMs} III - Ryan Ong](https://ryanong.co.uk/2020/10/09/day-283-learn-nlp-with-me-hidden-markov-models-hmms-iii/)
-   [Day 284: Learn NLP With Me - Introduction to Flair for NLP - Ryan Ong](https://ryanong.co.uk/2020/10/10/day-284-learn-nlp-with-me-introduction-to-flair-for-nlp/)
-   [Day 285: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 1 - What Is a Knowledge Graph I - Ryan Ong](https://ryanong.co.uk/2020/10/11/day-285-learn-nlp-with-me-domain-specific-kg-textbook-chapter-1-what-is-a-knowledge-graph-i/)
-   [Day 286: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 1 - What Is a Knowledge Graph II - Ryan Ong](https://ryanong.co.uk/2020/10/12/day-286-learn-nlp-with-me-domain-specific-kg-textbook-chapter-1-what-is-a-knowledge-graph-ii/)
-   [Day 287: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 2 - Information Extraction I - Ryan Ong](https://ryanong.co.uk/2020/10/13/day-287-learn-nlp-with-me-domain-specific-kg-textbook-chapter-2-information-extraction-i/)
-   [Day 288: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 2 - Information Extraction II - Ryan Ong](https://ryanong.co.uk/2020/10/14/day-288-learn-nlp-with-me-domain-specific-kg-textbook-chapter-2-information-extraction-ii/)
-   [Day 289: Ryans PhD Journey  Neo4j Graph Fundamentals - Ryan Ong](https://ryanong.co.uk/2020/10/15/day-289-ryans-phd-journey-neo4j-graph-fundamentals/)
-   [Day 290: Ryans PhD Journey  Cypher Introduction - Ryan Ong](https://ryanong.co.uk/2020/10/16/day-290-ryans-phd-journey-cypher-introduction/)
-   [Day 291: Learn NLP With Me - Named-Entity {NER} evaluation metrics based on entity-level - Ryan Ong](https://ryanong.co.uk/2020/10/17/day-291-learn-nlp-with-me-named-entity-ner-evaluation-metrics-based-on-entity-level/)
-   [Day 292: Ryans PhD Journey  Cypher's Queries and Patterns - Ryan Ong](https://ryanong.co.uk/2020/10/18/day-292-ryans-phd-journey-cypher-queries-and-patterns/)
-   [Day 293: Ryans PhD Journey  Cypher's CRUD Operations - Ryan Ong](https://ryanong.co.uk/2020/10/19/day-293-ryans-phd-journey-cyphers-crud-operations/)
-   [Day 294: Ryans PhD Journey  Cypher's Filtering Query Results - Ryan Ong](https://ryanong.co.uk/2020/10/20/day-294-ryans-phd-journey-cyphers-filtering-query-results/)
-   [Day 295: Ryans PhD Journey  Cypher's Controlling Query Processing - Ryan Ong](https://ryanong.co.uk/2020/10/21/day-295-ryans-phd-journey-cyphers-controlling-query-processing/)
-   [Day 296: Ryans PhD Journey  Cypher's Datetimes and Subqueries - Ryan Ong](https://ryanong.co.uk/2020/10/22/day-296-ryans-phd-journey-cyphers-datetimes-and-subqueries/)
-   [Day 297: Ryans PhD Journey  Cypher's User Defined Procedures and Functions - Ryan Ong](https://ryanong.co.uk/2020/10/23/day-297-ryans-phd-journey-cyphers-user-defined-procedures-and-functions/)
-   [Day 298: Ryans PhD Journey  Cypher's Hello World - Movie Graph Tutorial I - Ryan Ong](https://ryanong.co.uk/2020/10/24/day-298-ryans-phd-journey-cyphers-hello-world-movie-graph-tutorial-i/)
-   [Day 299: Ryans PhD Journey  Cypher's Hello World - Movie Graph Tutorial II - Ryan Ong](https://ryanong.co.uk/2020/10/25/day-299-ryans-phd-journey-cyphers-hello-world-movie-graph-tutorial-ii/)
-   [Day 300: Ryans PhD Journey  Cypher's Recommendation Engine Tutorial - Ryan Ong](https://ryanong.co.uk/2020/10/26/day-300-ryans-phd-journey-cyphers-recommendation-engine-tutorial/)
-   [Day 301: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 2 - Information Extraction III - Ryan Ong](https://ryanong.co.uk/2020/10/27/day-301-learn-nlp-with-me-domain-specific-kg-textbook-chapter-2-information-extraction-iii/)
-   [Day 302: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 2 - Information Extraction IV - Ryan Ong](https://ryanong.co.uk/2020/10/28/day-302-learn-nlp-with-me-domain-specific-kg-textbook-chapter-2-information-extraction-iv/)
-   [Day 303: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 2 - Information Extraction V - Ryan Ong](https://ryanong.co.uk/2020/10/29/day-303-learn-nlp-with-me-domain-specific-kg-textbook-chapter-2-information-extraction-v/)
-   [Day 304: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 3 - Entity Resolution I - Ryan Ong](https://ryanong.co.uk/2020/10/30/day-304-learn-nlp-with-me-domain-specific-kg-textbook-chapter-3-entity-resolution-i/)
-   [Day 305: Ryans PhD Journey  Why Graph Databases {Neo4j} - Ryan Ong](https://ryanong.co.uk/2020/10/31/day-305-ryans-phd-journey-why-graph-databases-neo4j/)
-   [Day 306: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 3 - Entity Resolution II - Ryan Ong](https://ryanong.co.uk/2020/11/01/day-306-learn-nlp-with-me-domain-specific-kg-textbook-chapter-3-entity-resolution-ii/)
-   [Day 307: Ryans PhD Journey  Neo4j's Python Driver - How to connecting Python with Neo4j - Ryan Ong](https://ryanong.co.uk/2020/11/02/day-307-ryans-phd-journey-neo4j-python-driver/)
-   [Day 308: Learn NLP With Me - Domain-Specific KG Textbook - Chapter 3 - Entity Resolution III - Ryan Ong](https://ryanong.co.uk/2020/11/03/day-308-learn-nlp-with-me-domain-specific-kg-textbook-chapter-3-entity-resolution-iii/)
-   [Day 309: Ryans PhD Journey  From Documents to Graph - Ryan Ong](https://ryanong.co.uk/2020/11/04/day-309-ryans-phd-journey-from-documents-to-graph/)
-   [Day 310: NLP Discovery - DiffBot's Knowledge Graph API - Ryan Ong](https://ryanong.co.uk/2020/11/05/day-310-nlp-discovery-diffbots-knowledge-graph-api/)
-   [Day 311: Ryans PhD Journey  Overview of Knowledge Graphs - Ryan Ong](https://ryanong.co.uk/2020/11/06/day-311-ryans-phd-journey-overview-of-knowledge-graphs/)
-   [Day 312: Ryans PhD Journey  Knowledge Representation Learning I - Ryan Ong](https://ryanong.co.uk/2020/11/07/day-312-ryans-phd-journey-knowledge-representation-learning-i/)
-   [Day 313: Ryans PhD Journey  Knowledge Representation Learning II - Ryan Ong](https://ryanong.co.uk/2020/11/08/day-313-ryans-phd-journey-knowledge-representation-learning-ii/)
-   [Day 314: Ryans PhD Journey  Knowledge Representation Learning III - Ryan Ong](https://ryanong.co.uk/2020/11/09/day-314-ryans-phd-journey-knowledge-representation-learning-iii/)
-   [Day 315: Ryans PhD Journey  Knowledge Acquisition I - Ryan Ong](https://ryanong.co.uk/2020/11/10/day-315-ryans-phd-journey-knowledge-acquisition/)
-   [Day 316: Ryans PhD Journey  Knowledge Acquisition II - Ryan Ong](https://ryanong.co.uk/2020/11/11/day-316-ryans-phd-journey-knowledge-acquisition-ii/)
-   [Day 317: Ryans PhD Journey  Temporal Knowledge Graph & Knowledge-Aware Applications - Ryan Ong](https://ryanong.co.uk/2020/11/12/day-317-ryans-phd-journey-temporal-knowledge-graph-knowledge-aware-applications/)
-   [Day 318: Ryans PhD Journey  Future Directions in KGs - Ryan Ong](https://ryanong.co.uk/2020/11/13/day-318-ryans-phd-journey-future-directions-in-kgs/)
-   [Day 319: Ryans PhD Journey  Overview of Graph Neural Networks - Ryan Ong](https://ryanong.co.uk/2020/11/14/day-319-ryans-phd-journey-overview-of-graph-neural-networks/)
-   [Day 320: Ryans PhD Journey  Introduction to GNNs - Ryan Ong](https://ryanong.co.uk/2020/11/15/day-320-ryans-phd-journey-introduction-to-gnns/)
-   [Day 321: Ryans PhD Journey  Variants of GNNs - Graph Types - Ryan Ong](https://ryanong.co.uk/2020/11/16/day-321-ryans-phd-journey-variants-of-gnns/)
-   [Day 322: Ryans PhD Journey  Variants of GNNs - Propagation Step - Ryan Ong](https://ryanong.co.uk/2020/11/17/day-322-ryans-phd-journey-variants-of-gnns-propagation-step/)
-   [Day 323: Ryans PhD Journey  Variants of GNNs - Training Methods and General Frameworks - Ryan Ong](https://ryanong.co.uk/2020/11/18/day-323-ryans-phd-journey-variants-of-gnns-training-methods-and-general-frameworks/)
-   [Day 324: Ryans PhD Journey  Applications and Future Work of GNNs - Ryan Ong](https://ryanong.co.uk/2020/11/19/day-324-ryans-phd-journey-applications-and-future-work-of-gnns/)
-   [Day 325: Ryans PhD Journey  Nodes 2020 Notes I - Ryan Ong](https://ryanong.co.uk/2020/11/20/day-325-ryans-phd-journey-nodes-2020-notes-i/)
-   [Day 326: Ryans PhD Journey  Nodes 2020 Notes II - Ryan Ong](https://ryanong.co.uk/2020/11/21/day-326-ryans-phd-journey-nodes-2020-notes-ii/)
-   [Day 327: Ryans PhD Journey  Link Prediction - Introduction - Ryan Ong](https://ryanong.co.uk/2020/11/22/day-327-ryans-phd-journey-link-prediction-introduction/)
-   [Day 328: Ryans PhD Journey  Link Prediction - General architecture and Negative Sampling - Ryan Ong](https://ryanong.co.uk/2020/11/23/day-328-ryans-phd-journey-link-prediction-general-architecture-and-negative-sampling/)
-   [Day 329: Ryans PhD Journey  Link Prediction - Traditional Pipeline - Ryan Ong](https://ryanong.co.uk/2020/11/24/day-329-ryans-phd-journey-link-prediction-traditional-pipeline/)
-   [Day 330: Ryans PhD Journey  Refinitiv Knowledge Graph Info - Ryan Ong](https://ryanong.co.uk/2020/11/25/day-330-ryans-phd-journey-refinitiv-knowledge-graph-info/)
-   [Day 331: Ryans PhD Journey  Literature Review - List of Knowledge Graph Representation Papers - Ryan Ong](https://ryanong.co.uk/2020/11/26/day-331-ryans-phd-journey-literature-review-list-of-knowledge-graph-representation-papers/)
-   [Day 332: Ryans PhD Journey  Literature Review - List of Deep Learning & Knowledge Graphs Papers - Ryan Ong](https://ryanong.co.uk/2020/11/27/day-332-ryans-phd-journey-literature-review-list-of-deep-learning-knowledge-graphs-papers/)
-   [Day 333: Ryans PhD Journey  Literature Review - List of Scoring Functions Papers - Ryan Ong](https://ryanong.co.uk/2020/11/28/day-333-ryans-phd-journey-literature-review-list-of-scoring-functions-papers/)
-   [Day 334: Ryans PhD Journey  Literature Review - List of Encoding Models & Auxiliary Information Papers - Ryan Ong](https://ryanong.co.uk/2020/11/29/day-334-ryans-phd-journey-literature-review-list-of-encoding-models-auxiliary-information-papers/)
-   [Day 335: Ryans PhD Journey  Literature Review - List of Knowledge Graph Completion Papers - Ryan Ong](https://ryanong.co.uk/2020/11/30/day-335-ryans-phd-journey-literature-review-list-of-knowledge-graph-completion-papers/)
-   [Day 336: Ryans PhD Journey  Literature Review - List of Entity Discovery Papers - Ryan Ong](https://ryanong.co.uk/2020/12/01/day-336-ryans-phd-journey-literature-review-list-of-entity-discovery-papers/)
-   [Day 337: Ryans PhD Journey  Literature Review - List of Relation Extraction Papers - Ryan Ong](https://ryanong.co.uk/2020/12/02/day-337-ryans-phd-journey-literature-review-list-of-relation-extraction-papers/)
-   [Day 338: Ryans PhD Journey  Literature Review - List of Temporal Knowledge Graph Papers - Ryan Ong](https://ryanong.co.uk/2020/12/03/day-338-ryans-phd-journey-literature-review-list-of-temporal-knowledge-graph-papers/)
-   [Day 339: Ryans PhD Journey  Literature Review - List of Knowledge-aware Applications Papers - Ryan Ong](https://ryanong.co.uk/2020/12/04/day-339-ryans-phd-journey-literature-review-list-of-papers/)
-   [Day 340: Ryans PhD Journey  Literature Review - List of Future Work Related Papers - Ryan Ong](https://ryanong.co.uk/2020/12/05/day-340-ryans-phd-journey-literature-review-list-of-future-work-related-papers/)
-   [Day 341: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes I - Ryan Ong](https://ryanong.co.uk/2020/12/06/day-341-ryans-phd-journey-literature-review-1st-passes/)
-   [Day 342: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes II - Ryan Ong](https://ryanong.co.uk/2020/12/07/day-342-ryans-phd-journey-literature-review-1st-passes-ii/)
-   [Day 343: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes III - Ryan Ong](https://ryanong.co.uk/2020/12/08/day-343-ryans-phd-journey-literature-review-1st-passes-iii/)
-   [Day 344: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes IV - Ryan Ong](https://ryanong.co.uk/2020/12/09/day-344-ryans-phd-journey-literature-review-1st-passes-iv/)
-   [Day 345: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes V - Ryan Ong](https://ryanong.co.uk/2020/12/10/day-345-ryans-phd-journey-literature-review-1st-passes-v/)
-   [Day 346: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes VI - Ryan Ong](https://ryanong.co.uk/2020/12/11/day-346-ryans-phd-journey-literature-review-1st-passes-vi/)
-   [Day 347: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes VII - Ryan Ong](https://ryanong.co.uk/2020/12/12/day-347-ryans-phd-journey-literature-review-knowledge-representation-1st-passes-vii/)
-   [Day 348: Ryans PhD Journey  Literature Review - Knowledge Representation - 1st Passes VIII - Ryan Ong](https://ryanong.co.uk/2020/12/13/day-348-ryans-phd-journey-literature-review-knowledge-representation-1st-passes-viii/)
-   [Day 349: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes I - Ryan Ong](https://ryanong.co.uk/2020/12/14/day-349-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-i/)
-   [Day 350: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes II - Ryan Ong](https://ryanong.co.uk/2020/12/15/day-350-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-ii/)
-   [Day 351: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes III - Ryan Ong](https://ryanong.co.uk/2020/12/16/day-351-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-iii/)
-   [Day 352: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes IIII - Ryan Ong](https://ryanong.co.uk/2020/12/17/day-352-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-iiii/)
-   [Day 353: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes IV - Ryan Ong](https://ryanong.co.uk/2020/12/18/day-353-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-iv/)
-   [Day 354: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes V - Ryan Ong](https://ryanong.co.uk/2020/12/19/day-354-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-v/)
-   [Day 355: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes VI - Ryan Ong](https://ryanong.co.uk/2020/12/20/day-355-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-vi/)
-   [Day 356: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes VII - Ryan Ong](https://ryanong.co.uk/2020/12/21/day-356-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-vii/)
-   [Day 357: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes VIII - Ryan Ong](https://ryanong.co.uk/2020/12/22/day-357-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-viii/)
-   [Day 358: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes IX - Ryan Ong](https://ryanong.co.uk/2020/12/23/day-358-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-ix/)
-   [Day 359: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes X - Ryan Ong](https://ryanong.co.uk/2020/12/24/day-359-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-x/)
-   [Day 360: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes XI - Ryan Ong](https://ryanong.co.uk/2020/12/25/day-360-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-xi/)
-   [Day 361: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes XII - Ryan Ong](https://ryanong.co.uk/2020/12/26/day-361-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-xii/)
-   [Day 362: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes XIII - Ryan Ong](https://ryanong.co.uk/2020/12/27/day-362-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-xiii/)
-   [Day 363: Ryans PhD Journey  Literature Review - Knowledge Acquisition - 1st Passes XIV - Ryan Ong](https://ryanong.co.uk/2020/12/28/day-363-ryans-phd-journey-literature-review-knowledge-acquisition-1st-passes-xiv/)
-   [Day 364: Ryans PhD Journey  OpenKE-PyTorch Library Analysis + code snippets for 11 KE models - Ryan Ong](https://ryanong.co.uk/2020/12/29/day-364-ryans-phd-journey-openke-pytorch-library-analysis-code-snippets-for-11-ke-models/)
-   [Day 365: NLP Papers Summary  A Survey on Knowledge Graph Embedding - Ryan Ong](https://ryanong.co.uk/2020/12/30/day-365-nlp-papers-summary-a-survey-on-knowledge-graph-embedding/)