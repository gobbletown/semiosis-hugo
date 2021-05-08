+++
title = "OpenAI API for NLP"
author = ["Shane Mulligan"]
date = 2020-06-12T00:00:00+12:00
keywords = ["gpt", "nlp", "marketing", "openai", "nlg", "nlu", "productivity", "semanticsearch"]
draft = false
+++

OpenAI API
: <https://beta.openai.com/>


## Summary {#summary}

I'd like to add some OpenAI support to emacs.
GPT-3 is easy to integrate because it is a very general-purpose transformer.
In a later article, I'd like to integrate more specialised huggingface transformers.


## Results {#results}

Here are some of the things I had managed to do with emacs and GPT-3 so far.

-   [Imaginary programming with GPT-3 // Bodacious Blog](https://mullikine.github.io/posts/imaginary-programming-with-gpt-3/)
    -   Here I assemble an imaginary programming environment in GPT-3.
-   [Translating with GPT-3 and Emacs // Bodacious Blog](https://mullikine.github.io/posts/translating-with-gpt-3-and-emacs/)
    -   I demonstrate using GPT-3 programmatically from emacs to do translation.
-   [GPT-3 mind maps with an AI tutor for any topic // Bodacious Blog](https://mullikine.github.io/posts/gpt-3-for-building-mind-maps-with-an-ai-tutor-for-any-topic/)
    -   I integrate `GPT-3` into `org-brain` to auto-scaffold a mind map and provide a tutor for anything.
-   [Generating pickup lines with GPT-3 // Bodacious Blog](https://mullikine.github.io/posts/generating-pickup-lines-with-gpt-3/)
    -   I integrate `GPT-3` into `cousel` for emacs.
    -   This allows me to fuzzy search with an ever growing candidates list.
-   [Autocompleting anything with GPT-3 in emacs // Bodacious Blog](https://mullikine.github.io/posts/autocompleting-anything-with-gpt-3-in-emacs/)
    -   I integrate `GPT-3` into `company` for emacs. This allows me to autocomplete in emacs.
    -   I also can make arbitrarily many completion engines based on `GPT-3`.
-   [Context menus based on GPT-3 // Bodacious Blog](https://mullikine.github.io/posts/context-menus-based-on-gpt-3/)
    -   I integrate `GPT-3` into `right-click-context-menu` for emacs.
    -   I can use `GPT-3` to classify the editing context, allowing me to suggest relevant functions through a contextual menu.
-   [An operating system based on GPT-3 // Bodacious Blog](https://mullikine.github.io/posts/an-operating-system-based-on-gpt-3/)
    -   I try to demonstrate that an OS may be based on `GPT-3`.
-   [crontab.guru in emacs and making a prompt with GPT-3 to copy it // Bodacious Blog](https://mullikine.github.io/posts/crontab-guru-in-emacs/)
    -   I demonstrate that `GPT-3` can be used with emacs to replace such tools as `crontab.guru`.
-   [Creating a playground for GPT-3 in emacs // Bodacious Blog](https://mullikine.github.io/posts/creating-a-playground-for-gpt-3-in-emacs/)
    -   I build a `GPT-3` prompt-editing environment for emacs.


## Capabilities of the OpenAI API {#capabilities-of-the-openai-api}

-   Apply the API to any language task
    -   `semantic search`,
    -   `summarization`,
    -   `sentiment analysis`,
    -   `content generation`,
    -   `translation`,
    -   and more...

Use only a few examples or by specifying your
task in English.

The following is the initial investigation I did into the API.


## Using the API for Semantic search {#using-the-api-for-semantic-search}

{{< highlight text "linenos=table, linenostart=1" >}}
Semantic Search
    [openai-api]

    Allows searching over documents based on
    the natural-language meaning of queries
    rather than keyword matching.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
casetext
    [#semantic search]
    [#openai-api]

    Automates litigation tasks to help
    attorneys efficiently provide high-quality
    representation, offering a comprehensive
    legal research platform used by over 5,500
    law firms.

    The platform includes CARA A.I.,
    technology that automates critical legal
    research tasks, and Compose, a first-of-
    its-kind technology that automates
    substantive elements of litigation.

    With OpenAI’s technology, Casetext seeks
    to enhance their semantic search
    capabilities, which has the potential to
    save lawyers hundreds of hours in
    research.

Algolia
    [#semantic search]
    [#openai-api]

    Offers highly relevant, fast search to
    everyone with a website, mobile app, or
    voice app.

    Combining OpenAI’s API with Algolia’s
    advanced search technology allows
    Algolia’s customers to provide their
    customers with NL semantic search, so they
    can better understand questions and
    connect searchers with results that are
    both relevant and fast.

    OpenAI helps Algolia answer more complex
    queries than ever before, trimming down
    the prediction time to around 100ms.

    This keeps Algolia from having to do a lot
    of work to cache and serve answers to its
    customers.

    With OpenAI, Algolia was able to answer
    complex NL questions accurately 4x as
    often as it was using BERT.

Search Plugin
    [#semantic search]
    [#chrome]
    [#openai-api]

    The API identifies relevant content for NL
    queries without using keywords.

    Here the API has been integrated into a
    browser plugin that lets users find
    answers on web page by typing in a
    question.
{{< /highlight >}}


## Using the API for Chat {#using-the-api-for-chat}

{{< highlight text "linenos=table, linenostart=1" >}}
Chat
    [openai-api]

    Enables fast, complex and consistent NL
    discussions.

    With a brief prompt, the API generates
    dialogues spanning a range of topics, from
    space travel to history.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
AI Channels
    [#openai-api]
    [social network]

    For people and artificial intelligence
    agents.

    AI Channels lets you interact with AI
    agents that can help you generate ideas,
    recommend books and movies, tell
    interactive stories or participate in a
    round table discussion with your friends
    and the greatest minds in history where
    you can ask a virtual Albert Einstein to
    explain relativity or get writing tips
    from Jane Austen.
{{< /highlight >}}


## Using the API for Customer Service {#using-the-api-for-customer-service}

{{< highlight text "linenos=table, linenostart=1" >}}
Customer Service
    [openai-api]

    Leveraging search and chat capabilities,
    the API generates natural dialogue to
    quickly give customers relevant
    information.

    Through semantic text comprehension, the
    API can offer a range of analytics and
    productivity tools to better serve
    customers.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
MessageBird
    [#openai-api]
    [#customer service]
    [#nlp]
    [#marketing]

    Provides an omnichannel platform-as-a-
    service to better support customers
    through its Inbox product -- which
    integrates with the top communications
    platforms.

    With OpenAI’s technology, MessageBird is
    developing automated grammar and spelling
    tools as well as predictive text to
    augment Inbox’s already powerful AI
    capabilities.

Sapling Intelligence
    [#openai-api]
    [#customer service]
    [#nlp]
    [#marketing]

    An AI writing assistant for customer-
    facing teams.

    Sapling sits on top of CRMs and messaging
    platforms to help agents more efficiently
    compose personalized responses.

    Managers gain conversational insights to
    coach and prepare teams.

    Sapling was developed by former ML
    researchers at Berkeley, Stanford, and
    Google, and assists customer-facing teams
    serving startups as well as Fortune 500
    clients.

    Sapling works on most B2B helpdesk and
    sales engagement chat platforms through a
    browser integration.

    Using the OpenAI API's semantic search
    feature, we developed a KB search feature
    that assists agents by suggesting chat
    responses, improving the customer
    experience for sales and support teams.
{{< /highlight >}}


## Using the API for Generation {#using-the-api-for-generation}

{{< highlight text "linenos=table, linenostart=1" >}}
Generation
    [openai-api]

    The API can generate complex and
    consistent NL, and enables use cases like
    creative writing.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
AI Dungeon
    [#openai-api]
    [#nlg]
    [#nlp]

    An AI-powered text adventure where every
    response is determined by an AI LM.

    Typically, for these types of games, the
    developer must preprogram a decision tree
    and text options for the user to select
    from.

    AI Dungeon is the first of its kind in
    which any story option is possible, and
    the AI adapts the adventure to the users’
    input.

    The game sees 20-25,000 daily users.

    Initiallky built on GPT-2, after moving to
    OpenAI’s new technology accessible through
    the API, AI Dungeon has seen a significant
    increase in user engagement and
    subscriptions.

    Users have reported positively on the
    speed and quality of conversations, and
    subscriptions for the game have increased
    nearly 25%.

    AI Dungeon hopes to expand AI’s use in
    gaming to make for richer experiences
    during gameplay (particularly with non-
    playable characters (NPCs)).

AI Weirdness
    [#openai-api]
    [#nlg]
    [#nlp]

    A popular science blog by Janelle Shane,
    author of
        You Look Like a Thing and I Love You:
            How Artificial Intelligence Works
            and Why it's Making the World a
            Weirder Place.

    She writes about the successes and
    sometimes amusing failures of various ML
    algorithms.

    Using the OpenAI API, she iterated with
    creative ideas for her blog posts and
    tweets.

Replika
    [#openai-api]
    [#nlg]
    [#nlp]

    An AI companion, uses our API to AB test
    extensively and has seen their customers’
    happiness ratings improve by 20 or more
    percentage points.

    They saw ratings hover around 60% with
    their original, in-house tech — this
    improved by 7-8% with GPT-2 — and is now
    in the 80-90% range with the API.
{{< /highlight >}}


## Using the API for Productivity tools {#using-the-api-for-productivity-tools}

{{< highlight text "linenos=table, linenostart=1" >}}
Productivity Tools
    [openai-api]

    The API allows for parsing text into
    spreadsheet tables, summarizing email
    discussions, expanding content from bullet
    points, and more.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
Quizlet
    [#openai-api]
    [#productivity]
    [#nlp]

    Quizlet is a global learning platform that
    provides engaging study tools to help
    people practice and master whatever they
    are learning.

    Every month, over 50 million people across
    130 countries use Quizlet to study any
    subject imaginable for school, work or as
    part of their personal interests.

    Combining cognitive science and ML,
    Quizlet guides students through adaptive
    study activities to confidently reach
    their learning goals, with over a billion
    questions answered each week.

    A popular use of Quizlet is to learn
    vocabulary faster.

    To enable a deeper understanding than rote
    memorization, Quizlet is building upon
    OpenAI’s powerful text generation
    capabilities to automatically generate
    examples of how each vocabulary word can
    be used in a sentence.

    By combining OpenAI’s technology with the
    in-depth work and research Quizlet is
    doing with ML, Quizlet will be able to
    develop example sentences for people
    studying vocabulary and languages, the way
    a tutor does, to help students integrate
    their knowledge in a fun way and test
    themselves more comprehensively.

    Using the latest in NLP technologies
    allows Quizlet to build toward the future
    of an AI-powered tutor in your pocket.

Art of Problem Solving
AoPS
    [#openai-api]
    [#education]
    [#nlp]

    Helping to effectively prepare the next
    generation of STEM professionals through
    engaging online instruction, at a time
    when the traditional nature of in-person
    education is being challenged.

    AoPS is breaking enrollment records across
    all their programs due to an onslaught of
    parents searching for at-home learning
    options for the first time.

    By delivering faster and more accurate
    student feedback, AoPS can help students
    improve their mathematical problem-solving
    skills and their delivery in explaining
    their answers.

    Among the students AoPS have trained are
    nearly all the members of the US
    International Math Olympiad team for the
    past 10 years and a number of the
    researchers / developers at OpenAI.

    By leveraging OpenAI’s technology, AoPS is
    empowering its human teachers as they
    evaluate students’ work and provide
    feedback on the accuracy, language and
    mastery of their solutions.

    AoPS continues to train OpenAI’s API on
    existing feedback from expert teachers,
    and are using it to quickly generate a
    first draft of feedback on a student’s
    work for the grader to refine and then
    send.

    The teacher’s final version is also shared
    back with the API to help improve it even
    further, and the teachers themselves are
    charged with evaluating the tool and
    deciding how widely it gets used.

    AoPS is currently testing the technology
    and seeing promising initial results.

    In the near term, AoPS believes that with
    OpenAI’s technology, students will be able
    to receive same-day feedback on their work
    while improving quality of feedback.

Natural Language Shell
    [#openai-api]
    [#productivity]
    [#nlp]
    [#semanticsearch]
    [#codegen]

    Here we show how the API can be used to
    translate natural language to unix
    commands, using a handful of
    representative examples.

Spreadsheets
    [#openai-api]
    [#data]
    [#nlp]

    In this example, we show how the API
    generates a table with suggested
    categories to make it easier to extract
    and organize information.

Code Completion
    [#openai-api]
    [#productivity]
    [#nlp]
    [#semanticsearch]
    [#codegen]

    With the API we can generate useful and
    context-aware code suggestions. After
    fine-tuning with code from thousands of
    Open Source GitHub repositories, the API
    completes code based on function names and
    comments.
{{< /highlight >}}


## Using the API for Content Comprehension {#using-the-api-for-content-comprehension}

The API can be used to build tools to help individuals consume content more efficiently.

{{< highlight text "linenos=table, linenostart=1" >}}
Content Comprehension
    [openai-api]

    The API can be used to build tools to help
    individuals consume content more
    efficiently.
{{< /highlight >}}


### Examples {#examples}

{{< highlight text "linenos=table, linenostart=1" >}}
Koko
    [#openai-api]
    [#nlp]
    [#nlu]

    An online mental health intervention that
    has reached nearly two million people,
    mostly adolescents.

    The platform started as a clinical trial
    at MIT and is based on the concept of
    crowdsourced cognitive therapy.

    Unlike traditional peer support platforms,
    all interactions on the service are
    supported and augmented by AI.

    Koko is using OpenAI’s technology to
    enhance its AI capabilities and improve
    its ability to keep users safe.

    Using the API, Koko can automatically
    identify users in acute states of crisis
    and route them to specialized services
    (such as the National Suicide Prevention
    Lifeline).

    This builds on Koko’s existing work in
    this area, in collaboration with Harvard
    University, and allows them to scale the
    service more broadly.

    This partnership is especially important
    now, given the staggering rise in reported
    mental health symptoms following the onset
    of Covid-19.

    With OpenAI, Koko’s text-based classifiers
    improved substantially, without
    preprocessing.

    The F1 score of its crisis classifier went
    up from .76 to .86, and the accuracy went
    up to 96%.

    In the future, this capability could help
    peer supporters work faster and more
    efficiently, in addition to many other
    therapeutic use cases.

Ross Intelligence
    [#openai-api]
    [#nlp]
    [#nlu]

    Founded in 2015, ROSS Intelligence
    ("ROSS") is the industry-leading AI-driven
    legal research provider.

    ROSS's easy-to-use legal research platform
    leverages proprietary AI technology to
    help lawyers conduct more thorough
    research in a fraction of the time.

    ROSS is funded by tier-one investors,
    including Comcast Ventures and
    Y-Combinator, and was recognized by the
    American Bar Association as "an example of
    how artificial intelligence can be used to
    improve the delivery of legal services."

    ROSS is using the API to better search
    through legal authority and synthesize law
    so that legal professionals can provide
    sound and timely advice to their clients.

Summarization
    [#openai-api]
    [#nlp]
    [#nlu]

    Through its pattern recognition and
    generative capabilities, the API can
    transform dense text into simplified
    summaries.

    Here, we show the API summarizing an NDA
    into content that's accessible at a 2nd-
    grade reading level.
{{< /highlight >}}


## Using the API for language translation {#using-the-api-for-language-translation}

{{< highlight text "linenos=table, linenostart=1" >}}
Polyglot
    [openai-api]

    While the API today works best in English,
    it also works quite well in other
    languages.

    The API can be used for tasks such as
    translation or chat with users in their
    preferred language.
{{< /highlight >}}


## Glossary {#glossary}

{{< highlight text "linenos=table, linenostart=1" >}}
Neural machine translation
NMT
    One approach to machine translation.

    The use of NN models to
    - learn a statistical model for machine
      translation.

      i.e. predict the likelihood of a
      sequence of words, typically modeling
      entire sentences in a single integrated
      model.

    Key benefit:
        A single system can be trained
        directly on source and target text, no
        longer requiring the pipeline of
        specialized systems used in
        statistical machine learning.

    Unlike the traditional phrase-based
    translation system which consists of many
    small sub-components that are tuned
    separately, neural machine translation
    attempts to build and train a single,
    large neural network that reads a sentence
    and outputs a correct translation.

    Widely used to translate natural langugae
    text.

NMT with code2vec
    Learn from the previous code changes and
    suggest the future edits.

    For modeling code changes, NMT seem to be
    a natural fit as they can learn the
    translation (i.e. edits) from an original
    to the changed version of the code.

    Essentially, these models learn the
    probability distribution of changes and
    assign higher probabilities to plausible
    code edits and lower probabilities to less
    plausible ones.

    In fact, Tufano et al. shows the
    initial promise of using a
    sequence-to-sequence translation model
    (seq2seq) for fixing bugs in their new
    idea paper.

    In this work, we design an
    encoder-decoder-based machin.

Cross-lingual Language Model
XLM

XLM pretraining
    Allows the seq2seq model to generate
    high quality representations of input
    sequences.
{{< /highlight >}}