+++
title = "A natural language database using a single GPT prompt"
author = ["Shane Mulligan"]
date = 2021-03-07T00:00:00+13:00
keywords = ["GPT-3", "NLP", "openai", "NLP"]
draft = false
+++

Original article
: <https://www.gwern.net/GPT-3#the-database-prompt>


## Summary {#summary}

A single prompt describes transactions to and from a database.

GPT-3 is able to answer questions about the
transactions that have taken place.

GPT-3 isn't actually a database.

The LM simply understands language so well
that describing the transactions that have
taken place would naturally lead to the GPT-3
response.


## The prompt {#the-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "database example"
doc: "GPT-3 as a NL interface for semantically querying logic in prose"
prompt: |+
    The database begins knowing nothing.
    The database knows everything that is added to it.
    The database does not know anything else.
    When asked a question, if the answer has been added to the database the database says the answer.
    When asked a question, if the answer has not been added the database says it does not know.

    Q: Does the database know “What is 2+2?”
    A: The database does not know.

    Q: Does the database know “What is the capital of France?”
    A: The database does not know.

    ""Tom is 20 years old"" is added to the database.
    Nothing else about Tom is added to the database.

    Q: Does the database know where Tom lives?
    A: The database does not know.

    Q: How does the database respond when Tom’s age?
    A: The database says “Tom is 20 years old.”

    Q: How does the database response when asked “What’s my age?”
    A: The database says “You are not in the database.”

    ""Shane is a cool guy"" is added to the database.
    ""Shane is 33 years old"" is added to the database.

    Q: <1>
    A:
engine: "davinci"
temperature: 0.3
max-tokens: 60
top-p: 1.0
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "\n\n"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "query or input"
examples:
- "How old is Shane?"
external: ""
conversation-mode: no
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
{{< /highlight >}}


## Demonstration {#demonstration}

As you can see, the prompt only functions
approximately as a database.

It could certainly be made more reliable
either through adjusting parameters or by
providing better counter-examples.

<a title="asciinema recording" href="https://asciinema.org/a/VDkB11XyAMv8kvieuDuONwMUp" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/VDkB11XyAMv8kvieuDuONwMUp.svg" /></a>


## Utility {#utility}

This ability of GPT-3 could be used for the
NLP task of information extraction.