+++
title = "Define made-up words with Davinci"
author = ["Shane Mulligan"]
date = 2021-10-13T00:00:00+13:00
keywords = ["gpt", "openai", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

I make a prompt to define made-up words.

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/define-a-given-word-1.prompt>

`pf-define-a-given-word/1`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Define a given word"
doc: "Given a word, even a fake one, come up with a definition"
prompt-version: 2
prompt: |-
    imprudent (imprudent)
    <delim>
    Not showing care for the consequences of
    an action; rash.
    "it would be imprudent to leave her winter
    coat behind"
    <delim>
    vain (vain)
    <delim>
    Having or showing an excessively high
    opinion of one's appearance, abilities, or
    worth.
    "their flattery made him vain"
    <delim>
    <word> (<word>)
    <delim>
force-engine: "OpenAI Davinci"
force-temperature: 0.8
max-generated-tokens: "prompt-length"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
defs:
# Annoyingly, PEN_STOP_SEQUENCE as --- can cause problems
# Because it may be recognised as a CLI argument.
# - delim2: "\"---\""
vars:
- "word"
validator: grep -q "<word>"
examples:
- "surrealeptitious"
info: on
completion: off
insertion: off
postprocessor: pen-str remove-starting-and-trailing-whitespace
{{< /highlight >}}


## Demo - `surrealeptitious` {#demo-surrealeptitious}


### Correct definitions {#correct-definitions}

I like this one:

{{< highlight text "linenos=table, linenostart=1" >}}
marked by the secretive nature of
surrealism.
  (surrealeptitious)
"the surrealeptitious art of collage"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
marked by an apparently incongruous
combination of circumstances or events
that seem to be deliberately contrary to
what one expects; bizarre; uncanny.
"it was a dark and stormy night that
might have served as the setting for
surrealeptitious meeting of the two old
friends"
{{< /highlight >}}


### Too much surreptitious; Not enough surreal {#too-much-surreptitious-not-enough-surreal}

{{< highlight text "linenos=table, linenostart=1" >}}
Lacking subtlety, especially in a way that is
embarrassing.
"The movie was full of surrealeptitious product
placement"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
marked by the intentional use of strategies
intended to mislead or confuse, as for the
purpose of political or military deception.
"his death was due to surrealeptitious poisoning"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
secret and crafty
"a conspiracy of surrealeptitious Watergate
burglars"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
An act usually in violation of the law,
the truth, or morality, done in secret or
with the permission or cooperation of
an authority.
"Rayburn's surrealeptitious intervention
tipped the balance in favor of the
railroads."
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
an underhanded and usually devious
method; sly and secret ; (devious)
"he resorted to surrealeptitious tactics"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
highly secret and mysterious
"a surrealeptitious glance at his watch"
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
So subtle as to escape notice.
"the surrealeptitious sound of their
footsteps"
{{< /highlight >}}