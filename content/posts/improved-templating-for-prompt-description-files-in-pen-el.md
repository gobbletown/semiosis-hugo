+++
title = "Templating for prompt description files in Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-03T00:00:00+12:00
keywords = ["gpt", "pen", "emacs"]
draft = false
+++

## Summary {#summary}

I improve the templating system in `.prompt` files.


## `include` key {#include-key}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: ada
include: Generic completion 50 tokens
prompt-version: 1
lm-command: "openai-complete.sh"
engine: ada
{{< /highlight >}}

The above `.prompt` file will inherit the
below keys, but may override some values.

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: Generic completion 50 tokens
prompt-version: 3
doc: This is a generic completer.
prompt: "<1>"
engine: davinci
temperature: 0.8
max-tokens: 50
top-p: 1
stop-sequences:
- "###"
vars:
- text
examples:
- Recycling is good for the world, no, you could not be more wrong
completion: true
var-defaults:
- "(pen-preceding-text)"
{{< /highlight >}}


## `subprompts` and named template variables {#subprompts-and-named-template-variables}

Below you can see that the prompt has been
subdivided into subsections.

This is partly to distinguish the `metaprompt`
from the rest of the prompt, but the main
purpose is to then enable other prompts to
inherit from a prompt the subsections without
redefining them.

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: Generic tutor for any topic and subtopic
prompt-version: 6
# I have broken up this prompt so another prompt may import and use
subprompts:
- metaprompt: |
    This is a conversation between a human and a brilliant AI.
    The topic is "<in the context of>" in the context of "<topic>".
- pretext: |
    Human: Hello, are you my <topic> tutor?
    ###
    AI: Hi there.
    Yes I am.
    How can I help you today?
    ###
    Human: What questions can I ask you about <topic>?
    ###
    AI: You may ask me anything relating to "<in the context of>" in the context of "<topic>".
    ###
    Human: OK then
prompt: |
  <metaprompt>

  <pretext>. <question>
  ###
  AI: I would be happy to answer your question.
engine: davinci
temperature: 0.8
max-tokens: 500
top-p: 1
best-of: 3
stop-sequences:
- "###"
- "Human:"
vars:
- topic
- in the context of
- question
examples:
- node js
- programming
- What was the version of node in 2018?
var-defaults:
- "(pen-topic-ask)"
- "(read-string-hist \"Pen subtopic: \")"
- "(pen-selected-text)"
conversation-mode: true
conversation-pretext-generator: human-conversation
aliases:
- asktutor
postprocessor: pen-pretty-paragraph
{{< /highlight >}}

Now the below `.prompt` can be created with much re-use:

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: Explain error
include: Generic tutor for any topic and subtopic
prompt-version: 1
prompt: |
  <metaprompt>

  <pretext>. What does the following error mean? <question>
  ###
  AI: I would be happy to answer your question.
engine: davinci
temperature: 0.8
max-tokens: 500
top-p: 1
best-of: 3
stop-sequences:
- "###"
- "Human:"
vars:
- topic
- in the context of
- question
examples:
- node js
- error messages
- undefined
var-defaults:
- "(pen-topic-ask)"
- "\"error messages\""
- "(pen-selected-text)"
{{< /highlight >}}

The AI tutor can now be primed to take
particular questions. You may simply select
error messages now and ask for an
explanation. That's a very convenient alternative to Stack Exchange!


## Demo {#demo}

{{< highlight text "linenos=table, linenostart=1" >}}
TypeError: 'method' object is not subscriptable
{{< /highlight >}}

It returned a stack exchange answer, but the link is incorrect! Quite amazing though.

{{< highlight text "linenos=table, linenostart=1" >}}
That error means the object referred to by the variable 'method' does not have a subscript that matches the index parameter passed to [subscript].
For example:
def sampleMethod(self, arg):
arg = 12
list1 = list(sampleMethod) # <- here index is 0
##
If the method is a tuple, unpacking the tuple into individual variables might succeed:
tramps = ['Jack', 'Jill']
[str(tramp['name']) for tramp in tramps]
##
See also: http://stackoverflow.com/questions/276510/why-is-typeerror-object-is-not-subscriptable
{{< /highlight >}}


### Actual link {#actual-link}

<https://stackoverflow.com/a/216980>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/lCi9h2i8kbk2caniKgIbJ8Za1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/lCi9h2i8kbk2caniKgIbJ8Za1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/lCi9h2i8kbk2caniKgIbJ8Za1.js" id="asciicast-lCi9h2i8kbk2caniKgIbJ8Za1" async></script>