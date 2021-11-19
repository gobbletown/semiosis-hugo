+++
title = "Multi-part prompts in Pen.el for more advanced chatbots!"
author = ["Shane Mulligan"]
date = 2021-11-19T00:00:00+13:00
keywords = ["pen", "openai", "nlp", "prompt-engineering"]
draft = false
+++

## Summary {#summary}

This is better than stackoverflow for many tasks. It's great
because this works for the imaginary web too.
The images in the web site were converted to
text, but can also browse imaginary websites
about anything. I asked John Dee what was his
most rarest book, and he told me about book.
Looked it up and it was real/legit.


## Prompt format addition {#prompt-format-addition}

At places where `<:fz-eol>` is found in a
prompt, the line will be completed and you
will be able to select from some possible
completions there.

{{< highlight yaml "linenos=table, linenostart=1" >}}
Document 3:
###
<document>
###
Topics: <:fz-eol>
###
Document 3 scholars:
<:pp>- 1.
{{< /highlight >}}

This method will be further extended to assign
specific prompt settings, beam search and
counterfactual parsing.


## Demo {#demo}


### Speaking to AI Richard Stallman {#speaking-to-ai-richard-stallman}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/RZj0HRo1n3ote0AMqZMW0I4HZ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/RZj0HRo1n3ote0AMqZMW0I4HZ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/RZj0HRo1n3ote0AMqZMW0I4HZ.js" id="asciicast-RZj0HRo1n3ote0AMqZMW0I4HZ" async></script>


### Speaking to AI Richard Stallman {#speaking-to-ai-richard-stallman}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/aUAjViUGrS42xqk9DHgONiyl8" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/aUAjViUGrS42xqk9DHgONiyl8.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/aUAjViUGrS42xqk9DHgONiyl8.js" id="asciicast-aUAjViUGrS42xqk9DHgONiyl8" async></script>