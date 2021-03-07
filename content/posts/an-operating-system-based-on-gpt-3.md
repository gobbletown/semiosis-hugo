+++
title = "An operating system based on GPT-3"
author = ["Shane Mulligan"]
date = 2021-03-08T00:00:00+13:00
keywords = ["GPT-3"]
draft = false
+++

Reference
: <http://github.com/semiosis/examplary>


## Summary {#summary}

I assume that `GPT-3` or some descendant of it
will become the primary interface to the
computer.

I create a configuration option to enable /
disable `GPT-3`.

When disabled, the environment will resort to
alternative means of performing tasks.


## Configuration {#configuration}

{{< highlight sh "linenos=table, linenostart=1" >}}
vim +/"use_gpt3: on" "$NOTES/myrc.yaml"
{{< /highlight >}}

{{< figure src="./gpt3-config.png" >}}

{{< highlight sh "linenos=table, linenostart=1" >}}
vim +/"summarize) {" "$SCRIPTS/s"
{{< /highlight >}}


### If `gpt3` is enabled, filter through OpenAI API abstractive summarizer {#if-gpt3-is-enabled-filter-through-openai-api-abstractive-summarizer}

Otherwise, use `sumy`.

{{< highlight bash "linenos=table, linenostart=1" >}}
summarize) {
    if myrc-test use_gpt3; then
        openai-complete $MYGIT/semiosis/prompts/prompts/summarize-for-2nd-grader.prompt
    else
        zsh -c "sumy lex-rank --length=10 --file=<(cat)"
    fi
}
;;
{{< /highlight >}}


## A Zettelkasten of Natural Language Processing tasks {#a-zettelkasten-of-natural-language-processing-tasks}

{{< figure src="./org-brain.png" >}}

I use `org-brain` because it enables me to
recall exactly where I am substituting `GPT-3`
within my scripts.

<span class="underline">**`org-brain` demo**</span>

<a title="asciinema recording" href="https://asciinema.org/a/nrqqHWCfc5eG0lj3LPyzO2T2h" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/nrqqHWCfc5eG0lj3LPyzO2T2h.svg" /></a>