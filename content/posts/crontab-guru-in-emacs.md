+++
title = "crontab.guru in emacs and making a prompt with GPT-3 to copy it"
author = ["Shane Mulligan"]
date = 2021-03-06T00:00:00+13:00
keywords = ["emacs", "gpt-3", "examplary"]
draft = false
+++

Related
: <https://crontab.guru/>


Author
: <http://github.com/mullikine>


## Summary {#summary}

I build some functionality into emacs to use
`crontab.guru` behind the scenes to interpret
tab lines displaying inside of emacs, without
using the web browser.

I then build a GPT-3 prompt which does exactly
the same thing without `crontab.guru` and
provide the initial script I made to
`examplary` (my GPT-3 DSL) as an example
generator, to enhance the prompt if that is
needed later.


## Initial steps {#initial-steps}

When lines in cron format appear in an emacs
buffer, the `crontab-guru` function is
suggested, allowing you to easily understand
crontabs.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun buffer-cron-lines ()
  (sor (snc "scrape \"((?:[0-9-/]+|\\\\*)\\\\s+){4}(?:[0-9]+|\\\\*)\"" (buffer-string))))

(defun chrontab-guru (tab)
  (interactive (list (fz (buffer-cron-lines))))
  (let ((tab (sed "s/\\s\\+/_/g" tab)))
    (chrome (concat "https://crontab.guru/#" tab))))
{{< /highlight >}}


## Demonstration {#demonstration}

<a title="asciinema recording" href="https://asciinema.org/a/dKU8QGolIthb93F8P29NESdn6" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/dKU8QGolIthb93F8P29NESdn6.svg" /></a>

{{< figure src="/ox-hugo/crontabguru.png" >}}


## Modify to display the explanation inside emacs {#modify-to-display-the-explanation-inside-emacs}

-   Dump the Google Chrome DOM for the website (since it requires javascript)
-   Scrape the explanation from the website
-   Create a new buffer in emacs with the explanation

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun crontab-guru (tab)
  (interactive (list (fz (buffer-cron-lines) (if (selectionp) (my/thing-at-point)))))
  (let ((tab (sed "s/\\s\\+/_/g" tab)))
    ;; (chrome (concat "https://crontab.guru/#" tab))
    (etv (scrape "\"[^\"]*\"" (snc (concat "elinks-dump-chrome " (q (concat "https://crontab.guru/#" tab))))))))
{{< /highlight >}}


## Prompt GPT-3 {#prompt-gpt-3}

This script is a `string->string` filter
script that scrapes the chrome dom of
`crontab.guru` to get its result, given the tab as standard input.

<span class="underline">**interpret-crontab**</span>

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

scrape-crontab | awk1 | while IFS=$'\n' read -r tab; do
    (
        exec 0</dev/null
        if test -n "$tab"; then
            taburlbit="$(p "$tab" | sed "s/\\s\\+/_/g")"
            elinks-dump-chrome "https://crontab.guru/#$taburlbit" | scrape "\"[^\"]*\""
        fi
    )
done | sed -e 's/^"//' -e 's/"$//'
{{< /highlight >}}

The following file is a GPT-3 prompt which
accepts the above `interpret-crontab` script
as an external function to generate and train
and ultimately replace the prompt.

<span class="underline">**crontab-translator.prompt**</span>

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "crontab translator"
prompt: |+
  crontab

  17 *	* * *
  At minute 17.
  ###
  25 6	* * *
  At 06:25.
  ###
  47 6	* * 7
  At 06:47 on Sunday.
  ###
  52 6	1 * *
  At 06:52 on day-of-month 1.
  <1>
engine: "davinci"
temperature: 0.3
max-tokens: 60
top-p: 1.0
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "###"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "tab"
examples:
- "30 7    * * *"
external: "interpret-crontab"
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
{{< /highlight >}}

Example output from GPT-3:

Input tab
: `15 7    * * *`


GPT-3 output
: `On day-of-week 7, at 15:00.`


## Moral of the story {#moral-of-the-story}

The moral of the story is that if a person
builds a website like crontab.guru, its
functionality actually becomes learned by the
next iteration of GPT and then its
functionality is able to be reproduced.