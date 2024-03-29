+++
title = "Search the web with Codex (bye Google)"
author = ["Shane Mulligan"]
date = 2021-09-09T00:00:00+12:00
keywords = ["codex", "pen", "gpt", "emacs"]
draft = false
+++

## Summary {#summary}

This is a demonstration of using a LM to
search the internet.

This is a very effective search engine,
actually. It pretty much replaces Google
Search. The technology has tighter integration
with tooling, faster look- up times, can be
used offline etc. with (free models).


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/o7X9udIBKyBRGLXbdHkV5divh" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/o7X9udIBKyBRGLXbdHkV5divh.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/o7X9udIBKyBRGLXbdHkV5divh.js" id="asciicast-o7X9udIBKyBRGLXbdHkV5divh" async></script>

Select any text, code or prose or whatever,
irrespective of if they have the keywords you
would typically search for. Then it generates
a bunch of URLs and you filter the results.
Firstly extracting the urls from the
generrated text with a tool such as 'xurls',
then validating those URLs by pinging the
server. Then you have a list of valid urls
that are related to the text you selected.
It's like the world wide web but without the
hyperlinks.


### GPT-3 can do this too {#gpt-3-can-do-this-too}

But GPT-3 can be thought of as being too high
temperature; The urls themselves are chimeric,
and a chimeric url is usually a wrong url.


## Prompt {#prompt}

Source
: <http://github.com/semiosis/prompts/blob/master/prompts/get-urls-for-a-passage-1.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get URLs for a passage"
doc: "Given some text return relevant URLs"
prompt-version: 2
issues:
  - DONE URLs are sometimes invalid or outdated. Add a validator.
  - The corrector could literally just be a google search for that url
  - It's a little slow because of the validator (checks results for 404).
  - Also, add a URL corrector (syntax checker for urls, with a specific LM)
prompt: |+
  """
  <text>
  """
  Links to read about the above:
  """
  <:pp>http
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 3 prompt-length)"
top-p: 1.0
n-collate: 1
n-completions: 5
stop-sequences:
- "\"\"\""
cache: on
end-split-patterns:
- "\n"
vars:
- text
validator: url-exists
postprocessor: sed '$d' | xurls
var-defaults:
- "(sor (pen-selected-text t) (pen-preceding-text))"
examples:
- |-
    import Data.Foldable (for_)
    import Data.Traversable (for)

    import Control.Monad (when)
info: on
completion: off
insertion: off
{{< /highlight >}}


## Scripts {#scripts}


### `url-exists` {#url-exists}

This is the validator I have chosen to use.

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

url="$1"

stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

if stdin_exists && ! test -n "$url"; then
    url="$(cat)"
fi

curl-firefox -s -I "$url" | grep -q "HTTP.*200"
{{< /highlight >}}


## Example workflows {#example-workflows}

-   Select arbitrary text and gather URLs on it
-   Select any raw text (i.e. code with tricky and unnameable syntax)
-   Select large amounts of text and gather URLs on it


### Conclusions {#conclusions}

-   Much longer queries taking into account more context
-   Much more intelligent results
-   Much more specified results
-   Much more accurate results
-   Full control
-   No ads