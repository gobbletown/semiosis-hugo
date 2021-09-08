+++
title = "Search the web with Codex"
author = ["Shane Mulligan"]
date = 2021-09-09T00:00:00+12:00
keywords = ["codex", "pen", "gpt", "emacs"]
draft = false
+++

## Summary {#summary}

This is a demonstration of using a LM to
search the internet.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/E2UDqQgfvEGaMOi8q0BZWbJdE" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/E2UDqQgfvEGaMOi8q0BZWbJdE.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/E2UDqQgfvEGaMOi8q0BZWbJdE.js" id="asciicast-E2UDqQgfvEGaMOi8q0BZWbJdE" async></script>


## Prompt {#prompt}

Source
: <http://github.com/semiosis/prompts/blob/master/prompts/get-urls-for-a-passage-1.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get URLs for a passage"
doc: "Given some text return relevant URLs"
prompt-version: 1
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
stop-sequences:
- "\"\"\""
cache: on
end-split-patterns:
- "\n"
vars:
- text
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