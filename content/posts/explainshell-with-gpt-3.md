+++
title = "explainshell with GPT-3"
author = ["Shane Mulligan"]
date = 2021-06-16T00:00:00+12:00
keywords = ["gpt"]
draft = false
+++

## Summary {#summary}

I make a simple GPT-3 prompt to explain shell
code while using emacs.

It mimics the functionality of `explainshell`
but it's able to also describe the purpose of
commands with syntax and those that are semi-baked or
pseudocode.

Related
: <https://mullikine.github.io/posts/crontab-guru-in-emacs/>


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/YoWeRJC6W1PhoZuyrtFUQ39sB" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/YoWeRJC6W1PhoZuyrtFUQ39sB.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/YoWeRJC6W1PhoZuyrtFUQ39sB.js" id="asciicast-YoWeRJC6W1PhoZuyrtFUQ39sB" async></script>


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
in-development: no
title: "explain a shell command"
issues:
design-patterns:
# future-titles: ""
# aims:
doc: "Explain what a shell command does"
# aims: |+
# - More abstractive rewording
prompt-version: 1
# <:pp> defines a point where the following
# text is concatenated before the postprocessor
# is run.
# <1>, <2> etc. are where variables are substituted
# <1> is special because it may be the current selection
# <2> May be inferred from <1> via a prompt.
# This way, a function can be curried/beta-reduced to a function of 1 argument.
# Use the markdown style
prompt: |+
    The following is a list of one-liners for the linux command-line and some explanations:

    $ ls -t * | head -1
    The above command gets the newest file in directory bash.
    ###
    $ find . -name '*' -type f -not -path '*.mp3'
    The above command finds with invert match - e.g. find every file that is not mp3
    ###
    $ find . -name "node_modules" -exec rm -rf '{}' +
    The above command recursively removes all "node_modules" folders
    ###
    $ <1>
    The above command
problems:
# Put problems directly after the prompt because the prompt is a multiline string.
# - "Struggles with the latter columns."
# # Additional transformation of prompt after the template
# prompt-filter: "sed -z 's/\s\+$//'"
# # Trailing whitespace is always removed
# prompt-remove-trailing-whitespace: on
# myrc will select the completion engine using my config.
# This may be openi-complete or something else
engine: "myrc"
# if nothing is selected in myrc and openapi-complete is used
# by default, then openai should select this engine.
preferred-openai-engine: "davinci"
# 0.0 = /r/ihadastroke
# 1.0 = /r/iamveryrandom
# Use 0.3-0.8
temperature: 0.3
max-tokens: 200
top-p: 1.0
# Not available yet: openai api completions.create --help
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
# Only the first one will be used by the API,
# but the completer script will use the others.
# Currently the API can only accept one stop-sequence, but that may change.
stop-sequences:
# - "\n"
# - "\n\n"
# 2 hashes is more reliable as a stop sequence because
# sometimes the generation morphs from ### to ##
- "##"
# inject-start-text: yes
# inject-restart-text: yes
# show-probabilities: off
# Cache the function by default when running the prompt function
cache: on
vars:
- "code"
examples:
- "raco pkg show --all"
# test-output: "both are types of berry"
# Completion is for generating a company-mode completion function
# completion: on
# # default values for pen -- evaled
# # This is useful for completion commands.
# pen-defaults:
# - "(pen-surrounding-text)"
# These are elisp String->String functions and run from pen.el
# It probably runs earlier than the preprocessors shell scripts
# pen-preprocessors:
# - "pen-pf-correct-grammar"
# # A preprocessor filters the var at that position
# the current implementation of preprocessors is kinda slow and will add ~100ml per variable
# # This may be useful to distinguish a block of text, for example
# preprocessors:
# - "sed 's/^/- /"
# - "cat"
chomp-start: on
chomp-end: off
prefer-external: on
# This is an optional external command which may be used to perform the same task as the API.
# This can be used to train the prompt.
# The external command must take arguments, not stdin
# So that all variables may be passed into it.
# external: "generate-text-from-input.sh"
# This compares the output of the external script to the output of the LM
# similarity-test:
# This script returns a 0-1 decimal value representing the quality of the generated output.
# The input is 2 arguments each containing output
# The output is a decimal number from 0 to 1
# quality-script: "my-quality-checker-for-this-prompt.sh"
# This script can be used to validate the output.
# If the output is accurate, the validation script returns exit code 1.
# The input is 2 arguments each containing output
# validation-script: "my-validator-for-this-prompt.sh"
# Enable running conversation
conversation-mode: no
# This is the name of an external database-driven pretext generator.
# It would typically summarize and fact extract from history.
# It then passes the pretext to the new prompt.
# conversation-pretext-generator: "human-conversation"
# Replace selected text
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
external-related:
- "https://explainshell.com/"
needs-work: no
n-test-runs: 5
related-prompts:
- "annotate-with-commentary.prompt"
# Prompt function aliases
# aliases:
# - "asktutor"
# postprocessor: "sed 's/- //' | uniqnosort"
# # Run it n times and combine the output
# n-collate: 10
# This for combining prompts:
# It might be, for example, summarize, or uniqnosort
# collation-postprocessor: "uniqnosort"
{{< /highlight >}}