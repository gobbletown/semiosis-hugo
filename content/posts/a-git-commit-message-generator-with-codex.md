+++
title = "A git commit message generator with Codex"
author = ["Shane Mulligan"]
date = 2021-09-02T00:00:00+12:00
keywords = ["codex", "pen", "openai", "git"]
draft = false
+++

## Summary {#summary}

I make a git commit message generator in
`Pen.el` with OpenAI's Codex.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/az2uRwBuuS3PyF0mLRBTYCckm" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/az2uRwBuuS3PyF0mLRBTYCckm.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/az2uRwBuuS3PyF0mLRBTYCckm.js" id="asciicast-az2uRwBuuS3PyF0mLRBTYCckm" async></script>


### Example diff {#example-diff}

<a id="code-snippet--diff"></a>
{{< highlight text "linenos=table, linenostart=1" >}}
diff --git a/scripts/pen-ai21 b/scripts/pen-ai21
index 4859e4a..dbcbc7c 100755
--- a/scripts/pen-ai21
+++ b/scripts/pen-ai21
@@ -42,13 +42,15 @@ pen_ai21() {
     # cmd-nice-jq is very slow. Find a faster, reliable solution.
     # Or use python

+    # \"maxTokens\": $PEN_ENGINE_MAX_GENERATED_TOKENS,
+
     curl "https://api.ai21.com/studio/v1/$PEN_MODEL/complete" \
          -H 'Content-Type: application/json' \
          -H "Authorization: Bearer $AI21_API_KEY" \
          -X POST \
          -d "{\"prompt\": $(cmd-nice-jq "$PEN_PROMPT"),
               \"numResults\": $PEN_N_COMPLETIONS,
-              \"maxTokens\": $PEN_ENGINE_MAX_GENERATED_TOKENS,
+              \"maxTokens\": $PEN_MAX_TOKENS,
               \"stopSequences\": [$(cmd-nice-jq "$PEN_STOP_SEQUENCE")],
               \"topKReturn\": $PEN_TOP_K,
               \"temperature\": $PEN_TEMPERATURE
diff --git a/src/pen.el b/src/pen.el
index 800a7d1..7c88475 100644
--- a/src/pen.el
+++ b/src/pen.el
@@ -1497,10 +1497,12 @@ Function names are prefixed with pf- for easy searching"
                         (model (ht-get yaml-ht "model"))

                         ;; min-tokens and max-tokens include the prompt
+                        ;; These values may also be inferred from max-generated-tokens + an approximation of the prompt size.
                         (min-tokens (ht-get yaml-ht "min-tokens"))
                         (max-tokens (ht-get yaml-ht "max-tokens"))

                         ;; min-generated-tokens and max-generated-tokens do not include the prompt
+                        ;; These values may also be inferred from max-tokens - an approximation of the prompt size.
                         (min-generated-tokens (ht-get yaml-ht "min-generated-tokens"))
                         (max-generated-tokens (ht-get yaml-ht "max-generated-tokens"))
{{< /highlight >}}


### Suggestions from first query {#suggestions-from-first-query}

{{< highlight text "linenos=table, linenostart=1" >}}
pf-api: update AI21 model
pen-ai21: extract AI21 to separate project
Add pen-ai21 as a more elaborate pen
pen-ai21: better alignment of generated completions
Ship AI21 Studio API key in a central place
pen-ai21: Add additional fields and sanitize
big-commit-with-diff
Add more bash completion
Use curl for AI21 completion requests
Revert "Do not include prompt in tokens to pen-ai21"
{{< /highlight >}}


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "Commit message from diff"
doc: "Generate a commit message from a diff"
is-info: on
prompt-version: 1
prompt: |+
    cat git-diff.txt <<EOD
    git diff HEAD\^!
    <diff>
    EOD

    git log HEAD\^! --oneline
    5ebcee4 (HEAD -> master)
engine: "OpenAI Codex"
temperature: 0.8
max-tokens: 60
n-completions: 10
top-p: 1.0
cache: on
vars:
- "diff"
preprocessors:
- "sed 's/^/> /'"
var-defaults:
- "(pen-selected-text)"
stop-sequences:
- "\n"
# validator: "wc -l | grep -qP '^1$'"
examples:
- |+
    diff --git a/prompts/randomize-private-info.prompt b/prompts/randomize-private-info.prompt
    index d4a21d1..4a4cc74 100644
    --- a/prompts/randomize-private-info.prompt
    +++ b/prompts/randomize-private-info.prompt
    @@ -21,10 +21,10 @@ prompt: |+
         Date:   Sun Apr 4 13:26:51 2021 +0200
         EOD

    -    # The exact same data file but with different private details
    -    cat data-fake.txt <<EOD
    +    # The exact same data file but with different and fake private details
    +    cat fake-data.txt <<EOD
     engine: "OpenAI Codex"
    -temperature: 0.8
    +temperature: 0.9
     max-tokens: "(* 2 prompt-length)"
     stop-sequences:
     - "EOD"
filter: on
completion: off
insertion: off
{{< /highlight >}}