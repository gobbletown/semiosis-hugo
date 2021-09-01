+++
title = "A syntax corrector with Codex"
author = ["Shane Mulligan"]
date = 2021-09-01T00:00:00+12:00
keywords = ["gpt", "codex", "pen", "openai"]
draft = false
+++

## Summary {#summary}

I just demonstrate the usage of a syntax corrector with Pen.el and codex.
This is a lot like the spelling and grammar corrector prompt with GPT-3 Davinci.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/5wFZY8x4gBD58FYRur5U3eZ5q" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/5wFZY8x4gBD58FYRur5U3eZ5q.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/5wFZY8x4gBD58FYRur5U3eZ5q.js" id="asciicast-5wFZY8x4gBD58FYRur5U3eZ5q" async></script>


### Bad awk code {#bad-awk-code}

{{< highlight awk "linenos=table, linenostart=1" >}}
if NR >= window_size && (effective_nr % step == 0) {
    s = ""
    for (i=1; i<window_size; i++) {
        s = s memory[i] RS
    }
    s = s memory[window_size]
    printf("(%s)", s) |& cmd

    close(cmd, "to");
    $0 = "" + "my string";

    brs=RS
    RS="##long read##"
    cmd |& getline $0;
    fflush(cmd);
    close(cmd);
    RS=brs
    print; system("");
}
{{< /highlight >}}


### Corrected code {#corrected-code}

{{< highlight awk "linenos=table, linenostart=1" >}}
if (NR >= window_size && (effective_nr % step == 0)) {
    s = ""
    for (i=1; i<window_size; i++) {
        s = s memory[i] RS
    }
    s = s memory[window_size]
    printf("(%s)", s) |& cmd

    close(cmd, "to");
    $0 = "" "my string";

    brs=RS
    RS="##long read##"
    cmd |& getline $0;
    fflush(cmd);
    close(cmd);
    RS=brs
    print; system("");
}
{{< /highlight >}}


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Correct the syntax"
doc: "Given some selected code, correct the syntax"
prompt-version: 1
prompt: |+
  """
  The following awk has incorrect syntax:
  """
  if NR >= window_size && (effective_nr % step == 0) {
    $0 = "" + "my string";
  }
  """
  The same program but with corrected syntax:
  """
  if (NR >= window_size && (effective_nr % step == 0)) {
    $0 = "" "my string";
  }
  """
  The following <language> has incorrect syntax:
  """
  <bad code>
  """
  The same program but with corrected syntax:
  """
engine: "OpenAI Codex"
stop-sequences:
- "\"\"\""
temperature: 0.3
max-tokens: "(* 2 prompt-length)"
top-p: 1.0
cache: on
vars:
- "language"
- "bad code"
var-defaults:
- "(pen-detect-language-ask)"
- "(pen-selected-text)"
examples:
- "awk"
- |-
  if NR >= window_size && (effective_nr % step == 0) {
      s = ""
      for (i=1; i<window_size; i++) {
          s = s memory[i] RS
      }
      s = s memory[window_size]
      printf("(%s)", s) |& cmd

      close(cmd, "to");
      $0 = "" + "my string";

      brs=RS
      RS="##long read##"
      cmd |& getline $0;
      fflush(cmd);
      close(cmd);
      RS=brs
      print; system("");
  }
preprocessors:
- "sed 's/^/- /"
- "cat"
filter: no
completion: on
insertion: on
{{< /highlight >}}