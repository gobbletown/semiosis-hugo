+++
title = "Codex for generating textual transformations"
author = ["Shane Mulligan"]
date = 2021-10-10T00:00:00+13:00
keywords = ["codex", "nlp", "emacs", "openai"]
draft = false
+++

## Summary {#summary}

In situations where a LM is used to perform a
formatting transformation, it's wasteful to
continually prompt for the same
transformation.

It's more efficient to prompt for a regular
expression or some code to perform the same
transformation later; That's the objective of
this exercise.

I will start by demonstrating the usage of Codex to format text.


## Add a title and indent for each passage: {#add-a-title-and-indent-for-each-passage}

{{< highlight text "linenos=table, linenostart=1" >}}
Passage:
<delim>
John Wesley was an English clergyman, theologian, and evangelist, who was a leader of a revival movement within the Church of England known as Methodism. The societies he founded became the dominant form of the independent Methodist movement that continues to this day.
<delim>
John Wesley
     An English clergyman, theologian, and
     evangelist, who was a leader of a revival
     movement within the Church of England
     known as Methodism.

     The societies he founded became the
     dominant form of the independent
     Methodist movement that continues to this
     day.
<delim>
Passage:
<delim>
{{< /highlight >}}


### Demo {#demo}

Running `pf-format-a-text-passage-for-the-glossary/1`:

With input:

{{< highlight text "linenos=table, linenostart=1" >}}
An epithet is a byname, or a descriptive term,
accompanying or occurring in place of a name
and having entered common usage. It has
various shades of meaning when applied to
seemingly real or fictitious people,
divinities, objects, and binomial
nomenclature.
{{< /highlight >}}

Produces:

{{< highlight text "linenos=table, linenostart=1" >}}
An epithet
     A byname, or a descriptive term,
     accompanying or occurring in place of
     a name and having entered common usage.

     It has various shades of meaning when
     applied to seemingly real or fictitious
     people, divinities, objects, and
     binomial nomenclature.
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/gC28lfnof1h9bdHJt7JNZrU2F" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/gC28lfnof1h9bdHJt7JNZrU2F.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/gC28lfnof1h9bdHJt7JNZrU2F.js" id="asciicast-gC28lfnof1h9bdHJt7JNZrU2F" async></script>


## Generate a transformation script instead {#generate-a-transformation-script-instead}

Generating a transformation script may be
preferable.

If you foresee yourself running this
transformation many times, creating a script
for it rather than prompting may be worth doing.

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-textual-transformation-script-4.prompt>

`pf-generate-textual-transformation-script/4`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate textual transformation script"
doc: "Given some text input and output, generate a script to mimic the transformation"
prompt-version: 1
prompt: |+
  # Input text:
  input.txt <<EOD
  <input>
  EOD

  # Expected output:
  expected_output.txt <<EOD
  <expected output>
  EOD

  # Test the program
  cat input.txt | <sl:scripting language>_program > expected_output.txt

  # Program instructions:
  <instructions>

  cat <sl:scripting language>_program <<EOD
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
top-p: 1.0
stop-sequences:
- "EOD"
cache: on
# defs:
# - language: "\"Python\""
vars:
- "input"
- "expected output"
- scripting language
- instructions
preprocessors:
- cat
- cat
- cat
- <shell-comment>
pipelines:
- shell-comment: pen-str shell-commentify
examples:
- "John Wesley was an English clergyman, theologian, and evangelist, who was a leader of a revival movement within the Church of England known as Methodism. The societies he founded became the dominant form of the independent Methodist movement that continues to this day."
- |-
    John Wesley
         An English clergyman, theologian, and
         evangelist, who was a leader of a revival
         movement within the Church of England
         known as Methodism.

         The societies he founded became the
         dominant form of the independent
         Methodist movement that continues to this
         day.
- Python
- "Add a title and indent for each passage. The title is removed from the start of the passage."
info: on
completion: off
insertion: off
{{< /highlight >}}


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/pURMVyjhzMlkJZOtxOeFKpR5B" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/pURMVyjhzMlkJZOtxOeFKpR5B.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/pURMVyjhzMlkJZOtxOeFKpR5B.js" id="asciicast-pURMVyjhzMlkJZOtxOeFKpR5B" async></script>


## Findings {#findings}


### "Formatted" is a dirty word for Codex {#formatted-is-a-dirty-word-for-codex}

When using Codex, if I put `"Formatted Passage 2"` rather than `"Passage 2 for glossary:"`, then I get python codex intermixed, which
is not what I want.

Avoid using that word unless you are dealing
with code (as opposed to prose).

Example of python interference:

{{< highlight text "linenos=table, linenostart=1" >}}
An epithet
     is a byname, or a descriptive term,
     accompanying or occurring in place of a
     name and having entered common usage.
     It has various shades of meaning when
     applied to seemingly real or fictitious
     people, divinities, objects, and
     binomial nomenclature.
"""


def format(passages):
    formatted_passages = []
    for passage in passages:
        title = passage[:passage.index('\n')]
        formatted_passage = ''
        indented = False
        for line in passage.split('\n'):
            if line.startswith('
{{< /highlight >}}