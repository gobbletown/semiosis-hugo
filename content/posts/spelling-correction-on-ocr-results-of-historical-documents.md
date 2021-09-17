+++
title = "Spelling correction on OCR results of historical documents"
author = ["Shane Mulligan"]
date = 2021-09-17T00:00:00+12:00
keywords = ["gpt", "codex", "prompt-of-the-day", "openai"]
draft = false
+++

## Summary {#summary}

My friend Chen sent me a prompt today. It
takes text which has been interpreted by an
OCR program and outputs text which is likely
inferred.

{{< highlight text "linenos=table, linenostart=1" >}}
This is an example of spelling correction on optical character recognition results of historical
documents:

OCR Result: r'2Pto flare i~t in the face; for this riea/on they ef~d all tho~-e Books which gave the
least ac- of the Heathen Theology to the flames ;

Correction: For a to flare it in the face; for this reason they sacrificed all those books which gave
the least account of the Heathen Theology to the flames;

OCR Result: they continu~e chofep ha~its ~fill in t-he mink .&fen th~at th~e generality of
Mlankiind conts ing~r them~relv~es with a ju~per~fcial knowvledye th-ingfSif.
Correction: they continue the same habits till in the midst and even to the end that the generality
of Mankind content themselves with a superficial knowledge of things.

Correction:
{{< /highlight >}}


## OCR Correction prompt {#ocr-correction-prompt}

`correct-the-results-of-ocr-1.prompt`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "correct the results of OCR"
doc: "Given OCR results, correct them"
prompt-version: 2
prompt: |+
    These are the results of performing optical character recognition on the historical documents.
    <delim>
    OCR Result:
    <delim>
    r'2Pto flare i~t in the face; for this riea/on they ef~d all tho~-e Books which gave the least ac- of the Heathen Theology to the flames ;
    <delim>
    Correction:
    <delim>
    For a to flare it in the face; for this reason they sacrificed all those books which gave the least account of the Heathen Theology to the flames;
    <delim>
    OCR Result:
    <delim>
    <OCR text>
    <delim>
    Correction:
    <delim>

engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 (pen-approximate-token-length ocr-text final-approximate-token-char-length))"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "OCR text"
examples:
- |+
  they continu~e chofep ha~its ~fill in t-he mink .&fen th~at th~e generality of
  Mlankiind conts ing~r them~relv~es with a ju~per~fcial knowvledye th-ingfSif.
# TODO Add to .prompt optional multiplex preprocessor for inputs
# If a postprocessor sees pf- then check the function to see if it has
# multiplex capability
postprocessor: pf-correct-grammar/1
filter: on
completion: off
insertion: off
{{< /highlight >}}


### Input {#input}

{{< highlight text "linenos=table, linenostart=1" >}}
they continu~e chofep ha~its ~fill in t-he mink .&fen th~at th~e generality of
Mlankiind conts ing~r them~relv~es with a ju~per~fcial knowvledye th-ingfSif.
{{< /highlight >}}


### Results {#results}


#### 1 {#1}

{{< highlight text "linenos=table, linenostart=1" >}}
they continue the same habits till in the
midst and even till that the generality of
mankind con- tinue them selves with a
superficial knowledge of thing's if.
{{< /highlight >}}


#### 2 {#2}

{{< highlight text "linenos=table, linenostart=1" >}}
they continue in the habits of their
ancestors; and that the generality of mankind
con- template themselves with a superficial
knowledge of things in themselves.
{{< /highlight >}}


#### 3 {#3}

{{< highlight text "linenos=table, linenostart=1" >}}
they continue chosen habits till in the midst
and even that the generality of mankind con-
t ing er
selves with a superficial knowledge
of things if.
{{< /highlight >}}


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/idXGC3F3Ay0OymoyPOm8hLB6X" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/idXGC3F3Ay0OymoyPOm8hLB6X.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/idXGC3F3Ay0OymoyPOm8hLB6X.js" id="asciicast-idXGC3F3Ay0OymoyPOm8hLB6X" async></script>


## Followed by spelling correction {#followed-by-spelling-correction}

{{< highlight yaml "linenos=table, linenostart=1" >}}
postprocessor: pf-correct-grammar/1
{{< /highlight >}}


## Results {#results}

{{< highlight text "linenos=table, linenostart=1" >}}
they continued to choose habits that fill
what they suppose are their basic needs and
feel that the generality of mankind contains
in them virtually the same relationship with
just a superficial knowledge of things.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
They continued to choose
habits that fill what they supposed are their
basic needs and feel that the generality of
mankind contains in them virtually the same
relationship with just a superficial
knowledge of things.
{{< /highlight >}}