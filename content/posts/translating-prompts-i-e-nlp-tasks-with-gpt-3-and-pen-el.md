+++
title = "Translating prompts (i.e. NLP tasks) with GPT-3 and Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-19T00:00:00+12:00
keywords = ["gpt", "pen", "openai", "nlp"]
draft = false
+++

## Summary {#summary}

I demonstrate how to encode into your prompts
information on how to translate it, so that it
then becomes a straight-forward process to
translate prompts/NLP tasks designed in
different languages and sub-languages (topics)
into your own language. This is important for
people to be able to collaborate on prompts.


## Translator example {#translator-example}

The translator takes stdin and templates into the command two arguments, from-language and to-language.
The output is the prompt designed for a different language. For example,
Turn an _"anything->English dictionary"_ into an _"anything->YourLanguage dictionary"_.

4 default parameters: `stdin(prompt)`, `<from-language>`, `<to-language>`, and `<topic>`.

If the prompt specifies the `language` and/or
`topic` keys, they will substituted in to
their respective positions. Otherwise, those
parameters are detected.

The following translator specification is a
template for a shell command. The substituted
variables are automatically quoted.

{{< highlight yaml "linenos=table, linenostart=1" >}}
translator: "wrlp pf-translate-from-world-language-x-to-y/3 <from language> <to language>"
{{< /highlight >}}

Here, `wrlp` is a script that reads each line
of the prompt one at a time and pipes that
line into the prompt function for translating
one world language into another.

It may also be specified in emacs lisp.
The arguments are provided through lexical scope.

{{< highlight yaml "linenos=table, linenostart=1" >}}
translator: "(wrlp prompt (pf-translate-from-world-language-x-to-y/3 from-language to-language))"
{{< /highlight >}}


## The prompt to be translated {#the-prompt-to-be-translated}

The prompt definition includes `language`,
`topic` and `translator` as hints to how it is
supposed to be translated.

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: define word for glossary
aliases:
- define word
prompt-version: 5
doc: Get a short definition of a term for the glossary system.
language: English
topic: Dictionary
translator: "(wrlp prompt (pf-translate-from-world-language-x-to-y/3 from-language to-language))"
prompt: |
  Glossary of terms.

  ossified
  Definition: Turn into bone or bony tissue.

  <1>
  Definition:
engine: OpenAI Davinci
temperature: 0.5
max-tokens: 200
top-p: 1
stop-sequences:
- "\n\n"
vars:
- word
examples:
- boysenberry
completion: true
postprocessor: sed 's/\s\+/ /g' | pen-s join ' ' | sed 's/^ *//'
prettifier: pen-pretty-paragraph
var-defaults:
- "(pen-thing-at-point-ask \"word\" t)"
{{< /highlight >}}


### Exposition of internal translation {#exposition-of-internal-translation}

{{< highlight bash "linenos=table, linenostart=1" >}}
cat "$PROMPTS/define-word-for-glossary.prompt" | yq -r .prompt | wrlp penf pf-translate-from-world-language-x-to-y/3 English French
{{< /highlight >}}

```bash
Glossaire.

ossifié
Définition : devenir osseux ou os.

<1>
Définition:

```


## Emacs lisp {#emacs-lisp}


### Default translators {#default-translators}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defvar pen-translators
  '("(wrlp prompt (pf-translate-from-world-language-x-to-y/3 from-language to-language))")
  "A list of translators that can be used to translate prompts.
These may be string representations of emacs lisp if beginning with '('.
Otherwise, it will be a shell expression template")
{{< /highlight >}}


### Translating a loaded prompt {#translating-a-loaded-prompt}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-translate-prompt ()
  "Select a prompt file and translate it."
  (interactive)
  (let* ((fname (fz pen-prompt-functions nil nil "pen translate prompt: "))
         (yaml (ht-get pen-prompts fname))
         (prompt (ht-get yaml "prompt"))
         (topic (ht-get yaml "topic"))
         (from-lang (ht-get yaml "language"))
         (from-lang (or from-lang (read-string-hist ".prompt Origin Language: ")))
         (to-lang (read-string-hist ".prompt Destination Language: "))
         (translator (let ((tlr (ht-get yaml "translator")))
                       (if (and
                            (sor tlr)
                            (not (string-match "^(" tlr)))
                           ;; if it's a shell script, convert it to elisp
                           (setq tlr
                                 (format
                                  "(pen-sn %s prompt)"
                                  (pen-q
                                   (pen-expand-template-keyvals
                                    tlr
                                    '(("from-language" . (pen-q from-lang))
                                      ("from language" . (pen-q from-lang))
                                      ("to-language" . (pen-q to-lang))
                                      ("to language" . (pen-q to-lang))
                                      ("topic" . (pen-q topic))
                                      ("prompt" . (pen-q prompt))))))))
                       tlr))
         (translator (or translator
                         (fz pen-translators nil nil "Select a prompt translator: ")))
         (translator (pen-eval-string
                      (concat "'" translator)))
         (newprompt
          (if translator
              (eval
               `(let ((from-language ,from-lang)
                      (to-language ,to-lang)
                      (topic ,topic)
                      (prompt ,prompt))
                  (pen-single-generation ,translator))))))
    (etv newprompt)))
{{< /highlight >}}

Original, in English:

{{< highlight text "linenos=table, linenostart=1" >}}
Glossary of terms.

ossified
Definition: Turn into bone or bony tissue.

<1>
Definition:
{{< /highlight >}}

New, in French:

{{< highlight text "linenos=table, linenostart=1" >}}
Glossaire.

ossifié
Définition: se changer en os ou en tissu osseux.

<1>
Définition:
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/vzo0q750cUC7KVSGrDSZ7s3dV" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/vzo0q750cUC7KVSGrDSZ7s3dV.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/vzo0q750cUC7KVSGrDSZ7s3dV.js" id="asciicast-vzo0q750cUC7KVSGrDSZ7s3dV" async></script>