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
  "Select a prompt file and translate it.
Reconstruct the entire yaml-ht for a different language."
  (interactive)

  (cl-macrolet ((translate
                 (input)
                 `(eval
                   `(let ((from-language ,from-lang)
                          (to-language ,to-lang)
                          (topic ,topic)
                          (input ,,input))
                      (if input
                          (pen-single-generation ,translator))))))
    (let* ((fname (fz pen-prompt-functions nil nil "pen translate prompt: "))
           (yaml-ht (ht-get pen-prompts fname))
           (prompt (ht-get yaml-ht "prompt"))
           (title (ht-get yaml-ht "title"))
           (task (ht-get yaml-ht "task"))
           (doc (ht-get yaml-ht "doc"))
           (topic (ht-get yaml-ht "topic"))
           (vars (vector2list (ht-get yaml-ht "vars")))
           (var-slugs (mapcar 'slugify vars))
           (examples-list (vector2list (ht-get yaml-ht "examples")))
           (from-lang (ht-get yaml-ht "language"))
           (from-lang (or from-lang (read-string-hist ".prompt Origin Language: ")))
           (to-lang (read-string-hist ".prompt Destination Language: "))
           (translator (let ((tlr (ht-get yaml-ht "translator")))
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
                                        ;; It's called 'input' and not 'prompt' because
                                        ;; we could translate other fields, such as variable names
                                        ("input" . (pen-q prompt))))))))
                         tlr))
           (translator (or translator
                           (fz pen-translators nil nil "Select a prompt translator: ")))
           (translator (pen-eval-string
                        (concat "'" translator))))

      (if translator
          (let* ((new-prompt (translate prompt))
                 (new-title (translate title))
                 (new-task (translate task))
                 (new-topic (translate topic))
                 (new-doc (translate doc))
                 ;; is there a mapcar for macros?
                 (new-vars (loop for v in vars collect
                                 (translate v)))
                 ;; (new-var-slugs (mapcar 'slugify new-vars))
                 (new-examples
                  (if (vectorp (car examples-list))
                      (mapcar
                       (lambda (v)
                         (loop for e in (vector2list v) collect
                               (translate e)))
                       examples-list)
                    (loop for e in examples-list collect
                          (translate e))))
                 (new-prompt
                  (pen-expand-template-keyvals
                   new-prompt
                   (-zip vars (mapcar (lambda (s) (format "<%s>" s)) new-vars))))
                 (newht (let ((h (make-hash-table :test 'equal)))
                          (ht-set h "prompt" new-prompt)
                          (ht-set h "title" new-title)
                          (ht-set h "task" new-task)
                          (ht-set h "doc" new-doc)
                          (ht-set h "examples" new-examples)
                          (ht-set h "topic" new-topic)
                          (ht-set h "vars" new-vars)
                          (ht-merge yaml-ht h)))
                 (newyaml (plist2yaml (ht->plist newht))))
            (pen-etv newyaml)))
      ;; (ht-get pen-prompts "pf-define-word/1")
      ;; (ht-get pen-prompts 'pf-define-word-for-glossary/1)
      )))
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

In this demo, an entire prompt description
including documentation, variable names and
examples is translated from one language into
another.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/IKOJHd2Y7NxJJ47massvb7VM3" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/IKOJHd2Y7NxJJ47massvb7VM3.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/IKOJHd2Y7NxJJ47massvb7VM3.js" id="asciicast-IKOJHd2Y7NxJJ47massvb7VM3" async></script>