+++
title = "spaCy in emacs"
author = ["Shane Mulligan"]
date = 2021-03-09T00:00:00+13:00
keywords = ["gpt-3", "spacy", "nlp", "emacs"]
draft = false
+++

## Summary {#summary}

I begin construction of an environment for developing with `spaCy`.

-   Goals
    -   `spaCy` pipeline builder/wizard
    -   Select and analyse text with `spaCy` linguistic features
    -   `spaCy` python playground text selection
    -   configuration of `spaCy` using emacs `custom.el`


## `deplacy` demo {#deplacy-demo}

`deplacy` code
: <https://github.com/KoichiYasuoka/deplacy>

<a title="asciinema recording" href="https://asciinema.org/a/OJcQVwHftFkLEypmaN8Qv8vLz" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/OJcQVwHftFkLEypmaN8Qv8vLz.svg" /></a>


## Code generation and bindings {#code-generation-and-bindings}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro etv-filter (cmd)
  (let* ((slug (slugify cmd))
         (sym (str2sym (concat "etv-" slug))))
    `(defun ,sym (&optional input)
       (interactive (list (my/selected-text)))
       (if (not input)
           (setq input (my/selected-text)))
       (etv (snc ,cmd input)))))

(cl-loop for s in
         '("partsofspeech"
           "entities"
           "displacy"
           "token-pos-dep"
           "sentiment"
           "segment-sentences")
         do
         (eval
          (expand-macro
           `(etv-filter ,s))))

(define-key selected-keymap (kbd "Z n") 'ngram-query-replace)
(define-key selected-keymap (kbd "Z S") 'sps-play-spacy)
(define-key selected-keymap (kbd "Z P") 'etv-partsofspeech)
(define-key selected-keymap (kbd "Z E") 'etv-entities)
(define-key selected-keymap (kbd "Z D") 'etv-displacy)
(define-key selected-keymap (kbd "Z T") 'etv-token-pos-dep)
(define-key selected-keymap (kbd "Z N") 'etv-sentiment)
(define-key selected-keymap (kbd "Z G") 'etv-segment-sentences)
{{< /highlight >}}

{{< figure src="/ox-hugo/spacy-bindings.png" >}}


## Configuration {#configuration}


### `yaml` {#yaml}

I store the configuration of `spaCy` inside a `yaml` file.

{{< figure src="/ox-hugo/spacyconf.png" >}}


### `custom.el` {#custom-dot-el}

I create a customization interface within
emacs for configuring `spaCy` system-wide.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defcustom spacy-model ""
  "spaCy model"
  :type 'string
  :group 'system-custom
  :initialize #'custom-initialize-default
  :options (list "en"
                 "en_core_web_trf"
                 "en_core_web_sm"
                 "en_pytt_bertbaseuncased_lg"
                 "de_core_news_sm"
                 "fr")
  :set (lambda (_sym value)
         (myrc-set (tr "-" "_" (sym2str _sym)) value)
         (set _sym (sor value)))
  ;; The default :initialize is custom-initialize-reset
  ;; And uses the :set function
  ;; :initialize (lambda
  :get (lambda (_sym)
         (let* ((yaml (yamlmod-read-file "/home/shane/notes/myrc.yaml"))
                (cfgval (sor (ht-get yaml (tr "-" "_" (sym2str _sym))))))

           (if cfgval
               (set _sym cfgval)
             ""))))
{{< /highlight >}}

{{< figure src="/ox-hugo/spacyconf-custom.png" >}}


## external scripts {#external-scripts}

This is an example of one of the exterynal
scripts that is used by and configured with
emacs via a `yaml` config.

<span class="underline">**displacy**</span>

{{< highlight python "linenos=table, linenostart=1" >}}
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
data = sys.stdin.read()

import sys
program_name = sys.argv[0]
arguments = sys.argv[1:]
count = len(arguments)

import spacy

import shanepy
spacy_model = shanepy.myrc_get("spacy_model") or "en_core_web_md"
nlp = spacy.load(spacy_model)

doc = nlp(data)
spacy.displacy.serve(doc, style='dep')
{{< /highlight >}}


## Demonstration {#demonstration}

This demonstrates configuration, context
menus, bindings, code generation of scripts
all in emacs.

<a title="asciinema recording" href="https://asciinema.org/a/G2RFrnbIiAgYv5uz5lbnYNqxr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/G2RFrnbIiAgYv5uz5lbnYNqxr.svg" /></a>