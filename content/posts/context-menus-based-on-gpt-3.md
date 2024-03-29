+++
title = "Context menus based on GPT-3"
author = ["Shane Mulligan"]
date = 2021-03-09T00:00:00+13:00
keywords = ["gpt", "openai", "emacs"]
draft = false
+++

## Summary {#summary}

I create a GPT-3 prompt for testing to see if
code is Haskell and use it as a test inside
emacs to suggest further functions.

Prompt file
: <http://github.com/semiosis/prompts/blob/master/prompts/get-language.prompt>


## Demonstration {#demonstration}

As you can see, GPT-3 is able to detect the
language and I can use that as a test in my
emacs to provide further functions. The
suggested function was yet another GPT-3
prompt function for translating Haskell into
Clojure.

<a title="asciinema recording" href="https://asciinema.org/a/IUrbnMxTqP3cFtQB5CPWelCuP" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/IUrbnMxTqP3cFtQB5CPWelCuP.svg" /></a>

<span class="underline">**The faster and more impressive version**</span>
This uses emacs' `language-detection` package.

<a title="asciinema recording" href="https://asciinema.org/a/64hjDdeeqkhTEzZA9RvYjKyy9" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/64hjDdeeqkhTEzZA9RvYjKyy9.svg" /></a>


## Create a prompt to check if selected code is Haskell {#create-a-prompt-to-check-if-selected-code-is-haskell}

-   The following prompt has been obsoleted by `get-language.prompt`.

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "test if text is haskell"
doc: "tests if text is haskell code"
prompt: |+
    If the code is haskell, respond with "Haskell". If the query is not haskell, respond with "?".

    ###
    Code:putStrLn "What is your name?"
    Language:Haskell
    ###
    Code:[1,2,3,4] ++ [9,10,11,12]
    Language:Haskell
    ###
    Code:In reality, it would require too much code to hide _Prelude_ clashes like this, so you
    Language:?
    ###
    Code:data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
    Language:Haskell
    ###
    Code:In reality, it would require too much code to hide _Prelude_ clashes like this, so you
    ###
    Code:(ns example.pprinter (:use clojure.pprint))
    Language:?
    ###
    Code:surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
    Language:Haskell
    ###
    Code:"Steve Buscemi" !! 6
    Language:Haskell
    ###
    Code:regex = re.compile(r'^(?P<indent>(?: {4})*)(?P<name>\S.*)')
    Language:?
    ###
    Code:doubleSmallNumber x = if x > 100 then x else x*2
    Language:Haskell
    ###
    Code:[ x | x <- [50..100], x `mod` 7 == 3]
    Language:Haskell
    ###
    Code:* TODO Look into =ctrl=
    Language:?
    ###
    <1>
    Language:
engine: "davinci"
temperature: 0.5
max-tokens: 40
top-p: 1.0
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "###"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "text"
examples:
- |+
    putStrLn $ "Hey, I Love " ++ city ++ "!"
external: ""
conversation-mode: no
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
{{< /highlight >}}


## Add code to use `GPT-3` to test the selected text {#add-code-to-use-gpt-3-to-test-the-selected-text}

`GPT-3` will test if the selected text is in the Haskell language.

It will then provide a context menu option to translate it into Clojure.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun gpt-test-haskell ()
  (let ((lang (pen-pf-test-if-text-is-haskell (selection))))
    (message lang)
    (string-equal (message lang) "Haskell")))

;; This is so I can jump here
(defvar right-click-context-global-menu-tree-placeholder nil)
(setq right-click-context-global-menu-tree
      '(("Cancel" :call identity)
        ("Edit menu" :call (j 'right-click-context-global-menu-tree-placeholder))
        ("Button cloud" :call toggle-buttoncloud)
        ("GPT-3"
         ("Convert Haskell to Clojure" :call pen-pf-translate-haskell-to-clojure :if (gpt-test-haskell)))
        ("NLP"
         ("Summarize" :call (progn
                              (call-interactively 'pen-pf-eli5-explain-like-i-m-five)
                              (call-interactively 'fi-text-to-paras)) :if (selected-p))
         ("Vexate" :call (progn
                           (call-interactively 'pen-pf-complicated-explanation-of-how-to-x)
                           (call-interactively 'fi-text-to-paras)) :if (selected-p))
         ("Fast Paras" :call (call-interactively 'fi-text-to-paras-nosegregate) :if (selected-p))
         ("Paras" :call (call-interactively 'fi-text-to-paras) :if (selected-p))
         ("spaCy" :call (call-interactively 'sps-play-spacy) :if (selected-p)))
))
{{< /highlight >}}