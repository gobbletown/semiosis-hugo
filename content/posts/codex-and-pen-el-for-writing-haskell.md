+++
title = "Codex and Pen.el for writing Haskell"
author = ["Shane Mulligan"]
date = 2021-10-11T00:00:00+13:00
keywords = ["codex", "pen", "openai", "emacs"]
draft = false
+++

## Summary {#summary}

I demonstrate some of the capabilities of
Pen.el with Codex for assisting in writing
Haskell.


## Some key bindings {#some-key-bindings}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key pen-map (kbd "H-^") 'pen-transform)
(define-key pen-map (kbd "H-p ^") 'pen-transform)
(define-key pen-map (kbd "H-p t") 'pen-transform)
(define-key pen-map (kbd "H-p i") 'pen-insert-snippet-from-lm)
(define-key pen-map (kbd "H-p l") 'pf-explain-some-code/2)
(define-key pen-map (kbd "H-p e") 'pf-get-an-example-of-the-usage-of-a-function/2)
(define-key pen-map (kbd "H-p a") 'pf-append-to-code/3)
(define-key pen-map (kbd "H-p n") 'pen-select-function-from-nl)
(define-key pen-map (kbd "H-p f") 'pen-autofix-lsp-errors)
{{< /highlight >}}


## Append to code with natural language {#append-to-code-with-natural-language}

| kb      | f                     |           |
|---------|-----------------------|-----------|
| `H-p a` | `pf-append-to-code/3` | `pen-map` |

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/append-to-code-3.prompt>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/2p0kl78iHcL35TaX27HEqMqNn" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/2p0kl78iHcL35TaX27HEqMqNn.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/2p0kl78iHcL35TaX27HEqMqNn.js" id="asciicast-2p0kl78iHcL35TaX27HEqMqNn" async></script>


## Find a function given a NL use-case {#find-a-function-given-a-nl-use-case}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/EYnsb6CD4pWz8u1xD9IBtcSkP" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/EYnsb6CD4pWz8u1xD9IBtcSkP.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/EYnsb6CD4pWz8u1xD9IBtcSkP.js" id="asciicast-EYnsb6CD4pWz8u1xD9IBtcSkP" async></script>

Get the signature of that funtion
: <https://asciinema.org/a/dIWvR63X9eKUxvRxuP919LhGy>


## Select function from NL {#select-function-from-nl}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/9zSTh51c9VJ7oXGGjSWQZySP5" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/9zSTh51c9VJ7oXGGjSWQZySP5.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/9zSTh51c9VJ7oXGGjSWQZySP5.js" id="asciicast-9zSTh51c9VJ7oXGGjSWQZySP5" async></script>


## Transform code with NL {#transform-code-with-nl}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/nkbVnEG7QWKyi7ZjyvswpDwvX" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/nkbVnEG7QWKyi7ZjyvswpDwvX.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/nkbVnEG7QWKyi7ZjyvswpDwvX.js" id="asciicast-nkbVnEG7QWKyi7ZjyvswpDwvX" async></script>


### Remove the `do` block and make into one line {#remove-the-do-block-and-make-into-one-line}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/e3ej4K9H0d7lRzQsthLeCWUF1" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/e3ej4K9H0d7lRzQsthLeCWUF1.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/e3ej4K9H0d7lRzQsthLeCWUF1.js" id="asciicast-e3ej4K9H0d7lRzQsthLeCWUF1" async></script>


## Working `main` {#working-main}

{{< highlight haskell "linenos=table, linenostart=1" >}}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process ( readProcess )
import Data.Aeson ( decode )
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe ( fromJust, fromMaybe )

pickUpLine :: String -> IO String
pickUpLine = readProcess "/home/shane/scripts/myeval" ["pena", "very-witty-pick-up-lines-for-a-topic/1"]

decodeResultsList :: String -> Maybe [String]
decodeResultsList results = Data.Aeson.decode (BLU.fromString (Prelude.take (Prelude.length results - 1) results :: String)) :: Maybe [String]

getResults :: String -> IO (Maybe [String])
getResults product = do
  results <- pickUpLine product
  return (decodeResultsList results)

main :: IO ()
main = do
  output <- getResults "Weather"
  print $ unlines $ fromMaybe [] output
{{< /highlight >}}