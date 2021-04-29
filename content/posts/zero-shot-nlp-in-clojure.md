+++
title = "Zero-shot NLP in Clojure"
author = ["Shane Mulligan"]
date = 2021-04-30T00:00:00+12:00
keywords = ["huggingface", "nlp", "clojure", "python"]
draft = false
+++

Original article
: <http://gigasquidsoftware.com/blog/2021/03/15/breakfast-with-zero-shot-nlp/>


Code
: <http://github.com/mullikine/zero-shot-nlp>

<!--listend-->

{{< highlight clojure "linenos=table, linenostart=1" >}}
(ns zero-shot-nlp.core
  (:gen-class)
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :as py :refer [py. py.. py.-]]))

(require-python '[transformers :bind-ns])

(require '[clojure.pprint :as p])

(use '[clojure.java.shell :only [sh]])

;; (in-ns 'clojure.pprint)

(defn -main
  "I classify stuff."
  [& args]
  ;; import transformers; transformers.pipeline("zero-shot-classification")
  (def classifier (py. transformers "pipeline" "zero-shot-classification"))

  (def text "French Toast with egg and bacon in the center with maple syrup on top. Sprinkle with powdered sugar if desired.")

  (def labels ["breakfast" "lunch" "dinner"])

  (let [s (classifier text labels)
        j (clojure.data.json/write-str s)]
    (sh "sh" "-c" "jq . | tv" :in j)))
{{< /highlight >}}

In python, the following syntax is used to get a classifier.

{{< highlight python "linenos=table, linenostart=1" >}}
import transformers; transformers.pipeline("zero-shot-classification")
{{< /highlight >}}

In clojure, this is how it's done with the interop.

{{< highlight clojure "linenos=table, linenostart=1" >}}
(py. transformers "pipeline" "zero-shot-classification")
{{< /highlight >}}

{{< highlight json "linenos=table, linenostart=1" >}}
{
  "sequence": "French Toast with egg and bacon in the center with maple syrup on top. Sprinkle with powdered sugar if desired.",
  "labels": [
    "breakfast",
    "lunch",
    "dinner"
  ],
  "scores": [
    0.989736795425415,
    0.007010197266936302,
    0.003252969356253743
  ]
}
{{< /highlight >}}

Breakfast wins.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/svXhlcT8OyXAn7KCFr1zUILYr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/svXhlcT8OyXAn7KCFr1zUILYr.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/svXhlcT8OyXAn7KCFr1zUILYr.js" id="asciicast-svXhlcT8OyXAn7KCFr1zUILYr" async></script>