+++
title = "Translating Haskell to Clojure with GPT-3"
author = ["Shane Mulligan"]
date = 2021-03-07T00:00:00+13:00
keywords = ["GPT-3", "openai", "emacs"]
draft = false
+++

Relevant material
: <https://hyperpolyglot.org/ml>


## Summary {#summary}

Who needs `hyperpolyglot` when you have `GPT-3`?

I translate Haskell into Clojure using the following prompt.


### `haskell-to-clojure.prompt` {#haskell-to-clojure-dot-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "Translate Haskell to Clojure"
prompt: |+
    Haskell:
    zip (map show [1,5,9]) ["a","b","c"]
    Clojure:
    (println (map vector '(1 2 3) '(4 5 6)))

    Haskell:
    map toUpper "MiXeD cAsE"
    Clojure:
    (clojure.string/upper-case "MiXeD cAsE")

    Haskell:
    putStrLn "Hello World"
    Clojure:
    (println "Hello World")

    Haskell:
    import Data.Time.Clock
    import Data.Time.Calendar
    date :: IO (Integer, Int, Int) -- :: (year, month, day)
    date = getCurrentTime >>= return . toGregorian . utctDay
    Clojure:
    (defn date [] (.toString (java.util.Date.)))

    Haskell:
    <1>
    Clojure:
engine: "davinci"
temperature: 0.3
max-tokens: 60
top-p: 1.0
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "\n\n"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "haskell code"
examples:
- "min 1 2"
external: ""
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
{{< /highlight >}}


## Demonstrations {#demonstrations}

<a title="asciinema recording" href="https://asciinema.org/a/64hjDdeeqkhTEzZA9RvYjKyy9" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/64hjDdeeqkhTEzZA9RvYjKyy9.svg" /></a>

<a title="asciinema recording" href="https://asciinema.org/a/UknQeVwRejokdRKJEYa7KdNKk" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/UknQeVwRejokdRKJEYa7KdNKk.svg" /></a>


## Translate Haskell into Clojure {#translate-haskell-into-clojure}

<span class="underline">**Haskell**</span>

{{< highlight haskell "linenos=table, linenostart=1" >}}
div 7 3
{{< /highlight >}}

<span class="underline">**Clojure**</span>

{{< highlight clojure "linenos=table, linenostart=1" >}}
(/ 7 3)
{{< /highlight >}}


## It's not perfect {#it-s-not-perfect}

<span class="underline">**Haskell**</span>

{{< highlight haskell "linenos=table, linenostart=1" >}}
z = x * y
  where x = 3.0
        y = 2.0 * x
{{< /highlight >}}

<span class="underline">**Clojure**</span>

{{< highlight clojure "linenos=table, linenostart=1" >}}
(defn z [x y] (* y x))
{{< /highlight >}}


## But it's **pretty good** {#but-it-s-pretty-good}

<span class="underline">**Haskell**</span>

{{< highlight haskell "linenos=table, linenostart=1" >}}
let absn = if n < 0 then -n else n
{{< /highlight >}}

<span class="underline">**Clojure**</span>

{{< highlight clojure "linenos=table, linenostart=1" >}}
(defn absn [n] (if (negative? n) (- n) n))
{{< /highlight >}}