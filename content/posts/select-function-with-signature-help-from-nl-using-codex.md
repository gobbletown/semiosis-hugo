+++
title = "Select function (with signature help) from NL using Codex"
author = ["Shane Mulligan"]
date = 2021-08-31T00:00:00+12:00
keywords = ["gpt", "openai", "codex", "pen"]
draft = false
+++

## Summary {#summary}

I create a handy function to suggest functions along with signature help, using Codex.


## elisp {#elisp}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-select-function-from-nl (use-case)
  (interactive (list (read-string "pen-select-function-from-nl use-case: ")))
  (let* ((lang ;; (pen-detect-language-ask)
          (read-string-hist "pen-select-function-from-nl lang: "))
         (funs (pf-find-a-function-given-a-use-case/2 lang use-case :no-select-result t))
         (sigs (pf-get-the-signatures-for-a-list-of-functions/2 lang (list2str funs) :no-select-result t)))
    (xc (fz (-zip-lists funs sigs) nil nil "pen-select-function-from-nl: "))))
{{< /highlight >}}


## Demos {#demos}


### Concurrency with ipython and codex {#concurrency-with-ipython-and-codex}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/RQTehM9JsRnlQFBqdjbeiSi0T" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/RQTehM9JsRnlQFBqdjbeiSi0T.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/RQTehM9JsRnlQFBqdjbeiSi0T.js" id="asciicast-RQTehM9JsRnlQFBqdjbeiSi0T" async></script>


### Bash process management {#bash-process-management}

{{< highlight text "linenos=table, linenostart=1" >}}
kill # kill PID
ps # ps -ef
wait # wait $PID
exec # exec $SHELL
trap # trap 'commands' SIGINT
bg # bg %1
fg # fg %1
disown # disown %1
jobs # jobs
killall # killall command
killall5 # killall5 -9 command
pkill # pkill -9 command
{{< /highlight >}}


### Haskell process management {#haskell-process-management}

{{< highlight text "linenos=table, linenostart=1" >}}
forkIO # forkIO :: IO ()
kill # kill :: ProcessId -> IO ()
killProcess # killProcess :: ProcessId -> IO ()
killThread # killThread :: ThreadId -> IO ()
nice # nice :: Int -> IO ()
openProcess # openProcess :: ProcessId -> IO ()
openProcessIO # openProcessIO :: ProcessId -> IO (ProcessIO)
openProcessPID # openProcessPID :: ProcessId -> IO (ProcessId)
{{< /highlight >}}

While in the interpreter:

_lenses and prisms_

{{< highlight text "linenos=table, linenostart=1" >}}
Prism # prism :: Prism s a
Lens # lens :: Lens s a
Iso # iso :: Iso s a
Traversal # traversal :: Traversal s a
Getter # getter :: Getter s a
Setter # setter :: Setter s a
Review # review :: Review s a
Lenses # lenses :: Lenses s a
PPrism # pPrism :: PPrism s a
{{< /highlight >}}

_parsec_

{{< highlight text "linenos=table, linenostart=1" >}}
parsec.string # parsec.string "a"
parsec.letter # parsec.letter
parsec.digit # parsec.digit
parsec.space # parsec.space
parsec.eof # parsec.eof
parsec.many # parsec.many
{{< /highlight >}}


### Ask the tutor some questions {#ask-the-tutor-some-questions}

_How do I parse a date?_

An aside
: With Davinci the topic+subtopic tutor does well, but not with Codex.

You can parse a date using the format
function, but you should not use the default
RecordParser, you need to use the FieldParser
When given a format string, such as 'yyyy-MM-
dd' (year, month, day), you should use the
'parseDateFromString' function.

And you can read about it here: <http://www.has>
kell.org/haskellwiki/Parsec#Format\_strings ##
So to parse "2010-01-10" you would use:
_What is a monad?_

Monads are a type of algebraic structure used
to create a category of actions, where each
action is an object in the category.

Monads are a very useful concept in functional
programming.

A monad is a functor in the category of
endofunctors, or a category in which every
endofunctor is a functor.

Monads also allow for the abstraction of the
"state" of a program at a given time.

Different variants of monads include
conceptually "state" monads, continuation
monads, Reader monads, Writer monads, and
various others.


### Ask the tutor abiout python {#ask-the-tutor-abiout-python}

_How do I manage a pool of threads?_

{{< highlight text "linenos=table, linenostart=1" >}}
Well, there are a few ways to manage a pool
of threads.

I can explain them to you.

AI: The first way to manage a pool of threads
is to use a ThreadPoolExecutor.

This is a special kind of an ExecutorService
that is meant to be used for executing threads
in a pool.

You can configure a ThreadPoolExecutor with a
corePoolSize and a maximumPoolSize.

The corePoolSize is the thread pool size that
is kept even if the pool is empty and the
queue is full.

The maximumPoolSize is the maximum number of
threads that can be in the pool.

The initialPoolSize is the number of threads
that are created when the thread pool is
created.

If the initialPoolSize is greater than zero,
then that many threads are created.

If the initialPoolSize is zero, the number of
threads is the sum of the values of the
corePoolSize and the maximumPoolSize.

The keepAliveTime is the amount of time that
threads in the pool stay alive if there are no
tasks to do (or time-out).

The maxQueueSize is the max number of tasks
that can wait in the queue.

The constantPoolSize is an upper bound on the
number of threads that will ever be in the
pool.

If there are more threads in the pool than the
constantPoolSize, then the pool has a larger
constantPoolSize.
{{< /highlight >}}