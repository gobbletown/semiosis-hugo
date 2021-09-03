+++
title = "Designing an imaginary programming library iλ for emacs"
author = ["Shane Mulligan"]
date = 2021-09-03T00:00:00+12:00
keywords = ["emacs", "openai", "pen", "gpt", "imaginary-programming"]
draft = false
+++

## Summary {#summary}

I design an imaginary programming (`IP`) library
(`iλ.el`) for emacs. Think of it a bit like a
functional programming library in that you
will find a set of functions and macros for
working in the programming paradigm. The
objective here is to create some functions for
doing IP in emacs lisp, since emacs lisp has
the expressive power to prototype such things,
but the ideas contained here can easily be
transferred to any other programming language.
The results of this little experiment will
straight into my thesis.

IP thesis
: <https://github.com/semiosis/imaginary-programming-thesis/blob/master/thesis.org>

IP libary
: <http://github.com/semiosis/pen.el/blob/master/src/pen-imaginary-library.el>

IP glossary
: <https://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>


### The objective with `iλ.el` {#the-objective-with-iλ-dot-el}

The objective here is to create `IP` functions
for programming in emacs lisp exclusively.

It will be extended in the future to do all
programming languages, but I intend to make
`iλ.el` simple and effective for programming
in emacs lisp without bloat or over-complication.


## Syntax forms {#syntax-forms}

| name             | basic idea                                                                                                     |
|------------------|----------------------------------------------------------------------------------------------------------------|
| `idefun`         | Run an expression on the given arguments and return an imagined result, but create a binding for the function. |
| `imacro`         | `imacro` does not evaluate. It merely generates code, but is like `idefun`.                                    |
| `ilist`          | Generate a list of things. Return a real list.                                                                 |
| `ilambda` / `iλ` | Imaginarily run an expression on the given arguments and return an imagined result.                            |
| `ifilter`        | Imaginarily filter a real list with natural language and return a real list. Optionally, enforce cardinality.  |
| `ieval`          | `ieval` will imagine the evaluation of some code without any other context.                                    |


### `idefun` {#idefun}

The `idefun` is mostly simply a call to a
function without necessarily specifying its
implementation. The LM will then imagine the
evaluation of the function.


### `imacro` {#imacro}

Components of the `imacro` should be inferred.
An `imacro` with only a function name should
work.

Also, an `imacro` is under the hood a regular
macro. This means, that expanding the `imacro`
will infer/generate underlying code.


### `ilist` {#ilist}

The easiest of the list of syntax forms I
aimed to implement, `ilist` simply takes a the
number of items to generate (`n`) and a string
describing the type of thing to generate
(`type-of-thing`). It will return a real list
of such things.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun ilist (n type-of-thing)
  (interactive (list (read-string-hist "ilist n: ")
                     (read-string-hist "ilist type-of-thing: ")))
  (pen-single-generation (pf-list-of/2 (str n) (str type-of-thing) :no-select-result t)))

(defun test-ilist ()
  (interactive)
  (etv (pps (ilist 10 "tennis players"))))
{{< /highlight >}}


### `imacro` {#imacro}


### `ilambda` {#ilambda}

Imaginarily run an expression on the given
arguments and return an imagined result.


### `ifilter` {#ifilter}

Example:

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pps (ifilter (ilist 10 "tennis players") "is male"))
{{< /highlight >}}


### Derived functions {#derived-functions}

-   _get-backstory_


### `ieval` {#ieval}

`ieval` will simply evaluate the provided
string as emacs lisp code.

{{< highlight bash "linenos=table, linenostart=1" >}}
(ieval "(+ 2 2)")
{{< /highlight >}}

`ieval` is used by `idefun` and `ilambda`.


### `itransform` {#itransform}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun get-backstory ()

  )
(itransform)
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pps (mapcar 'get-backstory (ilist 10 "tennis players"))
{{< /highlight >}}