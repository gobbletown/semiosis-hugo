+++
title = "iλ, an imaginary programming library for emacs"
author = ["Shane Mulligan"]
date = 2021-09-03T00:00:00+12:00
keywords = ["emacs", "openai", "pen", "gpt", "imaginary-programming"]
draft = false
+++

## `iλ` (Imaginary Programming Functions) {#iλ--imaginary-programming-functions}


### Summary {#summary}

I design and build an imaginary programming (`IP`) library
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

iλ project
: <http://github.com/semiosis/ilambda/>

IP thesis
: <https://github.com/semiosis/imaginary-programming-thesis/blob/master/thesis.org>

IP library
: <http://github.com/semiosis/pen.el/blob/master/src/pen-ilambda.el>

IP glossary
: <https://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>


#### The objective with `iλ.el` {#the-objective-with-iλ-dot-el}

The objective here is to create `IP` functions
for programming in emacs lisp exclusively.

It will be extended in the future to do all
programming languages, but I intend to make
`iλ.el` simple and effective for programming
in emacs lisp without bloat or over-complication.


### Syntax forms {#syntax-forms}

| name             | type     | depends on           | basic idea                                                                                                            |
|------------------|----------|----------------------|-----------------------------------------------------------------------------------------------------------------------|
| `ieval`          | MACRO    |                      | `ieval` will imagine the evaluation of some code without any other context.                                           |
| `imacro`         | MACRO    |                      | `imacro` does not evaluate. It merely generates code, but is like `idefun`.                                           |
| `idefun`         | FUNCTION | `ieval` and `imacro` | Run an imagined function on the given arguments and return an imagined result, but create a binding for the function. |
| `ilist`          | FUNCTION |                      | Generate a list of things. Return a real list.                                                                        |
| `ilambda` / `iλ` | FUNCTION |                      | Imaginarily run an expression on the given arguments and return an imagined result.                                   |
| `ifilter`        | FUNCTION |                      | Imaginarily filter a real list with natural language and return a real list. Optionally, enforce cardinality.         |
| `iparse`         | MACRO    |                      | Given a syntax form / expression, will parse a syntax form with natural language. Returns the subform.                |


#### `ieval` {#ieval}

`ieval` will simply evaluate the provided
string as emacs lisp code.

`ieval` is used by `idefun` and `ilambda`.

<span class="underline">_prompt function_</span> for running the eval.

`pf-imagine-evaluating-emacs-lisp/2`
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-evaluating-emacs-lisp-2.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "imagine evaluating emacs lisp"
doc: "Given some elisp return the imagined result"
prompt-version: 1
prompt: |+
  <code>
  (message (eval <expression>))
  -->
engine: "OpenAI Codex"
temperature: 0.2
max-generated-tokens: 60
top-p: 1.0
cache: on
stop-sequences:
- "\n"
vars:
- "code"
- "expression"
validator: "grep -qv '(:return'"
examples:
- |-
    (defun double-number (x)
      (x * x))
- "(double-number 5)"
filter: on
completion: off
insertion: off
{{< /highlight >}}

The following is the implementation of `ieval`.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro ieval (expression &optional code)
  (let* ((code-str (pps code))
         (result (car
                  (pen-single-generation
                   (pf-imagine-evaluating-emacs-lisp/2
                    code-str expression
                    :no-select-result t :select-only-match t)))))
    (eval-string result)))

(defun test-ieval ()
  (ieval
   (double-number 5)
   (defun double-number (x)
     (x * x))))
{{< /highlight >}}

`ieval` not only evaluates correctly despite
the deliberately incorrect naming of the
function (it multiplies rather than doubles),
but it returns the value as the correct data type.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(let ((result
       (ieval
        (defun double-number (x)
          (x * x))
        (double-number 5))))
  (list2str (list result
                  (type result))))
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
25
integer
{{< /highlight >}}


#### `idefun` {#idefun}

The `idefun` creates a binding to an imaginary
function. The implementation of the `idefun`
need not be specified in order for code to
run.

function without necessarily specifying its
implementation. The LM will then imagine the
evaluation of the function.


#### `imacro` {#imacro}

An `imacro` actually imagines the
implementation of a function.

Components of the `imacro` should be inferred.
An `imacro` with only a function name should
work.

Also, an `imacro` is under the hood a regular
macro. This means, that expanding the `imacro`
will infer/generate underlying code.

{{< figure src="/ox-hugo/macro-expand-codex.gif" >}}

`pf-imagine-an-emacs-function/3`
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-an-emacs-function-3.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: imagine an emacs function
task: "imagine an emacs lisp function given name, arguments and docstring"
doc: "Given a function name, arguments and docstring, return the imagined body of the function"
prompt-version: 1
prompt: |+
  ;;my-emacs-library.el

  (defun <name> (<arguments>)
    "<docstring>"
engine: "OpenAI Codex"
temperature: 0.2
max-generated-tokens: 1000
top-p: 1.0
cache: on
stop-sequences:
- "\n\n"
vars:
- "name"
- "arguments"
- "docstring"
validator: "chomp | sed -z 's/.*\\(.\\)$/\\1/' | grep -q ')'"
examples:
- "times"
- "x y"
- "multiply two numbers and return a number"
preprocessors:
- "slugify"
postprocessor: chomp
postpostprocessor: "sed -z \"s/^;;my-emacs-library.el\\\\n\\\\n//\""
filter: on
completion: off
insertion: off
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(car
 (pen-single-generation
  (pf-imagine-an-emacs-function/3
   "times"
   "x y"
   "multiply two numbers and return a number"
   :include-prompt t
   :no-select-result t)))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun times (x y)
  "multiply two numbers and return a number"
  (* x y))
{{< /highlight >}}

There are 3 different versions of `imacro`
depending on how many arguments are supplied to
it.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro imacro/3 (name args docstr)
  "Does not evaluate. It merely generates code."
  (let* ((argstr (apply 'cmd (mapcar 'slugify (mapcar 'str args))))
         (bodystr
          (car
           (pen-single-generation
            (pf-imagine-an-emacs-function/3
             name
             argstr
             docstr
             :include-prompt t
             :no-select-result t))))
         (body (eval-string (concat "'" bodystr))))
    `(progn ,body)))

(defmacro imacro/2 (name args)
  "Does not evaluate. It merely generates code."
  (let* ((argstr (apply 'cmd (mapcar 'slugify (mapcar 'str args))))
         (bodystr
          (car
           (pen-single-generation
            (pf-imagine-an-emacs-function/2
             name
             argstr
             :include-prompt t
             :no-select-result t))))
         (body (eval-string (concat "'" bodystr))))
    `(progn ,body)))

(defmacro imacro/1 (name)
  "Does not evaluate. It merely generates code."
  (let* ((bodystr
          (car
           (pen-single-generation
            (pf-imagine-an-emacs-function/1
             name
             :include-prompt t
             :no-select-result t))))
         (body (eval-string (concat "'" bodystr))))
    `(progn ,body)))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(imacro/3 my/itimes (a b c) "multiply three complex numbers")
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(progn
  (defun my-times
      (x y z)
    "multiply three numbers and return a number"
    (* x y z)))
{{< /highlight >}}

_`imacro` expansion demo_

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/TFjZGxMf0zhT59T7U3tO8uwY5" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/TFjZGxMf0zhT59T7U3tO8uwY5.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/TFjZGxMf0zhT59T7U3tO8uwY5.js" id="asciicast-TFjZGxMf0zhT59T7U3tO8uwY5" async></script>

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(imacro/2 my/subtract (a b c))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(progn
  (defun my-subtract
      (a b c)
    "Subtract B from A and return the result."
    (setq result
          (+ a
             (- b c)))
    result))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(imacro/1 my/subtract)
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(progn
  (defun my-subtract
      (a b)
    "Subtract A - B."
    (- a b)))
{{< /highlight >}}


#### `ilist` {#ilist}

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


#### `ilambda` {#ilambda}

Imaginarily run an expression on the given
arguments and return an imagined result.


#### `ifilter` {#ifilter}

Example:

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pps (ifilter (ilist 10 "tennis players") "is male"))
{{< /highlight >}}


#### Derived functions {#derived-functions}

-   _get-backstory_


#### `iparse` {#iparse}


#### `itransform` {#itransform}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun get-backstory ()

  )
(itransform)
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pps (mapcar 'get-backstory (ilist 10 "tennis players"))
{{< /highlight >}}