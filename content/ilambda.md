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

`imacro` (like regular macros) are for
generating code. `idefun`, however, doesnt
generate the code but attempts to evaluate a
function _without_ code, or **if** code is supplied
then _imagine_ evaluating that code rather than
actually evaluating it.

It's kinda crazy that `Im`-macros run faster
and are more reliable than `Im`-functions,
where the opposite is true in regular
programming. That's because they generate the
code which can be <span class="underline">reused</span>, adjusted and
interactively regenerated, where an
`Im`-function will typically have to query the
LM every time you call it.


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
| `imacro/N`       | MACRO    |                      | `imacro` does not evaluate. It merely generates code, but that code is imagined. Like `idefun` it is variadic.        |
| `idefun`         | FUNCTION | `ieval` and `imacro` | Run an imagined function on the given arguments and return an imagined result, but create a binding for the function. |
| `ilist`          | FUNCTION |                      | Generate a list of things. Return a real list.                                                                        |
| `ilambda` / `iλ` | FUNCTION | `ieval`              | Imaginarily run an expression on the given arguments and return an imagined result.                                   |
| `ifilter`        | FUNCTION |                      | Imaginarily filter a real list with natural language and return a real list. Optionally, enforce cardinality.         |
| `iparse`         | MACRO    |                      | Given a syntax form / expression, will parse a syntax form with natural language. Returns the subform.                |
| `defimacro`      | MACRO    | `imacro/N`           | Select the appropriate `imacro/N` form depending on the arity of arguments.                                           |


#### `ieval` {#ieval}

`ieval` will simply evaluate the provided
string/sexp as emacs lisp code. You
must provide `ieval` with, firstly, the preceding
code, which may be, for example, a function
definition or package requires, etc. and,
secondly, evaluated expression. Either
argument can either be a raw string containing
code or a sexp, but the expression will be
"one-line-ized" for the prompt.

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

You may pass either a `sexp` or a raw string containing code.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro ieval (expression &optional code)
  "Imaginarily evaluate the expression, given the code and return a real result."
  (let* ((code-str
          (cond
           ((stringp code) code)
           ((listp code) (pps code))))
         (expression-str
          (cond
           ((stringp expression) expression)
           ((listp expression) (pp-oneline expression))))
         (result (car
                  (pen-single-generation
                   (pf-imagine-evaluating-emacs-lisp/2
                    code-str expression-str
                    :no-select-result t :select-only-match t)))))
    (ignore-errors
      (eval-string result))))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun test-ieval-2 ()
  (ieval
   (double-number 5)
   "(defun double-number (x)\n     (x * x))\n;;Test the double-number function"))
{{< /highlight >}}

Resulting prompt from `test-ieval-2`:

{{< highlight text "linenos=table, linenostart=1" >}}
(defun double-number (x)
     (x * x))
;;Test the double-number function
(message (eval (double-number 5)))
--> <END>
{{< /highlight >}}

`ieval` not only evaluates correctly despite
the deliberately incorrect naming of the
function (it multiplies rather than doubles),
but it returns the value as the correct data type.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun test-ieval ()
  (ieval
   (double-number 5)
   (defun double-number (x)
     (x * x))))
{{< /highlight >}}

Expansion of `test-ieval`.

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


#### `ilambda` / `iλ` {#ilambda-iλ}

Imaginarily run an expression on the given
arguments and return an imagined result.

Here are three `ilambda` subforms which take different arguments.

`ilambda/task` is the most terse. Only a NL
task description is given.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro ilambda/task (args task)
  (let* ((slug (slugify (eval task)))
         (fsym (intern slug)))
    `(lambda ,args
       (let ((vals (mapcar 'eval ',args)))
         (eval
          ;; imagined by an LM
          `(ieval
            ;; An function and a function call
            (,',fsym ,@vals)
            ,,(concat ";; " task)))))))

(defmacro ilambda/task-code (args task code)
  (let* ((slug (slugify (eval task)))
         (fsym (intern slug)))
    `(lambda ,args
       (let ((vals (mapcar 'eval ',args)))
         (eval
          ;; imagined by an LM
          `(ieval
            ;; An function and a function call
            (,',fsym ,@vals)
            (defun ,',fsym ,',args
              ,,task
              ,',code)))))))

(defmacro ilambda/code (args code)
  `(lambda ,args
     (let ((vals (mapcar 'eval ',args)))
       (eval
        ;; imagined by an LM
        `(ieval
          ;; An function and a function call
          (main ,@vals)
          (defun main (,',@args)
            ,',code))))))
{{< /highlight >}}

_**Demonstrations**_

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(mapcar (ilambda/task (x) "double it")
        '(12 4))
{{< /highlight >}}

```emacs-lisp
"(24 8)
"
```

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(mapcar (ilambda/code (x)
                      (+ x 5))
        '(4))
{{< /highlight >}}

```emacs-lisp
"(9)
"
```

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(mapcar (ilambda/task-code (x)
                           "add five"
                           (+ x 5))
        '(8))
{{< /highlight >}}

```emacs-lisp
"(13)
"
```

The _**ilambda**_ macro.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro ilambda (args code-or-task &optional task-or-code)
  "define ilambda"
  (let ((task (if (stringp code-or-task)
                  code-or-task
                task-or-code))
        (code (if (listp code-or-task)
                  code-or-task
                task-or-code)))
    (cond
     ((and code
           (sor task))
      `(ilambda/task-code ,args ,task ,code))
     ((sor task)
      `(ilambda/task ,args ,task))
     ((listp code-or-task)
      `(ilambda/code ,args ,code)))))

(defalias 'iλ 'ilambda)
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(-reduce (iλ (x y) "add x to y") (number-sequence 1 3))
{{< /highlight >}}

```emacs-lisp
"6"
```


#### `idefun` {#idefun}

The `idefun` creates a binding to an imaginary
function. The implementation of the `idefun`
need not be specified in order for code to
run.

The new prompt function returned by `idefun` is provided with arguments and the
values of those arguments are taken and placed
into a prompt. An implementation may be
provided to `idefun` when defining the prompt function or optionally left out.
Unlike an `imacro`, when the prompt function
is evaluated the code is not returned. Rather,
the code is evaluted in imaginary space.

In short, the LM will imagine the evaluation
of the function as opposed to generate code.

`idefun` returns a binding to a new prompt
function.


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

`defimacro`

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defmacro defimacro (name &rest body)
  "Define imacro"
  (cond
   ((= 0 (length body))
    `(imacro/1
      ,name))
   ((= 1 (length body))
    `(imacro/2
      ,name
      ,(car body)))
   ((= 2 (length body))
    `(imacro/3
      ,name
      ,(car body)
      ,(cadr body)))))
{{< /highlight >}}

All of the following are valid ways to invoke `defimacro`.

`defimacro` selects the right `imacro/N` function depending on the arity of the arguments.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defimacro my/subtract)
(defimacro my/subtract (a b c))
(defimacro my/itimes (a b c)
   "multiply three complex numbers")
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/19czBa4Qyncgtg1JFi5JQLmfi" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/19czBa4Qyncgtg1JFi5JQLmfi.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/19czBa4Qyncgtg1JFi5JQLmfi.js" id="asciicast-19czBa4Qyncgtg1JFi5JQLmfi" async></script>


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