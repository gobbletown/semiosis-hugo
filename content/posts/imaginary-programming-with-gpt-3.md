+++
title = "Imaginary programming with GPT-3"
author = ["Shane Mulligan"]
date = 2021-04-08T00:00:00+12:00
keywords = ["gpt", "openai", "imaginary-programming"]
draft = false
+++

Code
: <https://github.com/semiosis/pen.el>


Prompts
: <https://github.com/semiosis/prompts/>


Disclaimer
: Please contribute as this is an open source project! It's very hard to find free prompts online currently and that's because everyone is out for themselves. Please support open source. Thank you.


Glossary
: <http://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>


Thesis
: <https://github.com/semiosis/imaginary-programming-thesis/blob/master/thesis.org>


## Summary {#summary}

This is a demonstration of an imaginary
programming environment. There may be nothing
else like it in the world today.

The world needs to get ready for the next
generations of Large LMs, such as `GPT-4`.

Indeed, they will be able to dream up the
entire experience of a textual user interface
by inferring how it changes based on keyboard
input. That is because a `tty` is simple text
stream.

A current generation GPT trained on `tty`
recordings would most certainly probably be
able to do this already, but with `GPT-4` and
future LMs comes convenience in the form of
prompt tuning.

An imaginary programming environment is [one of the supported features](https://github.com/semiosis/pen.el/blob/master/docs/README.org) of `pen.el`.


### What does it mean to be imaginary? {#what-does-it-mean-to-be-imaginary}

Several of the components of a normal
programming environment are replaced by
functions that infer rather than evaluate.

These components are replaced by GPT-3 and Codex prompts.

-   linter, (<https://mullikine.github.io/posts/an-imaginary-linter-with-codex/>)
-   indenter/reformatter (<https://mullikine.github.io/posts/a-syntax-corrector-with-codex/>)
-   code completion (<https://mullikine.github.io/posts/how-to-use-pen-el-to-autocomplete-your-code/>)
-   interpreter (<https://mullikine.github.io/posts/an-emacs-imaginary-interpreter-with-history-completion-and-evaluation/>)
-   function lookup (<https://mullikine.github.io/posts/select-function-with-signature-help-from-nl-using-codex/>)
-   commit message generator (<https://mullikine.github.io/posts/a-git-commit-message-generator-with-codex/>)
-   refactoring (codex playground style) (<https://mullikine.github.io/posts/transforming-prose-and-code-in-a-generic-way/>)
-   recommendation (<https://mullikine.github.io/posts/imaginary-real-codex-complex/>)
-   code generation
    -   functions by NL (<https://mullikine.github.io/posts/language-agnostic-code-generator-in-pen-el/>)
    -   purposeful one liner generation (<https://mullikine.github.io/posts/generating-perl-one-liners-using-gpt-3-and-pen-el/>)

Imaginary seems to be a good word because:

-   GPT-3 is imagining the environment, the code and the output.
-   the languages are kinda like imaginary numbers. They are all understood within the same language model, kinda like a coordinate space.
-   It lets you be very imaginative!


### The implications {#the-implications}

-   There are no interpreters or compilers being used but you can still generate code using words and evaluate code by inference.
-   All requests take the same amount of time to run.
-   All languages have equally facilitated environments.
-   You may program in any language.
-   It runs broken code.
-   One day you may design new languages within the latent space of GPT-3, without doing any programming.
-   You may have an interpreter for languages with no interpreter, such as C++.
-   You may have code completion for scripting languages without a completion engine.
-   You may use it for languages which are dead and an interpreter is not available.
-   It's great for thought experiments.


### Literate programming {#literate-programming}

According to Wikipedia, <span class="underline">literate programming</span>
is a programming paradigm introduced by Donald
Knuth in which a computer program is given an
explanation of its logic in a NL, such as
English, interspersed with snippets of macros
and traditional source code, from which
compilable source code can be generated.

The approach is used in scientific computing
and in data science routinely for reproducible
research and open access purposes.

Literate programming tools are used by
millions of programmers today.

Literate programming
: <https://en.wikipedia.org/wiki/Literate%5Fprogramming>


### Imaginary programming {#imaginary-programming}

Imaginary programming (IP) seems to be
orthogonal to literate programming. Imaginary
programming is a programming paradigm in which
a computer program's behaviour exists in
relativity to LMs. To make an analogy, imaginary programming is a
type of programming, where much like pure-
functional, the code who's behaviour depends
on the output of a LM, either pending or
precomputed, stands apart from the part that
has no such association.

What is deemed _imaginary_ is code that uses
or is waiting on output from a LM and its
behaviour is so altered by it. What is deemed
_ordinary_ is code which is not _imaginary_.

A distinction is made between grounded and
non-grounded imaginary programming. _Non-grounded imaginary programming_ may also be
called _pure imaginary programming_.

If a function has a LM as a parameter
affecting its behaviour and besides from that
has no other imaginary inputs then it is
considered _grounded_. If a function has a LM as a parameter
affecting its behaviour and contains other
imaginary inputs then it is considered
_tethered_'.

If an imaginary function relies on the output
of functions that have in the past used a LM
as a parameter, but does not contain a
reference to specific LM used then it is
considered _non-grounded_ or _pure imaginary_.

A function can be both tethered and pure
imaginary.

Pure imaginary code is still code in a similar
way to how pure functional code is considered
code and may be used to create useful
datastructures and algorithms.

Ordinary programming is programming with
functions that do not have an imaginary
dimension (they do not take a LM as a
parameter and the result of ordinary code is
not polluted by a LM).

`Holographic programming` is like imaginary
programming but where the LMs are trained on
software. Holographic code, therefore, may
employ associations made between elements of
the original code, how that code is used and
how it is described, to build applications.
Holographic programming lets you use the
latent space of a LM as a kind of hyperspace
to enable things like:

-   bridge the usage of an application with
    the application's code
-   imaginary reflection
-   inference in place of computation


### Current progress {#current-progress}

Some of the plumbing, including the
interpreter and the autocompletion system are
in an `MVP` stage.


## Latest GPT-3 Demonstration (Aug 25, 2021) {#latest-gpt-3-demonstration--aug-25-2021}

-   <https://mullikine.github.io/posts/an-emacs-imaginary-interpreter-with-history-completion-and-evaluation/>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/6EKIiUqvOSKetO6Fz439xZitE" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/6EKIiUqvOSKetO6Fz439xZitE.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/6EKIiUqvOSKetO6Fz439xZitE.js" id="asciicast-6EKIiUqvOSKetO6Fz439xZitE" async></script>


## Original Demonstration (April 2, 2021) {#original-demonstration--april-2-2021}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/G8HPLtlCWTQIzGssLrM3ZvxhT" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/G8HPLtlCWTQIzGssLrM3ZvxhT.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/G8HPLtlCWTQIzGssLrM3ZvxhT.js" id="asciicast-G8HPLtlCWTQIzGssLrM3ZvxhT" async></script>


## Prompts and demonstrations {#prompts-and-demonstrations}


### Linter (Update: Sep 1, 2021) {#linter--update-sep-1-2021}

-   <https://mullikine.github.io/posts/an-imaginary-linter-with-codex/>


### Reformatter and Syntax corrector (Update: Sep 1, 2021) {#reformatter-and-syntax-corrector--update-sep-1-2021}

-   <https://mullikine.github.io/posts/a-syntax-corrector-with-codex/>


## Prompts {#prompts}

-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-ammonite-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-emacs-lisp-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-fish-shell-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-haskell-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-javascript-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-julia-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-lua-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-nushell-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-perl-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-powershell-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-db-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-db-swipl-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-pseudocode-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-python-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-ruby-interpreter-2.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-scala-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-scheme-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-smalltalk-interpreter-1.prompt>
-   <http://github.com/semiosis/prompts/blob/master/prompts/imagine-an-awk-linter-1.prompt>


### kickstarter {#kickstarter}

This prompt initiates the imaginary interpreter / REPL.

{{< highlight yaml "linenos=table, linenostart=1" >}}
include: Generic Interpreter/3
task: Imagine a <language> interpreter
language: python
subprompts:
- kickstarter: |+
    Python 3.8.5 (default, Jan 27 2021, 15:41:15)
    Type 'copyright', 'credits' or 'license' for more information
    IPython 7.21.0 -- An enhanced Interactive Python. Type '?' for help.

    In [1]:
prompt: |+
    <history><expression>
    <:pp>Out
user-prompt: "^In \\[[0-9]*\\]: "
# Unfortunately, we can't generate the next In
# prompt because we need to match on it with stop-sequences.
# So the user prompt must be reconstructed manually.
stop-sequences:
- "In ["
# Create a user prompt with the number incremented.
# This is like postprocessor but happens even later.
# It is used in special circumstances when the prompt history is also required.
postpostprocessor: pen-str python-gen-next-user-prompt
vars:
- history
- expression
var-defaults:
- kickstarter
examples:
- "In [1]: "
- "5 + 5"
{{< /highlight >}}


### conjugator {#conjugator}

This prompt is part of the <span class="underline">interpreter conjugator</span>, and enables the <span class="underline">imaginary interpreter</span> to continue the "conversation"
more optimally than the <span class="underline">kickstarter</span> prompt.

Conjugation is comprised of melding and a sliding window.

-   <http://github.com/semiosis/prompts/blob/master/prompts/meld-two-passages-2.prompt>
-   <http://github.com/semiosis/pen.el/blob/master/scripts/pen-sliding-window>


## `iλ`, a family of imaginary programming libraries {#iλ-a-family-of-imaginary-programming-libraries}

-   <https://semiosis.github.io/ilambda/>
-   Interop with language models in your code.

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(ifilter (ilist 10 "tennis players") "is male")
{{< /highlight >}}


## Examplary (a example-oriented programming language) {#examplary--a-example-oriented-programming-language}

-   <https://semiosis.github.io/examplary/>
-   Generate prompts.

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defprompt ("lines of code" regex)
  :task "Convert lines to regex"
  :gen "examplary-edit-generator shane"
  :filter "grex"
  :examples '(("example 1\nexample2")
              ("example 2\nexample3" "^example [23]$")
              ("pi4\npi5" "^pi[45]$" "pi4\npi5"))
  :lm-command "openai-complete.sh")
{{< /highlight >}}