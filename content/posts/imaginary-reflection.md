+++
title = "Imaginary reflection"
author = ["Shane Mulligan"]
date = 2021-10-04T00:00:00+13:00
keywords = ["codex", "openai", "gpt", "lm", "nlp"]
draft = false
+++

## Summary {#summary}

I try to demonstrate _imaginary reflection_.

Imaginary programming glossary
: <https://github.com/semiosis/glossaries-gh/blob/master/imaginary-programming.txt>


### Definitions {#definitions}

{{< highlight text "linenos=table, linenostart=1" >}}
imaginary reflection
    This is where a program such as a complex
    interpreter can access knowledge of its
    holographic representation via a language
    model.

holographic representation
    The embedding of the way software is used
    within a language model that has been
    trained on the usage of that software,
    whether it is usage of the final program
    or usage of its internal source code
    within itself.

    THe holographic representation of software
    encompasses the way a program's source
    code is used both within itself and also
    in the real world as modelled by a
    language model.

complex interpreter
    This is a real/ordinary interpreter with
    an interactivity layer that solicits a
    language model for imaginary reflection
    capabilities to the real program.
{{< /highlight >}}


## Internal imaginary reflection {#internal-imaginary-reflection}

This is reflection but where the program
consults a LM for information on its own
source.

This can be used to:

-   optimise a software by statically or dynamically prioritising functions which are used more often
-   generating test cases for functions that the libary provides


### Generate test functions {#generate-test-functions}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "create a test case for a function, given the repository url"
doc: "Given a function name and a repository URL, generate a test case"
todo:
- Make this work for more languages
prompt-version: 1
prompt: |+
  git clone "<url>"
  cd `(insert (pen-snc "sed 's/.*github.com\\/[^/]*\\///'" "<url>"))`
  cd tests
  # Functions for testing <function name>
  cat test_<function name>.py <<EOD

engine: "OpenAI Codex"
temperature: 0.3

# This works, but the approximation of the max bound sometimes isn't good enough
# max-generated-tokens: "final-engine-max-generated-tokens"

# max-generated-tokens: "(- final-engine-max-generated-tokens 200)"

# Only start it off
max-generated-tokens: 150

top-p: 1.0
n-completions: 5
stop-sequences:
- "EOD"
cache: on
yas: on
vars:
- "url"
- "function name"
examples:
- "https://github.com/dbrattli/OSlash"
- "monad"
filter: off
completion: off
insertion: on
{{< /highlight >}}

Real source code
: <https://github.com/dbrattli/OSlash/blob/master/tests/test%5Fmonad.py>

A fictional/generated unit test:

{{< highlight yaml "linenos=table, linenostart=1" >}}
import unittest
import oslash

# Monads

class TestMonads(unittest.TestCase):

    def test_reader_monad(self):
        def ask(name):
            return ("Hello, %s" % name, lambda: "Pleased to meet you, %s." % name)

        # Reader Monad
        self.assertEqual(oslash.Reader.unit("World"), (lambda r: ("Hello, %s" % r, None)), "Reader monad unit failed")
        self.assertEqual(oslash.Reader.curry(ask, "World"), (lambda r: (lambda _: ("
{{< /highlight >}}

The assumption with the generated test cases
is that the LM should be capable of catering
generated test cases to an exact repository.

The resulting test isn't accurate enough to
make that claim, but perhaps the technique is
still valuable for inspiration in writing new
test cases, even if the test cases will not
run as generated.

The demo of prompting the above test case:

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/lhVDv6T1j9Q8NVb8XFyuP4CsT" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/lhVDv6T1j9Q8NVb8XFyuP4CsT.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/lhVDv6T1j9Q8NVb8XFyuP4CsT.js" id="asciicast-lhVDv6T1j9Q8NVb8XFyuP4CsT" async></script>


## External imaginary reflection {#external-imaginary-reflection}

This is reflection but where the program
consults a LM for information on the way the
program is used in the real world.

-   This can be used to:
    -   optimise a software by statically or
        dynamically prioritising functions which are
        used more often.