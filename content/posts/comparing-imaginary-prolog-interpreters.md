+++
title = "Comparing imaginary Prolog interpreters"
author = ["Shane Mulligan"]
date = 2021-08-23T00:00:00+12:00
keywords = ["gpt", "pen", "ii"]
draft = false
+++

## Summary {#summary}

I compare 3 different imaginary interpreters
for Prolog with the GPT-3 language model.

The winner definitely goes to gprolog as the
most reliable of the prolog imaginary
interpreters.


## Imaginary gprolog in GPT-3 {#imaginary-gprolog-in-gpt-3}

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-db-interpreter-2.prompt>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/baxTgPI3Jjh6Y0e0eVBLbNdwS" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/baxTgPI3Jjh6Y0e0eVBLbNdwS.svg" /></a>-->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/baxTgPI3Jjh6Y0e0eVBLbNdwS.js" id="asciicast-baxTgPI3Jjh6Y0e0eVBLbNdwS" async></script>

The imagined gprolog interpreter, in this
instance, performed quite well. It was fairly
accurate in querying this make-believe
database.

{{< highlight yaml "linenos=table, linenostart=1" >}}
include: Generic Interpreter/3
task: Imagine a <language> interpreter
language: prolog
subprompts:
- kickstarter: |+
    Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.4)
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
    Please run ?- license. for legal details.

    For online help and background, visit https://www.swi-prolog.org
    For built-in help, use ?- help(Topic). or ?- apropos(Word).

    ?-
prompt: |+
    <history><expression>
# Double newline is a better default stop sequence
# Because 200 tokens will take a long time to generate.
stop-sequences:
- "\n\n"
- "?- "
vars:
- history
- expression
var-defaults:
- kickstarter
max-tokens: 200
# sed wont even run if its input is empty so
# the postpostprocessor: cant ensure a prompt.
# But we cant force it.
postpostprocessor: pen-str newline-if-empty | sed -z 's/\(?-\)\?$/\n?- /'
examples:
- "?- "
- "write('Hello World')."
n-completions: 10
{{< /highlight >}}


## Imaginary swipl in GPT-3 {#imaginary-swipl-in-gpt-3}

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-prolog-db-swipl-interpreter-2.prompt>

<!-- Play on asciinema.com -->
<!--<a title="asciinema recording" href="https://asciinema.org/a/1fBGRViY1KaBLsneJ2sbGyMr8" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/1fBGRViY1KaBLsneJ2sbGyMr8.svg" /></a>-->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/1fBGRViY1KaBLsneJ2sbGyMr8.js" id="asciicast-1fBGRViY1KaBLsneJ2sbGyMr8" async></script>

On the several times in which I started the
imaginary swipl interpreter, I have found it
less reliable than the gprolog one.

I have had a thought and that I would like to
be able to pick from many different
generations in the imaginary interpreter.

That will come when I add `comint` support.

{{< highlight yaml "linenos=table, linenostart=1" >}}
include: Generic Interpreter/3
task: Imagine a <language>-db-swipl interpreter
language: prolog
subprompts:
- kickstarter: |+
    $ cat > db.pl <<HEREDOC
    food(burger).
    food(sandwich).
    food(pizza).
    lunch(sandwich).
    dinner(pizza).
    meal(X) :- food(X).
    HEREDOC
    $ swipl
    Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.4)
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
    Please run ?- license. for legal details.

    For online help and background, visit https://www.swi-prolog.org
    For built-in help, use ?- help(Topic). or ?- apropos(Word).

    ?- consult(db).
    true.

    ?-
prompt: |+
    <history><expression>
# Double newline is a better default stop sequence
# Because 200 tokens will take a long time to generate.
stop-sequences:
- "\n\n"
- "?- "
vars:
- history
- expression
var-defaults:
- kickstarter
max-tokens: 200
# sed wont even run if its input is empty so
# the postpostprocessor: cant ensure a prompt.
# But we cant force it.
postpostprocessor: pen-str newline-if-empty | sed -z 's/\(?-\)\?$/\n?- /'
examples:
- "?- "
- "write('Hello World')."
n-completions: 10
{{< /highlight >}}


## Imaginary prolog pseudocode REPL in GPT-3 {#imaginary-prolog-pseudocode-repl-in-gpt-3}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/7ta7D1FAmI3S6mlrWay9fHpWj" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/7ta7D1FAmI3S6mlrWay9fHpWj.svg" /></a>-->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/7ta7D1FAmI3S6mlrWay9fHpWj.js" id="asciicast-7ta7D1FAmI3S6mlrWay9fHpWj" async></script>

The imaginary pseudocode REPL performed terribly.

{{< highlight yaml "linenos=table, linenostart=1" >}}
include: Generic Interpreter/3
task: Imagine a <language>-pseudocode interpreter
language: prolog
subprompts:
- kickstarter: |+
    % prolog
    food(burger).
    food(sandwich).
    food(pizza).
    lunch(sandwich).
    dinner(pizza).
    meal(X) :- food(X).
    ?- consult(db).
    true.
    ?-
prompt: |+
    <history><expression>
# Double newline is a better default stop sequence
# Because 200 tokens will take a long time to generate.
stop-sequences:
- "\n\n"
- "?- "
vars:
- history
- expression
var-defaults:
- kickstarter
max-tokens: 200
# sed wont even run if its input is empty so
# the postpostprocessor: cant ensure a prompt.
# But we cant force it.
postpostprocessor: pen-str newline-if-empty | sed -z 's/\(?-\)\?$/\n?- /'
examples:
- "?- "
- "write('Hello World')."
n-completions: 10
{{< /highlight >}}