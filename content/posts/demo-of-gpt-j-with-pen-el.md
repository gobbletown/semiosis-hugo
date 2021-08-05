+++
title = "Demo of GPT-J with Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-06T00:00:00+12:00
keywords = ["gpt", "pen", "eleutherai", "aix", "emacs"]
draft = false
+++

## Summary {#summary}

I give a demo of GPT-J in Pen.el.

{{< highlight text "linenos=table, linenostart=1" >}}
Datomic
    NLG: A database that is built on top of a
    distributed graph of data and
    relationships.
{{< /highlight >}}


## Translating languages {#translating-languages}

{{< highlight text "linenos=table, linenostart=1" >}}
distributed graph
    NLG: A graph that is constructed using a
    number of different sized nodes and links.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
arbre distribué
    NLG: Un graphe construit avec
    une grandeur différente de nœuds et de liaisons.
{{< /highlight >}}


### Doesn't seem to handle Japanese too well {#doesn-t-seem-to-handle-japanese-too-well}

But, the AIx API only supports one completion
at a time, for the moment. Usually I could
cherry-pick from a list.

{{< highlight text "linenos=table, linenostart=1" >}}
こんにちは
    NLG: エンコードされたグラフ。
{{< /highlight >}}


## Explaining an error {#explaining-an-error}


### input {#input}

{{< highlight text "linenos=table, linenostart=1" >}}
error: failed to run custom build command for `emacs_module v0.10.0`
{{< /highlight >}}


### output {#output}

{{< highlight text "linenos=table, linenostart=1" >}}
It means that you really wanted to execute the script defined in the emacs_module's custom build.
Please click on the link to see more details about this error and how to fix it.
{{< /highlight >}}


## Generating pick-up lines {#generating-pick-up-lines}

GPT-J has demonstrated its ability to do pick
up lines in many of my demonstrations.


### rugby {#rugby}

{{< highlight text "linenos=table, linenostart=1" >}}
Do you play rugby? I love watching you move.
Do you like rugby? Cuz I like you a snack pack.
{{< /highlight >}}


## Adding comments to code {#adding-comments-to-code}


### A basic test {#a-basic-test}

{{< highlight bash "linenos=table, linenostart=1" >}}
sn="$(basename "$0")"
bn="$(basename "$0")"
{{< /highlight >}}

With flying colours.

{{< highlight bash "linenos=table, linenostart=1" >}}
# Remember the name of the program
sn="$(basename "$0")"
# Save a copy of the original program name
bn="$(basename "$0")"
{{< /highlight >}}


### slightly trickier {#slightly-trickier}

{{< highlight bash "linenos=table, linenostart=1" >}}
if test "$sn" = pena; then
    sn=penf
    all=y
fi
{{< /highlight >}}

With flying colours again.

{{< highlight bash "linenos=table, linenostart=1" >}}
# Check for pena setting
if test "$sn" = pena; then
    # Change the default sn to penf
    sn=penf
    # Set all to y
    all=y
fi
{{< /highlight >}}


### Something harder {#something-harder}

{{< highlight bash "linenos=table, linenostart=1" >}}
case "$sn" in
    penf) {
        if test "$#" -eq 0; then
            sn=penl
            penl_strip_pf=y
        elif test "$#" -eq 1; then
            sn=penh
            penl_strip_pf=y
        fi
    }
    ;;

    *)
esac
{{< /highlight >}}

I think the issue with this is that the prompt
is quite large currently and the AIx API is
not accepting enough tokens.

Currently the AIx API is not respecting the
number of tokens set.

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/annotate-code-with-commentary.prompt>

The entire snippet of code has to go into the
prompt in order for it to work well.

It wasn't fitting.

{{< highlight bash "linenos=table, linenostart=1" >}}
# When the program starts
case "$sn" in
    penf) {
        # If no args were passed in
        if test "$#" -eq 0; then
{{< /highlight >}}

Demo up to this point

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/PzFSAxkjaMwj5BJ52nBGmVBlS;" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/PzFSAxkjaMwj5BJ52nBGmVBlS.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/PzFSAxkjaMwj5BJ52nBGmVBlS.js" id="asciicast-PzFSAxkjaMwj5BJ52nBGmVBlS" async></script>


### Generate fizzbuzz {#generate-fizzbuzz}

{{< highlight js "linenos=table, linenostart=1" >}}
#!/usr/bin/nodejs

// Write a program that prints the numbers
// from 1 to 100.

// But for multiples of three print "Fizz"
// instead of the number and for the
// multiples of five print "Buzz".

// For numbers which are multiples of both
// three and five print "FizzBuzz".

function fizzBuzz(limit) {
    var i, num, str, mod, mod3, mod5;

    for (i = 1; i <= limit; i++) {
        num = i;
        mod = i % 3;
        mod3 = i % 5;
        mod5 = i % 15;

        if (mod === 0) {
            str = "Fizz";
        } else if (mod === 1) {
            str = "Buzz";
        } else if (mod === 2) {
            str = "FizzBuzz";
        } else {
            str = "";
        }

        if (mod3 === 0) {
            str += "Fizz";
        } else if (mod3 === 1) {
            str += "Buzz";
        } else if (mod3 === 2) {
            str += "FizzBuzz";
        } else {
            str += "";
        }

        if (mod5 === 0) {
            str += "Fizz";
        } else if (mod5 === 1) {
            str += "Buzz";
        } else if (mod5 === 2) {
            str += "FizzBuzz";
        } else {
            str += "";
        }

        if (str!== "") {
            console.log(num + " " + str);
        }
    }
}
{{< /highlight >}}

It starts generating towards the end.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/YBMKRGxUrzVe6nrYQ97ZYaFwE" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/YBMKRGxUrzVe6nrYQ97ZYaFwE.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/YBMKRGxUrzVe6nrYQ97ZYaFwE.js" id="asciicast-YBMKRGxUrzVe6nrYQ97ZYaFwE" async></script>