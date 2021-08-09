+++
title = "Language agnostic code generator in Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-10T00:00:00+12:00
keywords = ["pen", "gpt", "gpt-j", "openai", "eleutherai", "emacs"]
draft = false
+++

## Summary {#summary}

Following on from [YASnippet combined with Pen.el - Controllable prompt generation // Bodacious Blog](https://mullikine.github.io/posts/yasnippet-combined-with-pen-el-controllable-prompt-generation/),
I make another language-agnostic code generator.

Behind the scenes, this prompt generates the
comments describing a code snippet. The
comments are in the desired language and the
comments are removed from the results.


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: code generator from description
doc: Language agnostic
prompt-version: 1
lm-command: "openai-complete.sh"
model: davinci
# lm-command: "aix-complete.sh"
# model: GPT-J-6B
temperature: 0.3
max-tokens: 200
stop-sequences:
- "###"
top-p: 1
# Unfortunately, it's not yet possible to have a prompt which ends in whitespace.
# It would really help with suggesting the comments have finished.
prompt: |+
  Language: `(pen-detect-language)``(comment-line 3)`
  Description: The following code is an implementation of <code description>:
  Code:
  >
vars:
- code description
postprocessor: sed 1,3d | sed "/^[^>]/q" | sed -e "\$d" -e "s/^> *//"
end-yas: on
# The start will not be trimmed
insertion: on
# I guess that this would usually be done manually
continuation-prompt: Generic completion 50 tokens
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/h4yKifxjp6wskRCfJKtVXL2Q8" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/h4yKifxjp6wskRCfJKtVXL2Q8.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/h4yKifxjp6wskRCfJKtVXL2Q8.js" id="asciicast-h4yKifxjp6wskRCfJKtVXL2Q8" async></script>


## Results {#results}


### GPT-3 {#gpt-3}


#### haskell {#haskell}

{{< highlight haskell "linenos=table, linenostart=1" >}}
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
{{< /highlight >}}


#### perl {#perl}

{{< highlight perl "linenos=table, linenostart=1" >}}
sub quicksort {
    my ($left, $right) = @_;
    my $pivot = $_[0];
    my $pivotIndex = 0;
    for ($left; $left <= $right; $left++) {
        if ($left == $pivot) {
            $pivotIndex = $left;
        } else {
            $pivotIndex = ($left + $right) / 2;
        }
        $left = $pivotIndex;
    }
    $right = $pivotIndex;
    return ($left, $right);
}

print "Enter the array to be sorted: ";
my $array = <STDIN>;
print "Enter the number of elements in the array: ";
{{< /highlight >}}


### GPT-J {#gpt-j}

Unfortunately, I ran out of credits, but I
don't think it worked too well anyway.