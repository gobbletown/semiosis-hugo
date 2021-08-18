+++
title = "Generating perl one-liners using GPT-3 and Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-18T00:00:00+12:00
keywords = ["perl", "gpt", "openai", "pen"]
draft = false
+++

## Summary {#summary}

I demonstrate generating perl one-liners in
emacs using a prompt function and explain
parts of the prompt.


### _<:pp>perl -_ {#pp-perl}

This enables you to specify the first part of
the output. `<:pp>` is a placed where you want
to include the rest of the prompt in the
beginning of the output.


### _split-patterns_ {#split-patterns}

README
: <http://github.com/semiosis/prompts/>

`split-patterns` is a field for describing
patterns to separate multiple prompt results
from one output if the LM multiplexed the
results intentionally or unintentionally.


### _engine: OpenAI Davinci_ {#engine-openai-davinci}

README
: <http://github.com/semiosis/engines>

This specifies the engine which specifies both the model and
the shell command that calls the API.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/OVwCp9NF9YENgOHwuAu55iqxq" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/OVwCp9NF9YENgOHwuAu55iqxq.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/OVwCp9NF9YENgOHwuAu55iqxq.js" id="asciicast-OVwCp9NF9YENgOHwuAu55iqxq" async></script>


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: Generate perl command
prompt-version: 2
prompt: |
  perl

  Sort by line length
  perl -e 'print sort { length($b) <=> length($a) } <>'
  ###
  Increment all numbers by one
  perl -pe 's/(\d+)/ 1 + $1 /ge'
  ###
  Reverses paragraphs
  perl -00 -e 'print reverse <>'
  ###
  Look for duplicated words in a line
  perl -0777 -ne 'print "$.: doubled $_\n" while /\b(\w+)\b\s+\b\1\b/gi'
  ###
  <1>
  <:pp>perl -
engine: OpenAI Davinci
temperature: 0.3
max-tokens: 60
top-p: 1
stop-sequences:
- "###"
vars:
- description
n-collate: 2
split-patterns:
- "\n"
examples:
- Make all occurrences of Steve lowercase
{{< /highlight >}}


## Example outputs {#example-outputs}


### These outputs appear to not all be complete {#these-outputs-appear-to-not-all-be-complete}

_Extract URLs_.

Close, but not perfect.

{{< highlight bash "linenos=table, linenostart=1" >}}
perl -0777 -e 'print "$.: $_" while /<a href="\S+">\S+<\/a>/gi'
# perl -0777 -ne 'print "$.: $_" while /\b(http|https|ftp|gopher):\/\/[-a-z0-9+&@#\/%?=~_|!:,.;]*[-a-z0-9+&@#\/
# perl -0777 -ne 'print "$.: $_" while /\b(https?|ftp|file):\/\/[-a-z0-9+&@#\/%?=~_|!:,.;]*[-a-z0-9+&@#\/%=~
# perl -0777 -ne 'print "$.: $_" while /\b(https?|ftp|file)://[-a-z0-9+&@#\/%?=~_|!:,.;]*[-a-z0-9+&@#\/%=~
perl -0777 -ne 'print "$.: $_" while /<a href="(.+?)">/gi'
perl -0777 -ne 'print "$.: $_" while /<a href="(.*?)">(.*?)<\/a>/gi'
# perl -0777 -ne 'print "$.: $_" while /<a href="(.*?)">(.*?)<\/a
perl -0777 -ne 'print "$.: $_" while /\b(http|https|ftp|gopher|file|news|nntp):\b/gi'
perl -0777 -ne 'print "$.: $_\n" while /<a href="(.*?)">(.*?)<\/a>/gi'
# perl -0777 -ne 'print "$.: $_\n" while /<a href="(.*?)">(.*
perl -0777 -ne 'print "$.: $_" while /<a href="(.*?)"/gi'
perl -0777 -e 'print "$.: $_" while /<a href="(.*?)">(.*?)<\/a>/gi'
{{< /highlight >}}

_increment the first number_

I will need to also have a validation filter for prompt outputs.

{{< highlight bash "linenos=table, linenostart=1" >}}
perl -pe 's/(\d+)/ 1 + $1 /ge'
perl -0777 -ne 'print "$.: doubled $_\n" while /\b(\d+)\b/gi'
perl -pe '$_ = $. + 1'
perl -0777 -ne 'print "$.: doubled $_\n" while /\b(\d+)\b\s+\b\1\b/gi'
# perl -0777 -ne 'print "$.: doubled $_\n" while /\b(\d+)\b
perl -e '$_=reverse'
perl -e '$_=reverse; s/\d+/1+$_/ge'
{{< /highlight >}}