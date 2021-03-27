+++
title = "Generating pickup lines with GPT-3"
author = ["Shane Mulligan"]
date = 2021-03-27T00:00:00+13:00
keywords = ["GPT-3", "emacs", "examplary"]
draft = false
+++

Original prompt
: <https://www.reddit.com/r/GPT3/comments/mdl7fl/500%5Fopeners%5Ffor%5Ftinder%5Fwritten%5Fby%5Fgpt3%5Fthe%5Fprompt/>


## Summary {#summary}

I create a prompt in my prompt description
format and use it to generate some pickup
lines.


## Demonstration {#demonstration}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2.js" id="asciicast-Fio4sczoyqk5dMttvd36Dn5h2" async></script>


## Pick up lines {#pick-up-lines}


### Harry Potter {#harry-potter}

{{< highlight text "linenos=table, linenostart=1" >}}
What house would you be sorted into?
How many times have you watched the entire Harry Potter series?
Where would you go on your dream date with Hermione Granger?
When did you realize you were a wizard?
Do you believe we have a chance at world peace?
Are you OK with me using the Patronus charm to tell you I love you?
{{< /highlight >}}


### dancing {#dancing}

{{< highlight text "linenos=table, linenostart=1" >}}
What’s the last song you danced to?
You’re my favorite dance move.
Would you rather be a great dancer or a great singer?
Can you dance in the rain?
Can you do the robot?
If you could only dance to one song for the rest of your life, what song would it be?
{{< /highlight >}}


### The prompt {#the-prompt}

-   Design patterns used:
    -   [mulitplex pattern](http://github.com/semiosis/prompt-engineering-patterns/blob/master/patterns/multiplex-answers.org)
    -   [### pattern](http://github.com/semiosis/prompt-engineering-patterns/blob/master/patterns/use-separator-between-examples.org)
    -   [prelude pattern](http://github.com/semiosis/prompt-engineering-patterns/blob/master/patterns/reflective-description-of-task.org)

This prompt multiplexes answers.

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "generate pick up line"
doc: "Generate pick up lines from a topic"
prompt: |+
    The following are witty openers for Tinder:
    ###
    TOPIC: BOOKS
    OPENERS:
    I’m starting a book club, but my house is too small.
    Which Harry Potter house do you belong to?
    If you were asked to live your life as any book character, which one would you choose?
    Tell me three things you have in common with your favorite book character.
    Have you ever swam through an entire book?
    What book had the biggest impact on your life?
    ###
    TOPIC: PSYCHOLOGY
    OPENERS:
    Have you ever had two days in a row where you didn’t text anyone?
    How much do you pee per day?
    Are you a certified psychologist?
    Do you give pro-bono therapy?
    What’s the difference between you and a psychologist?
    Are you an expert on human behavior?
    ###
    TOPIC: <1>
    OPENERS:
engine: "davinci"
# 0.0 = /r/hadastroke
# 1.0 = /r/iamveryrandom
# Use 0.3-0.8
temperature: 0.8
max-tokens: 60
top-p: 1.0
# Not available yet: openai api completions.create --help
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "###"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "topic"
examples:
- "music"
chomp-start: on
chomp-end: off
external: ""
conversation-mode: no
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
needs-work: no
{{< /highlight >}}


## Further exploration {#further-exploration}

I wonder if I put the temperature down to 0 if
the pickup lines would all come out autistic.
and if I raised it to 1, the pickup lines
would be super random -- you know crazy girls
might like that.


### `temperature: 0.0` {#temperature-0-dot-0}

{{< highlight text "linenos=table, linenostart=1" >}}
What’s your favorite Harry Potter spell?
What’s your patronus?
What’s your favorite Harry Potter character?
What’s your favorite Harry Potter movie?
What’s your favorite Harry Potter book?
What’s your favorite Harry Potter character?
What’s your favorite Harry Potter spell?
What’s your favorite Harry Potter movie?
What’s your favorite Harry Potter book?
What’s your favorite Harry Potter character?
What’s your favorite Harry Potter spell?
What’s your favorite Harry Potter movie?
What’s your favorite Harry Potter book?
What’s your favorite Harry Potter character?
What’s your favorite Harry Potter spell?
What’s your favorite Harry Potter movie?
{{< /highlight >}}


### `temperature: 1.0` {#temperature-1-dot-0}

{{< highlight text "linenos=table, linenostart=1" >}}
To your mind, what is the mixture of Hufflepuff, Slytherin, and Gryffindor?
Did you know that Hermione Granger wasn’t real, she was just an illusion that you created in order to tell yourself that you didn’t have any hope of living a normal life?
{{< /highlight >}}

Oh my goodness. I love this.
I'm keeping temperature 1.0. This is me in real life, actually.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/hAdkA7aBewFbNVEmRCNwgcnfq" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/hAdkA7aBewFbNVEmRCNwgcnfq.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/hAdkA7aBewFbNVEmRCNwgcnfq.js" id="asciicast-hAdkA7aBewFbNVEmRCNwgcnfq" async></script>