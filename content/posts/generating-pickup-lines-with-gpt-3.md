+++
title = "Generating pickup lines with GPT-3"
author = ["Shane Mulligan"]
date = 2021-03-27T00:00:00+13:00
keywords = ["GPT-3", "emacs", "examplary"]
draft = false
+++

Latest version of the pick up lines prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/very-witty-pick-up-lines.prompt>


## Summary {#summary}

I create a prompt in my prompt description
format and use it to generate some pickup
lines.


## Demonstration (version 2) {#demonstration--version-2}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Dn3gncKZzJWdJ4PE4qCJPSwpF" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Dn3gncKZzJWdJ4PE4qCJPSwpF.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Dn3gncKZzJWdJ4PE4qCJPSwpF.js" id="asciicast-Dn3gncKZzJWdJ4PE4qCJPSwpF" async></script>


### Pick up lines with the topic "SETTLERS OF CATAN" {#pick-up-lines-with-the-topic-settlers-of-catan}

{{< highlight text "linenos=table, linenostart=1" >}}
When playing Settlers of Catan, the shortest route is a straight line to my heart.
I wish you were a Settler so we could settle down together.
What do you want to trade me for one of your sheep?
Do you play Catan? Cuz I think you'd be a great development card for me.
May I join your settlement?
I'm not sure if you're a settlement that I want to build up or a barbarian I want to raze.
Do you play settlers of Catan? You should play with me.
I'd Like to settle on your face.
I'd like to settle between your settlements.
Do you have any wool? I want to get sheep with you.
I'd Like to settle down on Catan with you.
I think you're something special, like a good card in Catan.
When we play Settlers of Catan, I'll trade a brick for you.
I'm happy to be on your game board.
I like you a lot! I like you so much I'd trade all my sheep for you.
Settlers need resources, and I need you.
I like you a lot. I'd like to trade you for a sheep.
Do you Like Settlers of Catan? Cuz I'd Like to settle on your land.
You look like a settler I'd like to settle with.
If you were settlers of Catan, you'd be the grain.
Can I be the robber to your resources?
I'm a fan of Settlers of Catan, and I'd like you to be my 10.
I want to settle my people on your resources.
I'm a settler looking for a hex that will settle.
You look like a woman I'd like to settle island with.
I'd like to settle along your rivers and settle your hills.
You look like the type of girl I'd like to settle in a small island with.
I'd like to trade hexes with you.
{{< /highlight >}}


## Demonstration (version 1, not as good) {#demonstration--version-1-not-as-good}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Fio4sczoyqk5dMttvd36Dn5h2.js" id="asciicast-Fio4sczoyqk5dMttvd36Dn5h2" async></script>


## Original batch of pick up lines {#original-batch-of-pick-up-lines}

These appeared to be more like questions.


### Topic: "Harry Potter" {#topic-harry-potter}

{{< highlight text "linenos=table, linenostart=1" >}}
What house would you be sorted into?
How many times have you watched the entire Harry Potter series?
Where would you go on your dream date with Hermione Granger?
When did you realize you were a wizard?
Do you believe we have a chance at world peace?
Are you OK with me using the Patronus charm to tell you I love you?
{{< /highlight >}}


### Topic: "dancing" {#topic-dancing}

{{< highlight text "linenos=table, linenostart=1" >}}
What’s the last song you danced to?
You’re my favorite dance move.
Would you rather be a great dancer or a great singer?
Can you dance in the rain?
Can you do the robot?
If you could only dance to one song for the rest of your life, what song would it be?
{{< /highlight >}}


### Breakdown of the original prompt {#breakdown-of-the-original-prompt}

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


## Version 2 {#version-2}


### Techniques to making a better prompt {#techniques-to-making-a-better-prompt}

-   Use query reformulation (pick the best ones and feed back in to improve the prompt)
-   Use a generic completer to more rapidly redesign the prompt
-   I label each generated pick up line so the new generations are more specific when leaving a label out that I want to avoid

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "Very witty pick up lines for a topc"
# future-titles: ""
# aims: |+
# - More abstractive rewording
doc: "Given a topic, generate pick up lines, one at a time"
# aims: |+
# - Successful
prompt-version: 1
prompt: |+
    The following are witty openers for Tinder:
    ###
    TOPIC: CATS AND DRUGS
    - Can I call you my catnip?
    ###
    TOPIC: CATS
    - If I said you had a purrfect body, would you hold it against me?
    ###
    TOPIC: CATS AND HAPPINESS
    - Do you want to be my cat? You'll get fed, I'll pet you, you'll stay warm, and we'll both be happy.
    ###
    TOPIC: CATS AND FOOD
    - You look like you'd taste good with a little milk.
    ###
    TOPIC: PIZZA
    - You look like a slice of heaven.
    ###
    TOPIC: PIZZA INNUENDO
    - If you were a pizza, you would be half cheese and half toppings.
    ###
    TOPIC: NACHOS INNUENDO
    - Do you want to be my dipping sauce?
    ###
    TOPIC: FOOD ANALOGY
    - You look like the salt to my pepper.
    ###
    TOPIC: SLEEP INNUENDO
    - Are you a good pillow talker or a bad pillow talker?
    ###
    TOPIC: SLEEP ANALOGY
    - You're like the sun in the morning.
    ###
    TOPIC: VIDEO GAMES
    - Are people ever mean to you in games for being too cute?
    ###
    TOPIC: VIDEO GAMES
    - You look like the type of girl I want to be a Pokemon trainer to.
    ###
    TOPIC: TRAVEL
    - I want some of your culture.
    ###
    TOPIC: TRAVEL
    - I want to explore every part of you.
    ###
    TOPIC: STARCRAFT
    - I'm glad I scouted you.
    ###
    TOPIC: TRAVEL
    - I think your exotic.
    ###
    TOPIC: TRAVEL
    - I'm at the airport, looking at the plane schedule. Now all I need is you.
    ###
    TOPIC: TACOS AND INNUENDO
    - You look like the type that would be good with a little hot sauce.
    ###
    TOPIC: ASTRONAUTS
    - I think we should make space travel plans.
    ###
    TOPIC: ASTRONAUTS AND INNUENDO
    - I'd like to be your space shuttle.
    ###
    TOPIC: COFFEE
    - Do you like coffee? Cuz I like you a latte.
    ###
    TOPIC: <1>
    -
# # Additional transformation of prompt after the template
# prompt-filter: "sed -z 's/\s\+$//'"
# # Trailing whitespace is always removed
# prompt-remove-trailing-whitespace: on
# myrc will select the completion engine using my config.
# This may be openi-complete or something else
engine: "myrc"
# if nothing is selected in myrc and openapi-complete is used
# by default, then openai should select this engine.
preferred-openai-engine: "davinci"
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
# - "\n"
# - "\n\n"
- "###"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
# Cache the function by default when running the prompt function
cache: on
vars:
- "topic"
examples:
- "tectonic plates"
# Completion is for generating a company-mode completion function
# completion: on
# # default values for pen -- evaled
# # This is useful for completion commands.
# pen-defaults:
# - "(detect-language)"
# - "(pen-preceding-text)"
# These are elisp String->String functions and run from pen.el
# It probably runs earlier than the preprocessors shell scripts
pen-preprocessors:
- "tr '[:lower:]' '[:upper:]'"
# # A preprocessor filters the var at that position
# the current implementation of preprocessors is kinda slow and will add ~100ml per variable
# # This may be useful to distinguish a block of text, for example
# preprocessors:
# - "sed 's/^/- /"
# - "cat"
chomp-start: on
chomp-end: off
prefer-external: on
external: ""
# Enable running conversation
conversation-mode: no
# Replace selected text
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
needs-work: no
n-test-runs: 5
# Prompt function aliases
# aliases:
# - "asktutor"
# postprocessor: "sed 's/- //' | uniqnosort"
# Run it n times and combine the output
n-collate: 10
postprocessor: "awk 1"
{{< /highlight >}}


### Pick up lines with the topic "tectonic plate" {#pick-up-lines-with-the-topic-tectonic-plate}

{{< highlight text "linenos=table, linenostart=1" >}}
I think we could make some beautiful earthquakes together.
I want to go down on you until you erupt like a tectonic plate.
I think we should meet at the fault-line.
We're plates tectonically.
I like you a lot.
Are you the one that moved my earth plate?
I feel an Earth-shaking connection.
I've been looking for a long time to find a place like this.
We're like the same tectonic plate.
We fit together.
I'm a tectonic plate shifting.
If you were a tectonic plate, I'd want to be a volcano.
The only friction I want between us is in the bedroom.
You're the type of girl that I want to shift into a new continent with.
I think I can see your tectonic plates through your top.
I think we might be convergent.
The earth's plates just moved under me.
You're the tectonic plate I've been looking for.
If you were a tectonic plate, I'd be your subduction zone.
If you were a tectonic plate, I'd want to be a subduction zone.
If we don't go on a date, we'll be tectonically doomed.
Did you feel it?
The earth just moved.
I'd like to meet under the two continental plates.
Your smile is so magnetic, I feel the Earth move every time I see it.
I'm hoping we can someday collide tectonically.
I'd like to be on your plate.
You move me.
I like your tectonic plates.
You look like the type to go down and make a plate tectonic.
I want to be your tectonic plate.
I think that we would be a good tectonic plate couple.
We're like the tectonic plates of the Earth.
We'll never collide, but we'll have an earthquake whenever we meet.
I think we're two tectonic plates colliding.
I think we should meet up and move together.
You look like you're a tectonic plate subducting under a continental plate.
{{< /highlight >}}


### Pick up lines with the topic "Easter" {#pick-up-lines-with-the-topic-easter}

{{< highlight text "linenos=table, linenostart=1" >}}
I think you're the Easter egg I've been hunting for all my life.
I hope you're not a rabbit, because I want to make you my pet.
I want to fill your basket.
I want to be the white chocolate bunny you squeeze to my heart's content.
Would you be my Easter bunny?
Do you celebrate Easter? Cuz I'd like to be your Easter bunny.
I'm just a little bunny-hop away from you.
I would like to give you a basket of treats.
I'll be good this year, Easter bunny.
What did the rabbit say to the other rabbit on Easter? Happy Easter bunnay!
I want to fill you up with some chocolate and one of my eggs.
I would egg-stra like to get to know you better.
I'm a little rabbit, hopping down the bunny trail, looking for a good time.
Are you an easter bunny? Cuz I'm gonna hide and seek you.
Are you the bunny that lays the golden eggs?
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/2sRonQkPhOdr5Kb6FZlQK7H57" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/2sRonQkPhOdr5Kb6FZlQK7H57.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/2sRonQkPhOdr5Kb6FZlQK7H57.js" id="asciicast-2sRonQkPhOdr5Kb6FZlQK7H57" async></script>


## Ideas {#ideas}

-   Mix two topics to attempt to make them even wittier?


## Appendix {#appendix}

Original prompt
: <https://www.reddit.com/r/GPT3/comments/mdl7fl/500%5Fopeners%5Ffor%5Ftinder%5Fwritten%5Fby%5Fgpt3%5Fthe%5Fprompt/>