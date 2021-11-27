+++
title = "Apostrophe (conversation unrestricted)"
author = ["Shane Mulligan"]
date = 2021-11-27T00:00:00+13:00
keywords = ["pen"]
draft = false
+++

| Install with Pen |                                                      |
|------------------|------------------------------------------------------|
| Pen.el on GitHub | <https://github.com/semiosis/pen.el/>                |
| Tutorial         | <https://mullikine.github.io/posts/pen-el-tutorial/> |


## Introducing Apostrophe! {#introducing-apostrophe}

The `comint` conversation mode for emacs that uses language models.

{{< highlight text "linenos=table, linenostart=1" >}}
apostrophe
    An exclamatory passage in a speech or poem
    addressed to a person (typically one who
    is dead or absent) or thing (typically one
    that is personified).

    It's a little bit like a soliloquy.
{{< /highlight >}}


### Talk to people {#talk-to-people}

{{< highlight sh "linenos=table, linenostart=1" >}}
apostrophe Amber Heard
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/nuT2ZMwujnnSObNUTGqHArOsB" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/nuT2ZMwujnnSObNUTGqHArOsB.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/nuT2ZMwujnnSObNUTGqHArOsB.js" id="asciicast-nuT2ZMwujnnSObNUTGqHArOsB" async></script>


### Use the universal highlighting (`-nw`) {#use-the-universal-highlighting--nw}

This works with any terminal `Pen.el` invocation.

Here is a three-way conversation between two chatbots and myself.

{{< highlight sh "linenos=table, linenostart=1" >}}
apostrophe -nv Amber Heard
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/K40px4H4CPPN15QMz6Uy8Pz3q" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/K40px4H4CPPN15QMz6Uy8Pz3q.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/K40px4H4CPPN15QMz6Uy8Pz3q.js" id="asciicast-K40px4H4CPPN15QMz6Uy8Pz3q" async></script>

{{< highlight sh "linenos=table, linenostart=1" >}}
apostrophe -nv Tom Cruise
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/aLn5lZq5yIeBxR6dyFyveDJGc" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/aLn5lZq5yIeBxR6dyFyveDJGc.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/aLn5lZq5yIeBxR6dyFyveDJGc.js" id="asciicast-aLn5lZq5yIeBxR6dyFyveDJGc" async></script>

Next step, combine the REPLs together and let them talk to eachother!


## Select arbitrary text and initiate a conversation {#select-arbitrary-text-and-initiate-a-conversation}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/YdX4bzPownHsVT3T0UPLMcp3c" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/YdX4bzPownHsVT3T0UPLMcp3c.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/YdX4bzPownHsVT3T0UPLMcp3c.js" id="asciicast-YdX4bzPownHsVT3T0UPLMcp3c" async></script>