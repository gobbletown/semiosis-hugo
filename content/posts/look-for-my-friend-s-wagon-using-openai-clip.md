+++
title = "Look for my friend's wagon using OpenAI CLIP"
author = ["Shane Mulligan"]
date = 2021-03-12T00:00:00+13:00
keywords = ["openai", "clip"]
draft = false
+++

## Full demonstration {#full-demonstration}

I show you how easy it is to search for an
arbitrary thing inside of an arbitrary youtube
video.

I am blogging and recording as I am
demonstrating the technology.

Skip to 3 minutes to see the magic.

<a title="asciinema recording" href="https://asciinema.org/a/rbMQDnUccPQEEigQYXXeE48Xa" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/rbMQDnUccPQEEigQYXXeE48Xa.svg" /></a>


## Summary {#summary}

I am looking for my friend's wagon in a
youtube video.

This is the video
: [Here's What We REALLY Think Of Your Cars 5 - YouTube](https://www.youtube.com/watch?v=K-jYThQKZsQ)

The images of various wagons should appear after I run the command which I have bound in emacs.

| kb        | f                 |              |
|-----------|-------------------|--------------|
| `M-l / V` | `find-in-youtube` | `global-map` |

It's based on code from this github repository.

<https://github.com/haltakov/natural-language-youtube-search>

{{< highlight sh "linenos=table, linenostart=1" >}}
find-in-video "https://www.youtube.com/watch?v=K-jYThQKZsQ" wagon
{{< /highlight >}}

This is looking for wagons.


## Results {#results}

{{< figure src="/ox-hugo/wagon.png" >}}