+++
title = "Exploring Neuralink concepts with GPT-3"
author = ["Shane Mulligan"]
date = 2021-04-14T00:00:00+12:00
keywords = ["GPT-3", "neuralink", "neuroengineering"]
draft = false
+++

## Summary {#summary}

I create several `org-brain` repositories for
exploratory learning and ideation.

{{< highlight text "linenos=table, linenostart=1" >}}
billboard
exploratory
fungible
ideation
infogetics
infrastructure-tooling
neuralink
open-source-alternatives
reference
thoughts
tooling
welfare-organisations
{{< /highlight >}}

I then use GPT-3 to discuss topics and why they are important.


## Glossaries {#glossaries}

-   <http://github.com/mullikine/glossaries-gh/blob/master/biochemistry.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/brain-computer-interface-bci.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/neuralink.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/neuroscience.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/neural-engineering.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/gpt.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/ai-safety.txt>
-   <http://github.com/mullikine/glossaries-gh/blob/master/general-ai-agi.txt>


## Demonstration {#demonstration}

Future of life institute and telepathy.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/gNHRbCi7kQDT6qFRih1zvLyni" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/gNHRbCi7kQDT6qFRih1zvLyni.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/gNHRbCi7kQDT6qFRih1zvLyni.js" id="asciicast-gNHRbCi7kQDT6qFRih1zvLyni" async></script>


### What is `neuroengineering`? {#what-is-neuroengineering}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/xs8YGxIbCZ4Bpc85N2qhenoRb" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/xs8YGxIbCZ4Bpc85N2qhenoRb.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/xs8YGxIbCZ4Bpc85N2qhenoRb.js" id="asciicast-xs8YGxIbCZ4Bpc85N2qhenoRb" async></script>


## Scrape the neuralink website for youtube videos {#scrape-the-neuralink-website-for-youtube-videos}

{{< highlight bash "linenos=table, linenostart=1" >}}
cd "$DUMP/programs/httrack/mirrors/-r6-https-neuralink-com-"
anygrep rosie-urls | grep youtube.com | oc -u | pavs
{{< /highlight >}}

<a id="code-snippet--neuralink-website-vids"></a>
```bash
[[https://www.youtube.com/watch?v=DVvmgjBL74w][Neuralink Progress Update, Summer 2020 - YouTube]]
[[https://www.youtube.com/watch?v=rsCul1sp4hQ][Monkey MindPong - YouTube]]
[[https://www.youtube.com/watch?v=iSutodqCZ74][Snout Boops - YouTube]]
[[https://www.youtube.com/watch?v=LgJpYOTll8U][Monkey MindPong Picture-in-Picture - YouTube]]
[[https://www.youtube.com/watch?v=gMCkMHbpPFA][Animal Care at Neuralink - YouTube]]
[[https://www.youtube.com/watch?v=-gQn-evdsAo][Working on the Neuralink Robot - YouTube]]
```


## <span class="org-todo todo TODO">TODO</span> Generate transcripts {#generate-transcripts}

$MYGIT/semiosis/prompts/prompts/key-points.prompt

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/NNGjEYHB9B2rOyvUAPzMPI0sa" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/NNGjEYHB9B2rOyvUAPzMPI0sa.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/NNGjEYHB9B2rOyvUAPzMPI0sa.js" id="asciicast-NNGjEYHB9B2rOyvUAPzMPI0sa" async></script>


### [Neuralink Progress Update, Summer 2020 - YouTube](https://www.youtube.com/watch?v=DVvmgjBL74w) {#neuralink-progress-update-summer-2020-youtube}

$DUMP/tmp/scratchF4ZkzB.txt


### [Monkey MindPong - YouTube](https://www.youtube.com/watch?v=rsCul1sp4hQ) {#monkey-mindpong-youtube}

Transcript
: <./monkey-mindpong.txt>

This is Pager.

He's a nine-year-old macaque who had a
neurolink placed in each side of his brain
about six weeks ago.

If you look carefully, you can see that the
fur on his head hasn't quite fully grown back
yet.

He's learnt to interact with a computer for a
tasty banana smoothie delivered through a
straw.

We can interact with the neurolinks simply by
pairing them to an iPhone, just as you might
pair your phone to a Bluetooth speaker.

The links record from more than 2000
electrodes implanted in the regions of the
subject's motor cortex that coordinate hand
and arm movements.

Neurons in this region modulate their activity
with intended hand movement for example, some
might become more active when the subject
moves his hand up, and others when he moves it
to the right.


### [Snout Boops - YouTube](https://www.youtube.com/watch?v=iSutodqCZ74) {#snout-boops-youtube}


### [Monkey MindPong Picture-in-Picture - YouTube](https://www.youtube.com/watch?v=LgJpYOTll8U) {#monkey-mindpong-picture-in-picture-youtube}


### [Animal Care at Neuralink - YouTube](https://www.youtube.com/watch?v=gMCkMHbpPFA) {#animal-care-at-neuralink-youtube}


### [Working on the Neuralink Robot - YouTube](https://www.youtube.com/watch?v=-gQn-evdsAo) {#working-on-the-neuralink-robot-youtube}