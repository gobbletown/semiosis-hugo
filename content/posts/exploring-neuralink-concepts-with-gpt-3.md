+++
title = "Exploring Neuralink concepts with GPT-3 (WIP)"
author = ["Shane Mulligan"]
date = 2021-04-14T00:00:00+12:00
keywords = ["GPT-3", "neuralink", "neuroengineering"]
draft = false
+++

<span class="underline">_This article is a work in progress._</span>


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


## A paper from my professor Zhiyi Huang {#a-paper-from-my-professor-zhiyi-huang}

<https://link.springer.com/article/10.1007%2Fs13755-021-00142-y>


### New terms taken from the synopsis {#new-terms-taken-from-the-synopsis}

{{< highlight text "linenos=table, linenostart=1" >}}
Crosstalk
    Unintended brain activations that
    interfere with bimanual actions and could
    also occur during motor imagery.

Bimanual coordination
    Encompasses a large class of situations in
    which the brain must simultaneously
    control multiple movements, such as when
    we use our two hands to manipulate an
    object or perform a task.

    Bimanual coordination has been one of the
    most widely studied problems in motor
    control.

Pairwise classification
    A class binarization procedure that
    converts a multi-class problem into a
    series of two-class problems, one problem
    for each pair of classes.

Motor imagery
    A dynamic state during which an individual
    mentally simulates a physical action.

    This type of phenomenal experience implies
    that the subject feels themselves
    performing the action.
{{< /highlight >}}


### Synopsis (tidied) {#synopsis--tidied}

Brain–computer interfaces (BCIs) target
specific brain activity for neuropsychological
rehabilitation, and also allow patients with
motor disabilities to control mobility and
communication devices.

Motor imagery of single-handed actions is used
in BCIs but many users cannot control the BCIs
effectively, limiting applications in the
health systems.

Crosstalk is unintended brain activations that
interfere with bimanual actions and could also
occur during motor imagery.

To test if crosstalk impaired BCI user
performance, we recorded EEG in 46
participants while they imagined movements in
four experimental conditions using motor
imagery: left hand (L), right hand (R), tongue
(T) and feet (F).

Pairwise classification accuracies of the
tasks were compared (LR, LF, LT, RF, RT, FT),
using common spatio-spectral filters and
linear discriminant analysis.

We hypothesized that LR classification
accuracy would be lower than every other
combination that included a hand imagery due
to crosstalk.

As predicted, classification accuracy for LR
(58%) was reliably the lowest.

Interestingly, participants who showed poor LR
classification also demonstrated at least one
good TR, TL, FR or FL classification; and good
LR classification was detected in 16% of the
participants.

For the first time, we showed that crosstalk
occurred in motor imagery, and affected BCI
performance negatively.

Such effects are effector-sensitive regardless
of the BCI methods used; and likely not
apparent to the user or the BCI developer.

This means that tasks choice is crucial when
designing BCI.

Critically, the effects of crosstalk appear
mitigatable.

We conclude that understanding crosstalk
mitigation is important for improving BCI
applicability.


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


### [Neuralink Monkey MindPong Deconstructed - YouTube](https://www.youtube.com/watch?v=rzNOuJIzk2E) {#neuralink-monkey-mindpong-deconstructed-youtube}

<./monkey-mindpong-deconstructed.txt>


### [Neuralink Progress Update, Summer 2020 - YouTube](https://www.youtube.com/watch?v=DVvmgjBL74w) {#neuralink-progress-update-summer-2020-youtube}

<./neuralink-progress-summer2020.txt>


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