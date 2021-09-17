+++
title = "Browsing the imaginary web with Codex"
author = ["Shane Mulligan"]
date = 2021-09-09T00:00:00+12:00
keywords = ["codex", "openai", "emacs", "imaginary-internet"]
draft = false
+++

## Summary {#summary}

I make an imaginary web browser. This will be
improved in the future, but it's already
fairly trivial to browse the imaginary web.
Let me show you how.

There is no need for the websites returned to
be perfect as we have a tool `Codex`, which
can make sense of the imperfect. This works
fine as it is, just open your mind on the way
in which people should view and interact with
the web.

-   Visit any website you can imagine, _even the ones that are **not real**!_
-   Edit and re-imagine as you go see alternative website realities -- change the sentiment of the author!
-   Peer into the future -- read about GPT-5!
-   Generate relevant URLs (often real, sometimes imaginary) from any text selection
-   Read an article on anything from your favourite blogger
-   Also generates HTML to load the page in your fav browser. Pull it all together, and
-   ... This is the future of the web.

---

{{< highlight text "linenos=table, linenostart=1" >}}
imaginary internet
imaginary web
    An imaginary world-wide-web is an
    analog of the World-Wide-Web imagined by a
    language model.
{{< /highlight >}}


## The Definitive Demo {#the-definitive-demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/GD8hBG0GvxCnAoFtuFSygJEtD" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/GD8hBG0GvxCnAoFtuFSygJEtD.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/GD8hBG0GvxCnAoFtuFSygJEtD.js" id="asciicast-GD8hBG0GvxCnAoFtuFSygJEtD" async></script>

Explanation / commentary at the bottom of this article.


## Related {#related}

-   [Search the web with Codex {bye Google} // Bodacious Blog](https://mullikine.github.io/posts/search-the-web-with-codex/)

The above prompt for searching and generating
URLs has been modified to remove the
constraint that the return URLs must be real.

{{< highlight yaml "linenos=table, linenostart=1" >}}
# This contraint is not needed for the imaginary internet
# validator: url-exists
{{< /highlight >}}


## Looking Glass 🔍 web browser {#looking-glass-web-browser}

This is a demonstraction of an imaginary/complex web browser named _Looking Glass_.

Code
: <https://semiosis.github.io/pen/>

The real website
: <https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html>

The opening text:

{{< highlight text "linenos=table, linenostart=1" >}}
An Introduction to Type Level Programming

Posted on August 25, 2021 by Rebecca Skinner

Prelude



This blog post is a long-form article based on a talk I delivered at the
haskell.love conference on 10 Sept 2021. Keep an eye out here and I will
update this section with a link to the recording of the talk when it’s
available.

The original slides for this talk are available on github along with the
complete source code for the examples used in this article and the talk.

An Introduction to Type-Level Programming
{{< /highlight >}}

I will now use Pen.el to imagine the same website.
This will use the Codex engine.

Here are 2 possible continuations.

{{< highlight text "linenos=table, linenostart=1" >}}
https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html

                Introduction to Type-Level Programming

   Posted by Rebecca Skinner on 25 August 2021
   4 min read

   Type-level programming is well suited for statically typed languages
   with pattern matching, like Haskell. It isn’t always the right
   solution, though…

   Type-level programming is also pretty different from other
   statically typed languages. It’s not the only paradigm. So, let’s
   look at some examples. If you’re familiar with Haskell, these
   examples should look familiar to you.
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
https://rebeccaskinner.net/posts/2021-08-25-introduction-to-type-level-programming.html

                                                                                                          Introduction to Type-Level Programming
    by Rebecca Skinner

This post is also available in two other formats:

    PDF  |  Slides

New to type-level programming?

You might want to check out my post on introduction to type-level
programming, which includes links to free versions of my textbook,
Real World Haskell, and my favorite Haskell libraries. Or if you’re
new to this concept, you might want to read my article on how Haskell
reduced my software development time by 95%.

Theory of Types

Even if you’re not familiar with type-level programming in general,
it’s still important to know what it is. What’s more, even if you’re
familiar with Haskell, there are some concepts in Haskell that make type-level
programming easier, like type synonyms.

But in case you’re unfamiliar, type-level programming is a style of
programming that uses types in place of strings or identifiers in
order to describe the behavior of your program. Here’s an example of
a simple type-level program:

Tutorial.Data.Maybe
{{< /highlight >}}

I have selected the first.

I then generate two more possible continuations.

And I select this one:

{{< highlight text "linenos=table, linenostart=1" >}}
If not, hopefully they
  illustrate the core concepts.

  What is it good for?

  The first thing I want to show you is that type-level programming
  is great for expressing common programming patterns.

  A perfectly valid question you might ask is “why not just code it
  that way?” Well…here’s the thing. Type-level programming has the
  ability to express concepts at multiple levels of abstraction, so
  whomever’s reading your code can quickly zoom in on the relevant
  parts.

  To demonstrate this, I’m going to jump right in. We’ll start with
  pattern matching.

  Pattern Matching

  I’ll show you how you can simulate pattern matching with type-level
  booleans, just like you can with `case` expressions in Haskell.

  Booleans are just numbers, so you can compare
{{< /highlight >}}

But I want to change the topic. So I change "What is it good for?" to "What is it not good for?"

{{< highlight text "linenos=table, linenostart=1" >}}
Due to its type system, Haskell is great for eliminating runtime
   errors.

   This is because the compiler is able to prove that a piece of code
   will not throw a runtime error, and thus no runtime code is
   executed. This allows code to be written with no runtime checks.

   This is why Haskell can have a single, undisturbed type hierarchy
   with no runtime checks. In fact, this is why Haskell is the only
   fully-featured purely-typed functional language. It’s reference
   implementation, GHC, is also known for its correctness, if not
   performance.

   However, runtime checks prevent the compiler from building a
   completely pure, efficient type system.

   In a purely type-safe language, you can reason about a program by
   simply knowing its type.

   This allows you to avoid all runtime checks by simply knowing the
   structure of a program, and
{{< /highlight >}}


## Rendering HTML {#rendering-html}

Generate HTML from ASCII
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-html-from-ascii.prompt>

Tighter integration will be made with the eww browser in emacs.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun lg-render (ascii &optional url)
  (interactive (list (buffer-string)))

  (let* ((firstline (pen-snc "sed -n 1p | xurls" ascii))
         (rest (pen-snc "sed 1d" ascii))
         (url (or (sor url)
                  (sor firstline)))
         (ascii (if (sor url)
                    ascii
                  rest)))

    (new-buffer-from-string
     (pen-one (pf-generate-html-from-ascii-browser/2 url ascii))
     nil 'text-mode)))
{{< /highlight >}}


## Demo {#demo}

-   Imagine a website from a URL url: <http://ascii-art.com/octopuss>
-   Imagine a website from a URL url: <http://ascii-art.com/octopuss>
-   Imagine a website from a URL url: <http://funny-jokes.com/cat>
-   Imagine a website from a URL url: <http://computer-help.com?question=how%20do%20i%20browse%20the%20internet>
-   <https://en.wikipedia.org/wiki/Accelerationism>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/erGo5858UQgMIPjv0eGzMVBRe" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/erGo5858UQgMIPjv0eGzMVBRe.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/erGo5858UQgMIPjv0eGzMVBRe.js" id="asciicast-erGo5858UQgMIPjv0eGzMVBRe" async></script>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/1ATlUjWVRqgMqb83MsaFMvpeu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/1ATlUjWVRqgMqb83MsaFMvpeu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/1ATlUjWVRqgMqb83MsaFMvpeu.js" id="asciicast-1ATlUjWVRqgMqb83MsaFMvpeu" async></script>

{{< highlight text "linenos=table, linenostart=1" >}}
http://www.economist.com/blogs/graphicdetail/2012/09/growth
http://www.economist.com/printedition/2012-09-13
http://graphics8.nytimes.com/newsgraphics/2012/10/30/us-gdp-vs-europe-by-state/us-gdp-vs-europe-by-state.html
http://www.economist.com/blogs/graphicdetail/2012/10/growth-or-acceleration
http://www.economist.com/news/britain/21583272-many-countries-match-chinas-rapid-rural-migration-chinas-progress
http://www.economist.com/news/britain/21582529-how-did-china-manage-stop-its-population-growing-did-its-policies-work
http://www.economist.com/node/21583245
http://www.economist.com/node/21583254
http://www.economist.com/news/britain/21583283-growth-or-acceleration-britains-big-challenge-itself-time-start-year
http://www.economist.com/news/science-and-technology/21583624-building-new-ones-or-adding-new-parts-old-ones-big-ones-science-and
http://www.economist.com/news/leaders/21583640-governments-should-ditch-their-ambivalent-approach-big-challenges-new-long-way
https://www.economist.com/node/21541143
{{< /highlight >}}


## Prompt {#prompt}

For the demo, to speed up website generation,
I have set completions to 1.

{{< highlight yaml "linenos=table, linenostart=1" >}}
n-collate: 1
n-completions: 1
{{< /highlight >}}


### `pf-imagine-a-website-from-a-url/1` {#pf-imagine-a-website-from-a-url-1}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Imagine a website from a URL"
doc: "Given a URL, imagine the HTML for that page"
prompt-version: 2
prompt: |+
  Lynx, an ascii web browser.
  """
  lynx --dump -nolist "http://google.com" | head -n 30 <<EOD
  http://google.com

  Search Images Maps Play YouTube News Gmail Drive More »
  Web History | Settings | Sign in
  To all doctors, nurses, and medical workers, thank you

  ________________________________________________________    Advanced search
  [ Google Search ]   [ I'm Feeling Lucky ]

  Google offered in: Māori
  Advertising Programs     Business Solutions     About Google     Google.co.nz

  (c) 2021 - Privacy - Terms
  EOD

  lynx --dump -nolist "https://www.apple.com/" | head -n 30 <<EOD
  https://www.apple.com/

                                       Apple

     We look forward to welcoming you to our stores. Whether you shop in a
     store or shop online, our Specialists can help you buy the products
     you love. Shop with a Specialist, get credit with Apple Trade In,
     choose free delivery or pickup, and more at the Apple Store Online.
     Shop with a Specialist, get credit with Apple Trade In, choose free
     delivery or pickup, and more at the Apple Store Online.


  iPhone 12

  Blast past fast.

     From $29.12/mo. for 24 mo. or $699 before trade‑in^1

     Buy directly from Apple with special carrier offers
     Learn more Learn more Buy


  iPhone 12 Pro

  It’s a leap year.

     From $41.62/mo. for 24 mo. or $999 before trade‑in^1

     Buy directly from Apple with special carrier offers
     Learn more Learn more Buy
  EOD

  # "<q:url>" rendered in ASCII:
  lynx --dump -nolist "<q:url>" | head -n 30 <<EOD
  <url>


engine: "OpenAI Codex"
n-collate: 1
n-completions: 2
temperature: 0.5
max-generated-tokens: "(/ prompt-length 2)"
top-p: 1.0
stop-sequences:
- EOD
cache: on
vars:
- "url"
examples:
- "http://www.gprolog.org/"
info: on
completion: off
insertion: off
{{< /highlight >}}


## I browsed the imaginary web for Sam Altman's recent interview {#i-browsed-the-imaginary-web-for-sam-altman-s-recent-interview}

-   <https://www.lesswrong.com/posts/aihztgJrknBdLHjd2/sam-altman-q-and-a-gpt-and-agi/#%5FAbout%5FGPT4>

The first result was baffling.

Speak of messages from the future! GPT-3 was trained in 2019 after all.

{{< highlight text "linenos=table, linenostart=1" >}}
About GPT-4

Sam Altman - Q&A - GPT-4 & AGI

I recently published a long Medium post on the
potential dangers of AGI. I also promised to
do an interview about GPT-4,

It's not too late to change your mind.

GPT-4 is the name of a new generation of
Transformers-based generative models, recently
published by researchers at OpenAI.

GPT-4 is the first completely self-supervised
LM of essentially unlimited size, trained on
an effectively unlimited amount of text.

GPT-4 is the first model to demonstrate human-
level language understanding without any human
effort. It is the first model to demonstrate
unsupervised learning of high-quality, high-
resolution, and long-range text understanding.



Sam Altman is the President and Co-Founder of
initial seed fund Y Combinator. He writes the
Ask a VC column on Hacker News and co-hosts
the RSS podcast Hacker News Radio. He is the
author of the textbook The VC Handbook.

Sam Altman

1:12

What do you think of this idea?

I think AGI is a serious concern and we're
making progress on solving it. GPT-4 is an
incredibly exciting and interesting new
development.

GPT-4 is a product of OpenAI and I’m excited
about it because it’s a demonstration of the
power of unsupervised learning. This is really
exciting. It’s basically a demonstration of
the power of unsupervised learning.


Sam Altman

1:18

What

Next result:

https://www.lesswrong.com/posts/aihztgJrknBdLHjd2/sam-altman-q-and-a-gpt-and-agi/#_About_GPT4

                                                                                                                 Q and A:

How can explain and/or explain why GPT-4 is so
much better than GPT-3 at conversational and
QA?

When can we expect to have GPT-2 and GPT-3 be
as good as GPT-4 at conversational and QA?

Are there any useful things we can do with
GPT-4 that we can't with GPT-3 to increase
performance even further?
{{< /highlight >}}


## Explanation / commentary {#explanation-commentary}

{{< highlight text "linenos=table, linenostart=1" >}}
19:31 < libertyprime>
    Hi guys and gals. sorry for the self-promotion. If any of you have hacker
    news accounts, could you please upvote this for me? It's an imaginary web
    browser based on emacs: https://news.ycombinator.com/item?id=28489942

19:37 < a>
    I'm sure people in the psychiatry will love it.

19:38 < libertyprime>>
    It integrates with any emacs buffer.

19:38 < libertyprime>
    You can generate both imaginary and real URLs from selected text in emacs,
    and visit them, even if they are not real.

19:38 < libertyprime>
    And they're very coherent.

19:39 < libertyprime>
    the interactivity of emacs makes it powerful.

19:40 < a>
    imaginary bufferes in an imaginary emacs...

19:40 < libertyprime>
    it's the combination of real and imaginary that makes it powerful. i try to
    keep a real emacs and imagine the contents.

19:41 < libertyprime>
    emacs is becoming something like an intelligible scaffolding.

19:41 < a>
    somehow sounds like selling clowds to windows users.

19:43 < b>
    I didn't understand what it is trying to achieve from the first screencast.

19:43 < libertyprime>
    lets just say this is timely with the release of matrix 4. i have spent a
    lot of time just thinking of the implications of this technology as its
    improving. gpt4 is out soon and it may be an order of magnitude more
    powerful than codex -- whatever that means.

19:44 < libertyprime>
    the text is so coherent that it appears to be real but it is not. you can
    imaginae any website you can think of -- even ascii art websites. wikipedia
    articles, lesswrong articles, about any topic.

19:44 < b>
    I feed it some text from a web page and it generates more. In this case it
    is a web page it could just as well be anything else, like we have seen
    examples of chats.

19:44 < libertyprime>
    but it's like an interactive fiction.

19:44 < b>
    libertyprime: ah, so you're trying to show how coherent the performance is
    with url + content?

19:44 < libertyprime>
    So you can interact with it and if you want to see a counter-argument to
    what you're reading, you can tweak the articule as such.

19:45 < libertyprime>
    c: not just that, but you can generate new URLs, etc. from any
    text in any emacs buffer, in any context

19:45 < libertyprime>
    It Replaces google, basically.

19:45 < b>
    libertyprime: I don't understand how it replaces google.

19:46 < libertyprime>
    i demonstrate how it replaces google in the screencasts that follow.

19:46 < b>
    Say I have this #emacs buffer. Would you mind running a scenario that
    replaces google for me?

19:46 < libertyprime>
    yes you are reading some code.

19:46 < b>
    Okay, second screencast?

19:46 < libertyprime>
    And you select some text, and imagine some URLs for that text. it will come
    up with some very nice suggestions. some of which are actually real website.

19:47 < libertyprime>
    Then you can tweak that list. maybe you want a blog article from your
    favourite blogger.

19:47 < libertyprime>
    instead of what it gave you

19:47 < libertyprime>
    then it imagines the website that follows. its very accurate too

19:48 < libertyprime>
    You imagine a set of continuations with a small continuation size, if you
    want, then you can cherrypick the continuation you want and generate more
    of the website

19:48 < libertyprime>
    It's interactive.

19:48 < libertyprime>
    At any stage you can select any text and generate more urls.

19:48 < b>
    I'm not sure that's how I use google. I generally ask google things like.

    "emacs modus-themes org tables alignment"

19:49 < b>
    It finds out web pages talking about this and I click on them one by one to
    find relevant info

19:49 < libertyprime>
    you could generate websites for that too. just generate a url with that
    query.

19:49 < libertyprime>
    Or you can also use your query as the input for the selection if you want.

19:49 < libertyprime>
    It also asks for verification of your input.

19:49 < libertyprime>
    So you can put whatever query you want in there.

19:50 < b>
    I see. Since it generates URLs for me for a given text, I could given it a
    long paragraph instead of these silly keywords and it may generate a URL
    for me. This URL may then turn out to be accurate.

19:50 < c>
    I don't get it, what's the point?

19:50 < b>
    Am I getting this right?

19:51 < b>
    d: you're asking the same thing as I am, but more directly ;)

19:51 < c>
    Yes, that's how I roll.

19:51 < libertyprime>
    It's an imaginary world wide web that can create fictional, but often
    factual websites.

19:51 < b>
    s/as I am/as am I

19:51 < c>
    Like, is this supposed to be funny?

19:51 < b>
    Cannot replace google for me, I want factual things!

19:51 < c>
    Auto-generated entertainment?

19:52 < libertyprime>
    c: you literally only need to validate with 404 on the generated
    URLs

19:52 < libertyprime>
    And im in the process of truthizing the return URLs.

19:52 < libertyprime>
    It already returns real websites. you just have to filter them.

19:52 < b>
    libertyprime: very interesting in some aspects!

19:52 < libertyprime>
    Its more than very interesting.  Its the future of web browsing.  That or
    blockchain internet.

19:53 < c>
    Why would you say that it's the future of browsing?

19:53 < libertyprime>
    Because it doesnt restrict you.

19:53 < c>
    Are you suggesting that in the future we need to do even more judicious
    filtering to find the actually interesting content?

19:53 < libertyprime>
    You can not only visit what you want, but you can read it in terms of your
    favourite blogger for example.

19:53 < b>
    I agree, in 5-10 years, perhaps OpenAI will come up with newer and newer
    approaches, directly changing the way we interface with the digital (and by
    extension of which, the physical) world

19:53 < a>
    The web is not about intersting content.

19:53 < c>
    Like, TV reaching lower and lower signal-to-noise ratio and the internet by
    extension, too?

19:53 < libertyprime>
    You can also browse an inferred future internet.

19:54 < libertyprime>
    I used it to read an interview by sam altman about gpt4.

19:54 < libertyprime>
    And then i tried GPT5.

19:54 < libertyprime>
    And it's actually quite precient.

19:54 < c>
    How would you know?

19:54 < libertyprime>
    it was interesting how it talked about transformers with infinite context.

19:54 < b>
    d: what I found promising was that I could feed it a long freaking
    paragraph and it would give me results. *IF* it was working "well", it
    would give me great results. Google cannot do this and they've been
    training us to use fucking keywords for decades

19:55 < libertyprime>
    A couple days there was an arxiv paper released about
    infinite-transformers.

19:55 < libertyprime>
    But the neural net im using was trained in 2019.

19:55 < b>
    There's also some work saying you don't need attention.

19:56 < a>
    2019 is not in the future.

19:56 < b>
    The model performed nearly as well as BERT without transformers.

19:56 < libertyprime>
    b: no but the technology that the imaginary article was talking
    about it described gpt-4 in terms of future capabilities

19:56 < libertyprime>
    My point is a 2019 model can infer the future.

19:56 < c>
    I somehow suspect that the whole GPT craze is some collective mass
    psychosis.

19:56 < b>
    b: do you want a neural network trained in future? Like SkyNet?

19:56 < c>
    Everyone's thought biases misfiring in the same way.

19:57 < libertyprime>
    And you are able to browse that future internet. my point that im making is
    in response to somebody's question as to the utility of an imaginary web.
    im describing it.

19:57 < libertyprime>
    By expressing that it can infer the future. and a person may want to take a
    probabilistic peek.

19:57 < c>
    Where people want to see the neural net do good and only look at the good
    outputs.

19:57 < b>
    d: these things keep showing us how little we know about _every
    single thing_ and I love them for it.

19:57 < a>
    What did the model from 2019 infer about today?

19:57 < libertyprime>
    I just said. that gpt-4 has infinite context. it's highly likely based on
    the last thing i said about inifite-former.

19:58 < b>
    "Well we thought we knew how languages work. But alas, this odd blackbox
    thing proves that we do not!"

19:58 < b>
    And so on.

19:58 < b>
    libertyprime: what does it mean to have infinite context?
    https://www.youtube.com/watch?v=0JlB9gufTw8&ab_channel=YannicKilcher
    infinite-memory former
    It means that it has 'sticky memories'.  And very long form coherence.

19:59 < b>
    Memories that stick forever?!

19:59 < b>
    huh

19:59 < libertyprime>
    Infinite long-form coherence. like a person has. That sounds like an
    advancement

19:59 < b>
    Did these computers not have solid state drives to store memories long
    term?

20:00 < libertyprime>
    Anyway. please upvote because i dont have a marketing team.
{{< /highlight >}}

-   Relevant to note in above:
    -   The imaginary internet allows you to peer into the future


## A word of caution! {#a-word-of-caution}

OpenAI is underplaying how transformative this
is, or simply do not know. They are heading to
creating an information bubble, and if too
many tools are made which are LM specific then
we will have closed societies. The future may
become a dystopia.