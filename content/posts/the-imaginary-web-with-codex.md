+++
title = "Browsing the imaginary web with Codex"
author = ["Shane Mulligan"]
date = 2021-09-09T00:00:00+12:00
keywords = ["codex", "openai", "emacs"]
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


## Related {#related}

-   [Search the web with Codex {bye Google} // Bodacious Blog](https://mullikine.github.io/posts/search-the-web-with-codex/)

The above prompt for searching and generating
URLs has been modified to remove the
constraint that the return URLs must be real.

{{< highlight yaml "linenos=table, linenostart=1" >}}
# This contraint is not needed for the imaginary internet
# validator: url-exists
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