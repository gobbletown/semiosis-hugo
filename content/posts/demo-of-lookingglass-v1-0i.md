+++
title = "Demo of LookingGlass v1.0i"
author = ["Shane Mulligan"]
date = 2021-11-12T00:00:00+13:00
keywords = ["𝑖web", "pen", "lg"]
draft = false
+++

## Summary {#summary}

This is an early demo of the LookingGlass web browser.

I use a standalone Docker image (i.e. standalone browser command, `lg`).


## Browse non-existent wiki pages {#browse-non-existent-wiki-pages}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/1MkhkkSP2BVDsmqUKE2jXRjOI" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/1MkhkkSP2BVDsmqUKE2jXRjOI.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/1MkhkkSP2BVDsmqUKE2jXRjOI.js" id="asciicast-1MkhkkSP2BVDsmqUKE2jXRjOI" async></script>


## Prompts {#prompts}


### Generate html from ascii browser {#generate-html-from-ascii-browser}

Code
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-html-from-ascii-browser-2.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "generate html from ascii browser"
doc: "Given an ascii browser dump, generate some html"
prompt-version: 3
prompt: |-
    # Render the website:
    lynx --dump -nolist <q:url> <<EOD
    <ascii>
    EOD

    # The following is the html for the above web page
    curl -s <q:url> <<EOD
    <!DOCTYPE html>
    <!-- This page uses very simple html -->
    <!-- Allowed tags: h1 p ul li a -->
    <:pp><html>
    <head>
    <title>
engine: "OpenAI Codex"
temperature: 0.2
max-generated-tokens: "(* 1 prompt-length)"
frequency-penalty: 0.5
top-p: 1.0
n-collate: 1
n-completions: 5

closer: pf-continue-last
autoclose: on

stop-sequences:
- "EOD"
cache: on
vars:
- url
- ascii
postprocessor: "sed -z -e 's=\\(</html>\\).*=\\1=' -e 's= >=>=g'"
examples:
- |-
     Apple

     We look forward to welcoming you to our stores. Whether you shop in a
     store or shop online, our Specialists can help you buy the products
     you love. Shop with a Specialist, get credit with Apple Trade In,
     choose free delivery or pickup, and more at the Apple Store Online.
     Shop with a Specialist, get credit with Apple Trade In, choose free
     delivery or pickup, and more at the Apple Store Online.
info: on
completion: off
insertion: off
{{< /highlight >}}


### Imagine a website from a URL {#imagine-a-website-from-a-url}

Code
: <http://github.com/semiosis/prompts/blob/master/prompts/imagine-a-website-from-a-url-1.prompt>


## Designing a house {#designing-a-house}

Here I demonstrate interactively imagining and
designing a website, and image of a house with
Codex.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/F1uXYJn8BXcNUOCb9uYwL5ySr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/F1uXYJn8BXcNUOCb9uYwL5ySr.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/F1uXYJn8BXcNUOCb9uYwL5ySr.js" id="asciicast-F1uXYJn8BXcNUOCb9uYwL5ySr" async></script>

The next step is to use DALL-E to imagine the
images.


## Regular google search with `image-to-text` {#regular-google-search-with-image-to-text}

The web browser works like a regular browser, too.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/MBEZM8EwCK2VtDuYq8ww8X5zf" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/MBEZM8EwCK2VtDuYq8ww8X5zf.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/MBEZM8EwCK2VtDuYq8ww8X5zf.js" id="asciicast-MBEZM8EwCK2VtDuYq8ww8X5zf" async></script>


## Browse unavailable website {#browse-unavailable-website}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/MsI2s9TpwSinAhGEkL6WnWerT" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/MsI2s9TpwSinAhGEkL6WnWerT.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/MsI2s9TpwSinAhGEkL6WnWerT.js" id="asciicast-MsI2s9TpwSinAhGEkL6WnWerT" async></script>

-   Automatically visit imaginary when 404 or 502
-   Generate html from ascii, up to the `</html>`