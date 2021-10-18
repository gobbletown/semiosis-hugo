+++
title = "Demonstrating an LSP mode based on Codex with org-mode, and with code"
author = ["Shane Mulligan"]
date = 2021-10-18T00:00:00+13:00
keywords = ["codex", "pen", "openai", "gpt", "lsp"]
draft = false
+++

## Summary {#summary}

I just want to demonstrate what an LSP server
based on Codex looks like.


## `org-mode` {#org-mode}


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/pXUdukEJfh5qgq29KnZDb9SF0" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/pXUdukEJfh5qgq29KnZDb9SF0.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/pXUdukEJfh5qgq29KnZDb9SF0.js" id="asciicast-pXUdukEJfh5qgq29KnZDb9SF0" async></script>


## `awk` {#awk}

`awk` doesn't have a language server, as far as
I know. But we can add a Codex one.

I'm not going to be bothered right now with
tailoring prompts for awk.

This is just a proof of concept.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/t5uTLkI5gTfVPk4dChCEI6uYT" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/t5uTLkI5gTfVPk4dChCEI6uYT.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/t5uTLkI5gTfVPk4dChCEI6uYT.js" id="asciicast-t5uTLkI5gTfVPk4dChCEI6uYT" async></script>

All I've done here is copy the world-language
context for awk, but I could easily customize
it.


## Room for improvement {#room-for-improvement}

-   Add prompt multiplexing
-   Prompting parallelism
    -   Multiple Pen.el LSP servers
-   p2p prompting, prompt caching, etc.
-   One mode -- Codex-mode, for all languages.