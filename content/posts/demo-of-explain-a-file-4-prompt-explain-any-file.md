+++
title = "Demo of explain-a-file-4.prompt – Explain any file"
author = ["Shane Mulligan"]
date = 2021-11-11T00:00:00+13:00
keywords = ["openai", "codex", "pen"]
draft = false
+++

## Explain any file demo {#explain-any-file-demo}


### Question {#question}

I'm just getting started with this type of
prompt engineering so apologies for the noob
question. I do think is awesome. I'm trying to
use <https://github.com/semiosis/prompts/blob/master/prompts/explain-a-file-4.prompt> to
explain any github repo but I cannot get the
right completion. Wondering if you have any
examples I can reference to. Thanks!


### Answer {#answer}

I just ran the
<https://github.com/semiosis/prompts/blob/master/prompts/explain-a-file-4.prompt>
prompt on the following file:
<http://github.com/semiosis/pen.el/blob/master/docker-compose.yml>

As a result, the following text was generated as the final prompt:

{{< highlight text "linenos=table, linenostart=1" >}}
$ ls | head -n 10
CHANGELOG.org
config
CONTRIBUTING.org
docker-compose.yml
Dockerfile
docs
examples
glossary.txt
LICENSE
Makefile

$ ls ..
Barliman
dni
engines
examplary
fine-tuning-gpt-3
glossaries
glossaries-gh
huggingface-clj
huggingface.el
ilambda

$ cat "docker-compose.yml" <<EOD
version: "3.9"
services:
  pen:
    build: .
    ports:
      - "57575:57575"
  semantic:
    image: "docker.pkg.github.com/github/semantic/semantic:latest"
    restart: always
  duckling:
    image: rasa/duckling:0.1.6.5
    restart: always
    ports:
      - 8000:8000
EOD

$ cat "README for docker-compose.yml.md" <<EOD
# "docker-compose.yml"
## Explanation of code
{{< /highlight >}}

`Pen.el` then completes the above final prompt producing the following generated readme file:

{{< highlight text "linenos=table, linenostart=1" >}}
# "docker-compose.yml"
## Explanation of code
### Naming
- pen = the pen,
- semantic = the `semantic-ui` website,
- duckling = the duckling service,

This is the same file from `semantic-ui` and the `duckling` example.

### `semantic`
- `semantic` is a static website. It does not do anything on its own.
- It just serves static files from one or more sources.
- We can use it to serve the files from the `semantic-ui` github site:
<!--
    semantic:
      image: "docker.pkg.github.com/github/semantic/semantic:latest"
      restart: always
-->

### `duckling`
- `duckling` is an `rasa` service.
- The files are fetched from `rasa/duckling` which is the source code on github.
- See `rasa/duckling` in the `huggingface` repo.

- `docker run -p 8000:8000 rasa/duckling:0.1.6.5`

<!--
  duckling:
    image: rasa/duckling:0.1.6.5
    restart: always
    ports:
      - 8000:8000
-->

The static website runs
{{< /highlight >}}