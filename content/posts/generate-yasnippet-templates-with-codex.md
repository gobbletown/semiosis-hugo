+++
title = "Generate YASnippet templates with Codex"
author = ["Shane Mulligan"]
date = 2021-10-12T00:00:00+13:00
keywords = ["codex", "pen", "emacs"]
draft = false
+++

## Summary {#summary}

I demonstate snippet generation using Codex.


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate a YASnippet"
doc: "Given a language and a feature, generate a snippet"
prompt-version: 1
prompt: |+
  # This is a snippet template example of a <q:name> in <mode>:
  cd ~/.emacs.d/yasnippet/snippets/<sl:mode>
  cat <sl:mode>_<sl:name> <<EOD
  <:pp># -*- mode: snippet -*-
  # name: <sl:name>
  # --
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 200
top-p: 1.0
stop-sequences:
- EOD
cache: on
vars:
- mode
- name
examples:
- "haskell"
- "catamorphism"
filter: off
completion: off
insertion: on
{{< /highlight >}}


## Demo {#demo}


### Haskell {#haskell}

`main`

{{< highlight snippet "linenos=table, linenostart=1" >}}
# -*- mode: snippet -*-
# name: main
# --
main = do
    -- put your code here
{{< /highlight >}}


### Python {#python}

`for`

{{< highlight snippet "linenos=table, linenostart=1" >}}
for ${1:i} in ${2:range($3)}:
    $0
{{< /highlight >}}

`while`

{{< highlight snippet "linenos=table, linenostart=1" >}}
while ${1:condition}:
    ${0:pass}
{{< /highlight >}}

`class`

{{< highlight snippet "linenos=table, linenostart=1" >}}
class ${1:NAME}:
    def __init__(self, ${2:arguments}):
        self.${3:state} = ${2:arguments}

    def __str__(self):
        return '${1:NAME}(${2:state})'
{{< /highlight >}}


### Full Python snippets {#full-python-snippets}

{{< highlight snippet "linenos=table, linenostart=1" >}}
# -*- mode: snippet -*-
# name: class
# --
class $1(object):

    def __init__(self, $2):
        $0
{{< /highlight >}}


## Java {#java}

`class`

{{< highlight snippet "linenos=table, linenostart=1" >}}
# -*- mode: snippet -*-
# name: class
# --
class ${1:ClassName} {
  public ${1:ClassName}() {
    // TODO: add your own switch statements for each variable
  }
}
{{< /highlight >}}


### Asciinema demo {#asciinema-demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/xui3Di5pyJzx43TMU5e52utqO" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/xui3Di5pyJzx43TMU5e52utqO.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/xui3Di5pyJzx43TMU5e52utqO.js" id="asciicast-xui3Di5pyJzx43TMU5e52utqO" async></script>