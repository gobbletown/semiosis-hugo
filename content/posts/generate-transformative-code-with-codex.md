+++
title = "Generate transformative code with Codex"
author = ["Shane Mulligan"]
date = 2021-10-18T00:00:00+13:00
keywords = ["codex", "pen", "emacs"]
draft = false
+++

## Summary {#summary}

I demonstrate the generation of code which
transforms input into output, using Codex.

I am building a system of creating pipelines,
in the style of Jupyter notebooks, but using
Codex to generate parts of the pipeline.


## Future code {#future-code}

-   <http://github.com/semiosis/org-pen>


## Prompt {#prompt}

-   <http://github.com/semiosis/prompts/blob/master/prompts/generate-transformative-code.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate transformative code"
doc: "Given input, output and a tool, generate transformative code"
prompt-version: 1
notes:
- '"into the style of" is important here, because otherwise the output needs to be the exact output I already need.'
- 'With "into the style of", the prompt generalises a bit more.'
prompt: |+
    * Here is some <sl:input lang>

    Example 1:

    #+BEGIN_SRC <sl:input lang>
    <input>
    #+END_SRC

    * Here is what I want the <input lang> to look like

    Example 2:

    #+BEGIN_SRC <sl:output lang>
    <output code>
    #+END_SRC

    * This is the <tool> to change example-1.<sl:input lang> into the style of example-2.<sl:output lang>

    #+BEGIN_SRC
    cat example-1.<sl:input lang> | <:pp><tool>
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: prompt-length
top-p: 1.0
stop-sequences:
- "#+END_SRC"
cache: on
n-completions: 10
vars:
- "input"
- "input lang"
- "output code"
- "output lang"
- "tool"
examples:
- |
    [
      "\n\nWhat went wrong?\n\nPossible Issue:\n\nThe sass executable has not been copied into the project. To get sass to work, run the following code:\n\nsudo gem install sass\n\n\n\nThen,",
      "\n\nThe output should be:\n\n{ \"status\": \"success\", \"data\": { \"id\": \"1\", \"name\": \"Test Name 1\", \"content\": \"Hello World\", \"updated_at\": \"2014-08",
      "",
      "\n\nThe output should be formatted so it looks like the image below.\n\nWhat code should be used?\n\nThe code should be written in C++. The functions randint(a,b) and srand(x) should",
      "\n\nWhat do you want to happen?\n\nCreate a feature branch with a failing test-case.\n\nPush the branch.\n\nOpen a pull request.\n\nA CI system will run the failing test-case and report that"
    ]
- "json"
- |
    {
      "jsonrpc": "2.0",
      "id": 10,
      "result": [
        {
          "label": "directory-exists?"
        },
        {
          "label": "nack-guard-evt"
        },
        {
          "label": "procedure->method"
        }
      ]
    }
- "json"
- "jq"
info: on
completion: off
insertion: off
{{< /highlight >}}


## Output {#output}

Generate `jq`.


### `jq` {#jq}

These look like valid results.

{{< highlight bash "linenos=table, linenostart=1" >}}
# jq 'map(. + {label: .})' # This one was bad
jq '[.[] | {label: .}]'
jq '.result = [.result[] | {label: .}]'
jq 'map({label: .[0]})'
{{< /highlight >}}

Works good:

{{< highlight bash "linenos=table, linenostart=1" >}}
echo "What should the output look like?" | ci pena pf-generic-completion-50-tokens/1 | jq '[.[] | {label: .}]'
{{< /highlight >}}

```bash
[
  {
    "label": "\n\nWhat went wrong?\n\nPossible Issue:\n\nThe sass executable has not been copied into the project. To get sass to work, run the following code:\n\nsudo gem install sass\n\n\n\nThen,"
  },
  {
    "label": "\n\nThe output should be:\n\n{ \"status\": \"success\", \"data\": { \"id\": \"1\", \"name\": \"Test Name 1\", \"content\": \"Hello World\", \"updated_at\": \"2014-08"
  },
  {
    "label": ""
  },
  {
    "label": "\n\nThe output should be formatted so it looks like the image below.\n\nWhat code should be used?\n\nThe code should be written in C++. The functions randint(a,b) and srand(x) should"
  },
  {
    "label": "\n\nWhat do you want to happen?\n\nCreate a feature branch with a failing test-case.\n\nPush the branch.\n\nOpen a pull request.\n\nA CI system will run the failing test-case and report that"
  }
]
```

These are LSP completions and the `jq` will go into the LSP server.


### `python` {#python}

Not as good results with Python.

However, I bet that if I were to put the
results of the `jq` generation ion the prompt
it would improve the python generation.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/1tsxgQEc2En8xzWBCIjNgItzV" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/1tsxgQEc2En8xzWBCIjNgItzV.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/1tsxgQEc2En8xzWBCIjNgItzV.js" id="asciicast-1tsxgQEc2En8xzWBCIjNgItzV" async></script>