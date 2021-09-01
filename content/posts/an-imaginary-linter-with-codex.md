+++
title = "An imaginary linter with Codex"
author = ["Shane Mulligan"]
date = 2021-09-01T00:00:00+12:00
keywords = ["awk", "openai", "gpt", "codex", "pen"]
draft = false
+++

## Summary {#summary}

I demonstrate an imaginary linter with Codex.
This predicts an error message for a line of
awk code. However, it also infers when the
code is correct, so it's useful.

The accuracy is good enough to make a reliable
imaginary linter.


## Demo {#demo}


### Correctly identifies syntax error 7 times out of 20 {#correctly-identifies-syntax-error-7-times-out-of-20}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/d6k4Ez4aJz4VVezjw256WqDpp" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/d6k4Ez4aJz4VVezjw256WqDpp.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/d6k4Ez4aJz4VVezjw256WqDpp.js" id="asciicast-d6k4Ez4aJz4VVezjw256WqDpp" async></script>

{{< highlight text "linenos=table, linenostart=1" >}}
6:   }
5:         ^ syntax error
6:     }
5:         ^ syntax error
5:         ^ syntax error
5:         ^
6:     }
5:         ^ syntax error
6:     }
6:   }
6:   }
6:   }
6:     }
6:   }
6:   }
5:         ^ syntax error
6:   }
5:         ^ syntax error
6:   }
5: ^ syntax error
{{< /highlight >}}


### Correctly identifies correct code 19 times out of 20 {#correctly-identifies-correct-code-19-times-out-of-20}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/ttlPidLbP1uOqvqHayYDLomcL" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/ttlPidLbP1uOqvqHayYDLomcL.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/ttlPidLbP1uOqvqHayYDLomcL.js" id="asciicast-ttlPidLbP1uOqvqHayYDLomcL" async></script>

{{< highlight text "linenos=table, linenostart=1" >}}
6:   }
6:   }
6:   }
6: }
6:   }
6:   }
6:   }
6:   }
6: }
6:   }
6:   }
6:   }
6:   }
6: }
6:   }
6:     }
6:   }
6:   }
6:   }
5:         ^ syntax error
{{< /highlight >}}


## Prompt {#prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Imagine an awk linter"
doc: "Given a line of awk code, check it for errors"
prompt-version: 1
prompt: |+
    awk -f program.awk
    awk: /path/to/file:1: if NR >= window_size && (effective_nr % step == 0) {
    awk: /path/to/file:1: ^ syntax error
    awk: /path/to/file:2:   for (i=1; i<window_size; i++) {
    awk: /path/to/file:3:     s = s memory[i] RS
    awk: /path/to/file:4:   }
    awk: /path/to/file:5: <line of code>
    awk: /path/to/file:
engine: "OpenAI Codex"
temperature: 0.2
max-tokens: 60
top-p: 1.0
cache: on
stop-sequences:
- "\n"
vars:
- "line of code"
examples:
- "        close cmd, \"to\""
preprocessors:
- "cat"
filter: yes
completion: off
insertion: off
{{< /highlight >}}