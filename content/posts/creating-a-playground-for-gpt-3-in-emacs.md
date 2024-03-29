+++
title = "Creating a playground for GPT-3 in emacs"
author = ["Shane Mulligan"]
date = 2021-02-18T00:00:00+13:00
keywords = ["gpt", "codecraft", "emacs", "openai", "prompt-engineering", "pen"]
draft = false
+++

_This is outdated._

Code
: <https://github.com/semiosis/pen.el>


Prompts
: <https://github.com/semiosis/prompts>


`meetup.com` event
: <https://www.meetup.com/Code-Craft-Dunedin/events/276407816/>


Slides
: <http://github.com/mullikine/presentation-prompt-engineering-in-emacs/blob/master/presentation.pdf>

| Latest docs (12.08.21)          |                                                                                                      |
|---------------------------------|------------------------------------------------------------------------------------------------------|
| GitHub project                  | <https://github.com/semiosis/pen.el/>                                                                |
| Project timeline and objectives | <https://github.com/semiosis/pen.el/tree/master/docs>                                                |
| Prompts README                  | <http://github.com/semiosis/prompts/blob/master/README.org>                                          |
| ★ Tutorial                      | <https://semiosis.github.io/posts/pen-tutorial/>                                                     |
| Video demo of `Pen.el`          | [Augment Minds 2021: Demo of Loom and Pen.el - YouTube](https://www.youtube.com/watch?v=J9BnZjWV1jw) |
| Bash prompting README           | <http://github.com/semiosis/pen.el/blob/master/docs/using-prompt-functions-in-bash.org>              |


### Demonstration (2021-02-18) {#demonstration--2021-02-18}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/t7ATnFpnfzBp0yicIlGCt6eXi" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/t7ATnFpnfzBp0yicIlGCt6eXi.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/t7ATnFpnfzBp0yicIlGCt6eXi.js" id="asciicast-t7ATnFpnfzBp0yicIlGCt6eXi" async></script>


## **Prompt-Engineering Part 1:** Building an environment {#prompt-engineering-part-1-building-an-environment}


### Summary of talk {#summary-of-talk}

I received a key for the `OpenAI` API 2 weeks ago, just a couple
of days before my birthday.

I had applied around 5 to 10 times in the last
year and I had finally been given a key.

I decided since the hour is late I would work
on productivity tools that anyone can use, to
facilitate programming in the new paradigm.


### `pen.el` : Prompt engineering in emacs {#pen-dot-el-prompt-engineering-in-emacs}


#### `prompt-engineer-mode` {#prompt-engineer-mode}

Prompt-Engineer Mode is a global minor mode for emacs that facilitates the
creation, development, discovery and usage of prompts in your emacs.

-   Create elisp functions based on GPT-3 prompts
-   Chain GPT-3 queries together using keyboard macros and functions
-   Interactively query, generate and transfrom both prose and code
-   Use GPT-3 as a search engine within emacs
    -   Search the internet
    -   Search documents
        -   <https://beta.openai.com/docs/introduction/semantic-search>
        -   <https://gpttools.com/semanticsearch>


### Works with {#works-with}

-   OpenAI API (GPT-3)
    -   <https://beta.openai.com/>


#### Planned {#planned}

-   `openai-server` (GPT-2)
    -   <https://github.com/shawwn/openai-server>
    -   Requires tensorflow 1
-   DeepAI API
    -   <https://deepai.org/>
-   OpenAI API (GPT-4)
    -   <https://beta.openai.com/>


### Usage / Workflow {#usage-workflow}

-   Enable `prompt-engineerer-mode` globally (on all buffers)
-   Have your `.prompt` file open in one window
-   Have the document open you want to apply prompt functions to
    -   This could be source code, a chat window, or any kind of text document.
-   Apply functions to do all sorts of things
    -   Summarization
    -   Generation of prose
    -   Generation of code
    -   Translation


### Defining a `YAML` format {#defining-a-yaml-format}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "complicated explanation of how to X"
prompt: |+
  The human asks the AI to make a simple description more complicated. The AI responds by writing a verbose response full of jargon to explain a simple concept.

  Human: explain how to use a drinking fountain.

  AI: To activate the mechanized dihydrogen monoxide dispensary service station, use the flanges to provide a downward force to the simple circular activation console and continue to apply force until the pneumatic system begins operating as intended, which can be surmised at a time when the dihydrogen monoxide travels in a parabolic arc which at is greatest exceeds three inches from the rounded excess liquid receptacle. Once hydration is completed, cease applying force to the console and regard whether or not the dispensary has ceased expelling contents as expected.

  Human: explain how to play with a dog

  AI: Being a typical domesticated animal, having been genetically selected for controllability, trainability, and general anthropomorphic features and functions, the typical dog possesses highly specialized and developed feeding behavior. The typical feeding behavior involves routine sustenance intake which can be observed by monitoring changes in animal size. Routine sustenance intake involves active and passive feeding habits, and one can gauge animal health by observing the relative levels of hydration. During training, reward conditioned reinforcement is applied. This involves the practice of rewarding targeted behaviors through the application of reinforcers, with the provision that the targeted behavior is observed. Relevant to the discussion at hand is the typical active and passive behavior exhibited by a typical dog.

  Human: explain how to <1>

  AI:
engine: "davinci"
temperature: 0.8
max-tokens: 100
top-p: 1
frequency-penalty: 0.2
presence-penalty: 0.4
best-of: 1
stop-sequences:
  - "\n"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
  - "passage"
{{< /highlight >}}


### Code generation {#code-generation}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-generate-prompt-functions ()
  "Generate prompt functions for the files in the prompts directory"
  (interactive)
  (let ((paths
         (glob (concat pen-prompt-directory "/*.prompt"))))
    (cl-loop for path in paths do
             ;; results in a hash table
             (let* ((yaml (yamlmod-read-file path))
                    (title (ht-get yaml "title"))
                    (title-slug (slugify title))
                    (vars (vector2list (ht-get yaml "vars")))
                    (var-slugs (mapcar 'slugify vars))
                    (var-syms (mapcar 'str2sym var-slugs))
                    (func-name (concat "pen-" title-slug))
                    (iargs (let ((iteration 0))
                             (cl-loop for v in vars do
                                      (progn
                                        (setq iteration (+ 1 iteration))
                                        (message (str iteration)))
                                      collect
                                      (if (equal 1 iteration)
                                          ;; The first argument may be captured through selection
                                          `(if (selectionp)
                                               (my/selected-text)
                                             (read-string-hist ,(concat v ": ")))
                                        `(read-string-hist ,(concat v ": ")))))))
               ;; var names will have to be slugged, too
               (eval
                `(defun ,(str2sym func-name) ,var-syms
                   (interactive ,(cons 'list iargs))
                   (etv (chomp (sn ,(flatten-once
                                     (list
                                      (list 'concat "openai-complete " (q path))
                                      (flatten-once (cl-loop for vs in var-slugs collect
                                                             (list " "
                                                                   (list 'q (str2sym vs))))))))))))
               (message (concat "pen-mode: Loaded prompt function " func-name))))))
{{< /highlight >}}


### Back-end {#back-end}


#### shell {#shell}

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

first_arg="$1"

stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

if stdin_exists; then
    # The stdin can be the first argument
    set -- "$@" "$(cat | chomp)"
fi

test -f "$first_arg" || exit
shift

prompt="$(cat "$first_arg" | yq -r ".prompt // empty")"
stop_sequence="$(cat "$first_arg" | yq ".\"stop-sequences\"[0] // empty" | uq | qne)"
temperature="$(cat "$first_arg" | yq -r ".\"temperature\" // empty")"
engine="$(cat "$first_arg" | yq -r ".\"engine\" // empty")"
max_tokens="$(cat "$first_arg" | yq -r ".\"max-tokens\" // empty")"
top_p="$(cat "$first_arg" | yq -r ".\"top-p\" // empty")"

test -n "$prompt" || exit 0

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
    "") { shift; }; ;;
    -e) {
        engine="$2"
        shift
        shift
    }
    ;;

    *) break;
esac; done

: "${engine:="ada"}"
: "${temperature:="0.6"}"
: "${max_tokens:="64"}"

: "${sub_completions:="1"}"

i=1
for var in "$@"
do
    var="$(printf -- "%s" "$var" | uq | chomp)"
    prompt="$(p "$prompt" | template -$i "$var")"
    ((i++))
done

prompt_fp="$(printf -- "%s" "$prompt" | chomp | tf)"

# printf -- "%s\n" "$prompt" | tv

prompt="$(p "$prompt" | qne)"

IFS= read -r -d '' SHCODE <<HEREDOC
openai api \
    completions.create \
    -e "$engine" \
    -t "$temperature" \
    -M "$max_tokens" \
    -n "$sub_completions" \
    $(
        if test -n "$stop_sequence"; then
            printf -- "%s" "--stop \"$stop_sequence\""
        fi
    ) \
    -p "$prompt"
HEREDOC

response_fp="$(eval "$SHCODE" | uq | s chomp | tf txt)"

prompt_bytes="$(cat "$prompt_fp" | wc -c)"
response_bytes="$(cat "$response_fp" | wc -c)"

tail -c +$((prompt_bytes + 2)) "$response_fp"
{{< /highlight >}}


### Additional reading {#additional-reading}

-   <https://www.overfit.ai/classroom-items/gpt-3-text-to-emoji>
-   <https://www.gwern.net/GPT-3>
-   <https://matthewmcateer.me/blog/messing-with-gpt-3/>
-   [#029 GPT-3, Prompt Engineering, Trading, AI Alignment, Intelligence - YouTube](https://youtu.be/fTvB5xMNfTY)
-   <https://github.com/mullikine/examplary>
-   <https://github.com/mullikine/prompt-engineer-mode>
-   <http://github.com/mullikine/fine-tuning-gpt-3/puns/>