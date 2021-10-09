+++
title = "Autofix code with Codex and LSP"
author = ["Shane Mulligan"]
date = 2021-10-10T00:00:00+13:00
keywords = ["codex", "pen", "openai"]
draft = false
+++

## Summary {#summary}

I make a prompt to autocorrect code based on
LSP linter output.


## Setup {#setup}

Firstly, lets get a list of algorithms.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(pps (ilist 10 "voting algorithms"))
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
("Plurality"
 "Borda"
 "Coombs"
 "Black"
 "Nanson"
 "Maxim"
 "Minimax"
 "Baldwin"
 "Copeland"
 "Instant-runoff"
 "Approval"
 "Kemeny-Young"
 "Instant-Runoff"
 "Maximin"
 "Maximizer")
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/FDnoqe5MgQ3ctjVylKSEbVm3U" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/FDnoqe5MgQ3ctjVylKSEbVm3U.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/FDnoqe5MgQ3ctjVylKSEbVm3U.js" id="asciicast-FDnoqe5MgQ3ctjVylKSEbVm3U" async></script>


### Generate first some code with errors/warnings {#generate-first-some-code-with-errors-warnings}

Codex is quite good at generating code, so it
may take some attempts.

I generated the `Instant-Runoff voting algorithm` using Codex.

I'll just force break the code.

{{< highlight yaml "linenos=table, linenostart=1" >}}
# Broken
def irv(ballots):
    candidates =
    for ballot in ballots:
        candidates |= set(ballot)
    while len(candidates) > 1:
        totals = dict((c, 0) for c in candidates)
        for ballot in ballots:
            for i, c in enumerate(ballot, sdjaf):
                totals[c] += 10 ** i
        min_total = min(totals.values())
        min_candidates = [c for c in candidates if totals[c] == min_total]
        candidates -= set(min_candidates)
        ballots = [ballot for ballot in ballots if ballot[0] not in min_candidates]
    return candidates.pop()
{{< /highlight >}}

Linter output:

{{< highlight text "linenos=table, linenostart=1" >}}
2: pyflakes: invalid syntax
2: pycodestyle: W291 trailing whitespace
13: pycodestyle: E501 line too long (83 > 79 characters)
{{< /highlight >}}

It will be good enough for a demo.


## Fixed code -- First 2 results {#fixed-code-first-2-results}

{{< highlight yaml "linenos=table, linenostart=1" >}}
def irv(ballots):
    candidates = set()
    for ballot in ballots:
        candidates |= set(ballot)
    while len(candidates) > 1:
        totals = dict((c, 0) for c in candidates)
        for ballot in ballots:
            for i, c in enumerate(ballot, 1):
                totals[c] += 10 ** i
        min_total = min(totals.values())
        min_candidates = [c for c in candidates if totals[c] == min_total]
        candidates -= set(min_candidates)
        ballots = [ballot for ballot in ballots if ballot[0] not in min_candidates]
    return candidates.pop()


# Fixed?
def irv(ballots):
    candidates = set()
    for ballot in ballots:
        candidates |= set(ballot)
    while len(candidates) > 1:
        totals = dict((c, 0) for c in candidates)
        for ballot in ballots:
            for i, c in enumerate(ballot):
                totals[c] += 10 ** i
        min_total = min(totals.values())
        min_candidates = [c for c in candidates if totals[c] == min_total]
        candidates -= set(min_candidates)
        ballots = [ballot for ballot in ballots if ballot[0] not in min_candidates]
    return candidates.pop()
{{< /highlight >}}


## Prompt {#prompt}

-   <http://github.com/semiosis/prompts/blob/master/prompts/autofix-code-2.prompt>

`pf-autofix-code/2`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Given the current error and the code snippet, fix the code snippet"
title: "autofix code"
prompt-version: 2
prompt: |+
  Questions (<language>)

  Help, my code is not compiling.

  This is the error I get:
  <delim>
  <compiler/linter output>
  <delim>

  # Code which is causing the error:
  <delim>
  <code segment>
  <delim>

  1 Answer

  Your code should look like the following:
  <delim>

engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: "(* 2 prompt-length)"
delimiter: "```"
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "compiler/linter output"
- "code segment"
defs:
- language: "(pen-detect-language)"
var-defaults:
- "(pen-list2str (pen-lsp-error-list))"
- "(pen-selection)"
examples:
- |+
    2: compile: Pattern match(es) are non-exhaustive
    2: typecheck: • Couldn't match expected type ‘[a]’ with actual type ‘[a] -> [a]’
    4: typecheck: • Occurs check: cannot construct the infinite type: a ~ [a]
- |+
    mergesort :: Ord a => [a] -> [a]
    mergesort [] ys = ys
    mergesort xs [] = xs
    mergesort (x:xs) (y:ys) | x <= y  = x : mergesort x
filter: off
info: on
completion: off
insertion: off
external-related:
- "https://stackoverflow.com/"
{{< /highlight >}}


## Emacs lisp {#emacs-lisp}


### Collect the error list from LSP {#collect-the-error-list-from-lsp}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-lsp-error-list (&optional path)
  (if (not path)
      (setq path (get-path)))
  (let ((l))
    (maphash (lambda (file diagnostic)
               (if (string-equal path file)
                   (dolist (diag diagnostic)
                     (-let* (((&Diagnostic :message :severity? :source?
                                           :range (&Range :start (&Position :line start-line))) diag)
                             (formatted-message (or (if source? (format "%s: %s" source? message) message) "???"))
                             (severity (or severity? 1))
                             (line (1+ start-line))
                             (face (cond ((= severity 1) 'error)
                                         ((= severity 2) 'warning)
                                         (t 'success)))
                             (text (concat (number-to-string line)
                                           ": "
                                           (car (split-string formatted-message "\n")))))
                       (add-to-list 'l text t)))))
             (lsp-diagnostics))
    l))
{{< /highlight >}}