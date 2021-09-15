+++
title = "The Codex Quine"
author = ["Shane Mulligan"]
date = 2021-08-12T00:00:00+12:00
keywords = ["codex", "openai", "symbology"]
draft = false
+++

OpenAI Demo
: [Converting Python to Ruby with OpenAI Codex - YouTube](https://www.youtube.com/watch?v=Iq3rDFZOorw)


Pen.el Engine
: <http://github.com/semiosis/engines/blob/master/engines/openai-codex.engine>


Pen.el Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/transpile-3.prompt>


## Summary {#summary}

I convert the OpenAI quine relay into some
imaginary code (`WIP`).

{{< figure src="/ox-hugo/Chrysopoea_of_Cleopatra_1.png" >}}

{{< highlight text "linenos=table, linenostart=1" >}}
ouroboros
uroboros
    An ancient symbol depicting a serpent or
    dragon eating its own tail.

    Originating in ancient Egyptian
    iconography, the ouroboros entered Western
    tradition via Greek magical tradition and
    was adopted as a symbol in Gnosticism and
    Hermeticism and most notably in alchemy.

ouroboros program
uroboros program
quine-relays
    The quine concept can be extended to
    multiple levels of recursion, originating
    "ouroboros programs", or quine-relays.

    This should not be confused with
    multiquines.

multiquine
biquine
    A multiquine consisting of 2 languages (or
    biquine) would be a program which:

    - When run, is a quine in language X.
    - When supplied with a user-defined
      command line argument, would print a
      second program in language Y.
    - Given the second program in language Y,
      when run normally, would also be a quine
      in language Y.
    - Given the second program in language Y,
      and supplied with a user-defined command
      line argument, would produce the
      original program in language X.
    - A biquine could then be seen as a set of
      two programs, both of which are able to
      print either of the two, depending on
      the command line argument supplied.
{{< /highlight >}}


## The Quine: Initial Python source {#the-quine-initial-python-source}

{{< highlight python "linenos=table, linenostart=1" >}}
#!/usr/bin/env python
#
# A program which calls OpenAI with its own source code.
#
# Implemented in two languages:
#
# In Python, uses the "http.client" and "subprocess" libraries and uses Python 3 semantics. In Ruby, uses httparty and system (does NOT use open3).

import http.client
import json
import subprocess

here = __file__

api_key = open('openai_api_key.txt').read().strip()
source = open(here).read() # Read our own source code

my_language = "python"
next_Language = "ruby"

# Print with trailing newline.
print("Hello from " + my_language + ". I'm going to rewrite myself in " next_language + ". Stand by...")

E_O_D = "E" + "O" + "D"  # Avoid the string literal here

# Make request to OpenAI to translate to next language

prompt = "## The program in " + my_language + "\n\ncat > program_in_" + my_language + " <<" + E_O_D + "\n" + source + "\n" + E_O_D "\n\n## The exact same program in " + next_language + "\n\ncat > program_in_" + next_language + " <<" + E_O_D + "\n"

authorization = "Bearer " + api_key

# Create the payload
payload = {
    "stop": E_O_D,
    "max_tokens": 1000,
    "temperature": 0,
    "prompt": prompt
}

# Dump payload to JSON
payload = json.dumps(payload)

conn = http.client.HTTPSConnection("api.openai.com")
conn.request("POST", "/v1/engines/davinci-codex/completions", payload, {
    "Content-type": "application/json",
    "Authorization": authorization
})

response = conn.getresponse()
translated = json.loads(response.read())['choices'][0]['text']

path = "program_in_" next_language + ".rb"
f = open(path, "w")
f.write(translated.strip())
f.write("\n")
f.close()

subprocess.call(["pygmentize", path])

# Now launch next iteration
subprocess.call([next_language, path])
{{< /highlight >}}


### Interesting features {#interesting-features}

{{< highlight python "linenos=table, linenostart=1" >}}
E_O_D = "E" + "O" + "D"  # Avoid the string literal here
{{< /highlight >}}

The string literal is most likely avoided to
prevent Codex from becoming disoriented during
the transpilation when it sees two 'EOD's.


## Pen.el {#pen-dot-el}


### Codex Engine {#codex-engine}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: OpenAI Codex
lm-command: "openai-complete.sh"
model: davinci-codex
modes:
- search
- classification
specialities:
- code
min-tokens: 64
max-tokens: 1000
{{< /highlight >}}


### Codex Transpilation Prompt {#codex-transpilation-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "transpile"
doc: "Given some code and a target language, transpile into that language"
prompt-version: 1
engine: openai-codex
prompt: |+
    ## The program in <from language>

    cat program_in_<from language> <<EOD
    <code>
    EOD

    ## The exact same program in <to-language>

    cat program_in<to language> <<EOD
n-generate: 5
temperature: 0
max-tokens: 1000
top-p: 1.0
best-of: 1
cache: on
vars:
- code
- "from language"
- "to language"
var-defaults:
- "(pen-selected-text)"
- "(pen-detect-language-ask)"
postprocessors:
- sed '$d'
examples:
- "print(\"Hello world with empathy\")"
- "Python"
- "Ruby"
n-test-runs: 5
n-collate: 1
n-completions: 10
new-document: yes
external-related:
- "https://www.youtube.com/watch?v=Iq3rDFZOorw"
{{< /highlight >}}


### The interactive function {#the-interactive-function}

{{< highlight text "linenos=table, linenostart=1" >}}
pf-transpile is an interactive function without a source file.

Signature
(pf-transpile &optional CODE FROM-LANGUAGE TO-LANGUAGE &key NO-SELECT-RESULT)

Documentation
transpile
Given some code and a target language, transpile into that language

path:
- /home/shane/source/git/spacemacs/prompts/prompts/transpile.prompt

external-related
:- https://www.youtube.com/watch?v=Iq3rDFZOorw

examples:
- print("Hello world with empathy")
- Python
- Ruby

var-defaults:
- (pen-selected-text)
- (pen-detect-language-ask)

Key Bindings
This command is not in any keymaps.

References
Could not find source file.

Find all references Functions used by pf-transpile

Debugging
Enable tracing
Disassemble Forget

;; Could not find source code, showing raw function object.
(lambda
  (&optional code from-language to-language &rest --cl-rest--)
  "transpile\nGiven some code and a target language, transpile into that language\n\npath:\n- /home/shane/source/git/spacemacs/prompts/prompts/transpile.prompt\n\nexternal-related\n:- https://www.youtube.com/watch?v=Iq3rDFZOorw\n\nexamples:\n- print(\"Hello world with empathy\")\n- Python\n- Ruby\n\nvar-defaults:\n- (pen-selected-text)\n- (pen-detect-language-ask)\n\n(fn &optional CODE FROM-LANGUAGE TO-LANGUAGE &key NO-SELECT-RESULT)"
  (interactive
   (list
    (if "(pen-selected-text)"
        (eval-string "(pen-selected-text)")
      (read-string-hist "code: " "print(\"Hello world with empathy\")"))
    (if "(pen-detect-language-ask)"
        (eval-string "(pen-detect-language-ask)")
      (read-string-hist "from language: " "Python"))
    (if nil
        (eval-string "")
      (read-string-hist "to language: " "Ruby"))))
  (let*
      ((no-select-result
        (car
         (cdr
          (plist-member --cl-rest-- ':no-select-result)))))
    (progn
      (let
          ((--cl-keys-- --cl-rest--))
        (while --cl-keys--
          (cond
           ((memq
             (car --cl-keys--)
             '(:no-select-result :allow-other-keys))
            (setq --cl-keys--
                  (cdr
                   (cdr --cl-keys--))))
           ((car
             (cdr
              (memq ':allow-other-keys --cl-rest--)))
            (setq --cl-keys-- nil))
           (t
            (error "Keyword argument %s not one of (:no-select-result)"
                   (car --cl-keys--))))))
      (cl-block pf-transpile
        (let
            ((is-interactive
              (interactive-p)))
          (pen-force-custom
           (cl-macrolet
               ((expand-template
                 (string-sym)
                 `(--> ,string-sym
                    (pen-onelineify it)
                    (pen-expand-template-keyvals it subprompts)
                    (pen-expand-template it vals)
                    (pen-expand-template-keyvals it var-keyvals-slugged)
                    (pen-expand-template-keyvals it var-keyvals)
                    (pen-unonelineify it))))
             (let*
                 ((do-pen-update
                   (pen-var-value-maybe 'do-pen-update))
                  (pen-sh-update
                   (or
                    (>=
                     (prefix-numeric-value current-global-prefix-arg)
                     4)
                    (pen-var-value-maybe 'pen-sh-update)
                    do-pen-update))
                  (cache
                   (and
                    (not do-pen-update)
                    (pen-var-value-maybe 'cache)))
                  (final-flags
                   (or
                    (pen-var-value-maybe 'flags)
                    nil))
                  (final-flags
                   (if final-flags
                       (mapconcat
                        (lambda
                          (s)
                          (concat "<" s ">"))
                        (vector2list final-flags)
                        " ")))
                  (final-is-info
                   (or
                    (pen-var-value-maybe 'do-etv)
                    (pen-var-value-maybe 'is-info)
                    nil))
                  (final-start-yas
                   (or
                    (pen-var-value-maybe 'start-yas)
                    nil))
                  (final-end-yas
                   (or
                    (pen-var-value-maybe 'yas)
                    (pen-var-value-maybe 'end-yas)
                    nil nil))
                  (subprompts nil)
                  (subprompts
                   (if subprompts
                       (ht->alist
                        (-reduce 'ht-merge
                                 (vector2list subprompts)))))
                  (final-prompt "## The program in <from language>\n\ncat program_in_<language> <<EOD\n<code>\nEOD\n\n## The exact same program in <to-language>\n\n")
                  (final-prompt
                   (if final-start-yas
                       (pen-yas-expand-string final-prompt)
                     final-prompt))
                  (vals
                   (mapcar 'str
                           (if
                               (not is-interactive)
                               (progn
                                 (cl-loop for sym in
                                          '(code from-language to-language)
                                          for iarg in
                                          '((if "(pen-selected-text)"
                                                (eval-string "(pen-selected-text)")
                                              (read-string-hist "code: " "print(\"Hello world with empathy\")"))
                                            (if "(pen-detect-language-ask)"
                                                (eval-string "(pen-detect-language-ask)")
                                              (read-string-hist "from language: " "Python"))
                                            (if nil
                                                (eval-string "")
                                              (read-string-hist "to language: " "Ruby")))
                                          collect
                                          (let*
                                              ((initval
                                                (eval sym)))
                                            (if
                                                (and
                                                 (not initval)
                                                 iarg)
                                                (eval iarg)
                                              initval))))
                             (cl-loop for v in
                                      '(code from-language to-language)
                                      until
                                      (eq v '&key)
                                      collect
                                      (eval v)))))
                  (vals
                   (cl-loop for tp in
                            (-zip-fill nil vals 'nil)
                            collect
                            (let*
                                ((v
                                  (car tp))
                                 (pp
                                  (cdr tp)))
                              (if pp
                                  (pen-sn pp v)
                                v))))
                  (final-prompt
                   (if nil
                       (if
                           (< 0
                              (length vals))
                           (concat
                            (pen-awk1 final-prompt)
                            (string-replace "{}"
                                            (str
                                             (car
                                              (last vals)))
                                            nil))
                         (concat
                          (pen-awk1 final-prompt)
                          nil))
                     final-prompt))
                  (var-keyvals
                   (-zip
                    '("code" "from language" "to language")
                    vals))
                  (var-keyvals-slugged
                   (-zip
                    '("code" "from-language" "to-language")
                    vals))
                  (final-n-collate
                   (or
                    (pen-var-value-maybe 'n-collate)
                    1))
                  (final-n-completions
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'n-completions)
                      10))))
                  (final-max-tokens
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'max-tokens)
                      1000))))
                  (final-temperature
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'temperature)
                      0))))
                  (final-mode
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'mode)
                      nil))))
                  (final-top-p
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'top-p)
                      1.0))))
                  (final-top-k
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'top-k)
                      nil))))
                  (final-stop-sequences
                   (cl-loop for stsq in
                            (or
                             (pen-var-value-maybe 'stop-sequences)
                             '("###<long>###"))
                            collect
                            (expand-template stsq)))
                  (final-stop-patterns
                   (or
                    (pen-var-value-maybe 'stop-patterns)
                    '("^Input:")))
                  (final-stop-sequence
                   (expand-template
                    (str
                     (or
                      (pen-var-value-maybe 'stop-sequence)
                      "###<long>###"))))
                  (final-prompt
                   (expand-template final-prompt))
                  (final-prompt
                   (pen-log-final-prompt
                    (if nil
                        (sor
                         (pen-snc nil final-prompt)
                         (concat "prompt-filter " nil " failed."))
                      final-prompt)))
                  (final-prompt
                   (if final-end-yas
                       (pen-yas-expand-string final-prompt)
                     final-prompt))
                  (final-prompt
                   (chomp final-prompt))
                  (prompt-end-pos
                   (or
                    (byte-string-search "<:pp>" final-prompt)
                    (string-bytes final-prompt)))
                  (final-prompt
                   (string-replace "<:pp>" "" final-prompt))
                  (final-prompt
                   (chomp final-prompt))
                  (shcmd
                   (pen-log
                    (s-join " "
                            (list
                             (sh-construct-envs
                              `(("PEN_PROMPT" ,(pen-encode-string final-prompt))
                                ("PEN_LM_COMMAND" ,"openai-complete.sh")
                                ("PEN_MODEL" ,nil)
                                ("PEN_MAX_TOKENS" ,final-max-tokens)
                                ("PEN_TEMPERATURE" ,final-temperature)
                                ("PEN_MODE" ,final-mode)
                                ("PEN_STOP_SEQUENCE" ,(pen-encode-string final-stop-sequence))
                                ("PEN_TOP_P" ,final-top-p)
                                ("PEN_TOP_K" ,final-top-k)
                                ("PEN_FLAGS" ,final-flags)
                                ("PEN_CACHE" ,cache)
                                ("PEN_N_COMPLETIONS" ,final-n-completions)
                                ("PEN_END_POS" ,prompt-end-pos)))
                             "lm-complete"))))
                  (resultsdirs
                   (cl-loop for i in
                            (number-sequence 1 final-n-collate)
                            collect
                            (progn
                              (message
                               (concat "pf-transpile" " query "
                                       (int-to-string i)
                                       "..."))
                              (let
                                  ((ret
                                    (pen-prompt-snc shcmd i)))
                                (message
                                 (concat "pf-transpile" " done "
                                         (int-to-string i)))
                                ret))))
                  (results
                   (-uniq
                    (flatten-once
                     (cl-loop for rd in resultsdirs collect
                              (if
                                  (sor rd)
                                  (->>
                                      (glob
                                       (concat rd "/*"))
                                    (mapcar 'e/cat)
                                    (mapcar
                                     (lambda
                                       (r)
                                       (cl-loop for stsq in final-stop-sequences do
                                                (let
                                                    ((matchpos
                                                      (pen-string-search stsq r)))
                                                  (if matchpos
                                                      (setq r
                                                            (s-truncate matchpos r "")))))
                                       r))
                                    (mapcar
                                     (lambda
                                       (r)
                                       (cl-loop for stpat in final-stop-patterns do
                                                (let
                                                    ((matchpos
                                                      (re-match-p stpat r)))
                                                  (if matchpos
                                                      (setq r
                                                            (s-truncate matchpos r "")))))
                                       r))
                                    (mapcar
                                     (lambda
                                       (r)
                                       (if
                                           (and nil
                                                (sor nil))
                                           (pen-sn nil r)
                                         r)))
                                    (mapcar
                                     (lambda
                                       (r)
                                       (if
                                           (and
                                            (variable-p 'prettify)
                                            prettify nil
                                            (sor nil))
                                           (pen-sn nil r)
                                         r)))
                                    (mapcar
                                     (lambda
                                       (r)
                                       (if
                                           (not nil)
                                           (s-trim-left r)
                                         r)))
                                    (mapcar
                                     (lambda
                                       (r)
                                       (if
                                           (not nil)
                                           (s-trim-right r)
                                         r))))
                                (list
                                 (message "Try UPDATE=y or debugging")))))))
                  (result
                   (if no-select-result
                       (length results)
                     (cl-fz results :prompt
                            (concat "pf-transpile" ": ")
                            :select-only-match t))))
               (if no-select-result results
                 (if is-interactive
                     (cond
                      ((or final-is-info
                           (>=
                            (prefix-numeric-value current-prefix-arg)
                            4))
                       (etv result))
                      ((and nil mark-active)
                       (if
                           (sor result)
                           (replace-region result)
                         (error "pen filter returned empty string")))
                      ((or nil nil)
                       (insert result))
                      (t
                       (etv result)))
                   result))))))))))

Symbol Properties
event-symbol-element-mask
  (pf-transpile 0)
event-symbol-elements
  (pf-transpile)
modifier-cache
  ((0 . pf-transpile))
{{< /highlight >}}