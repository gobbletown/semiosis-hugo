+++
title = "Imagine a project with Codex"
author = ["Shane Mulligan"]
date = 2021-10-13T00:00:00+13:00
keywords = ["codex", "gpt", "openai", "emacs", "pen"]
draft = false
+++

## Summary {#summary}

I work on prompts for imagining what's inside
of a directory, and on imagining the contents
of those files, while building the
functionality tightly into emacs.

This is useful for scaffolding new projects
(or maybe imagining the Colonel's 11 secret
herbs & spices and other secret recipes).


## Generated project {#generated-project}

-   <http://github.com/mullikine/codex-scaffolding-demo>


## Demo {#demo}

Codex imagines the file structure and the contents of the files inside of a new, empty project.
scaffolding them.


## elisp {#elisp}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-find-file (path)
  "Create directories and edit file"
  (pen-snc (cmd "mkdir" "-p" (f-dirname path)))
  (if (re-match-p "/$" path)
      (progn
        (pen-snc (cmd "mkdir" "-p" path))
        (find-file path))
    (find-file path)))
{{< /highlight >}}


## Prompts {#prompts}

See also
: <https://semiosis.github.io/posts/generate-new-files-with-codex-and-pen-el/>


`pf-recurse-current-directory/3`
: <http://github.com/semiosis/prompts/blob/master/prompts/recurse-subdirectory-4.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "recurse current directory"
doc: "imaginarily recurse the current directory"
prompt-version: 3
todo:
- Make a more general version of this
notes:
- parent was removed because for short listings, it was interfering
# Don't include this necessarily
subprompts:
  - dirs: |-
        $ ls -d */ # list directories here
        <ls dirs here output>

        $ find . -type d -maxdepth 1 # list directories here
        <find dirs here output>

prompt: |+
  <dirs>

  $ pwd
  <working directory>

  # recursively search for files in this directory
  $ find . -maxdepth 5
  <:pp>./
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
stop-sequences:
- "\n\n"
cache: on
n-completions: 10
vars:
- "working directory"
- "ls dirs here output"
- "find dirs here output"
var-defaults:
- "(pen-snc \"pwd\")"
- "(pen-snc \"ls -d */ | head -n 5\")"
- "(pen-snc \"find . -type d -maxdepth 1\")"
examples:
- "/home/shane/source/git/mobile-shell/mosh"
- "secret documents"
- |-
    drwxrwxr-x   2 shane shane     4096 Oct 13 19:34 contracts
    -rw-rw-r--   1 shane shane      816 Oct 13 02:59 README.org
    -rw-rw-r--   1 shane shane 10166273 Oct 13 02:59 tags
    drwxrwxr-x 271 shane shane    12288 Oct 13 01:08 node_modules
    -rw-r--r--   1 shane shane   262758 Oct 13 01:08 package-lock.json
- |-
    .
    ./.git/
    ./.github
    ./src/
    ./test/
info: on
completion: off
insertion: off
validator: grep -qP "^\\./"
# must come after validator
postpostprocessor: sed 's/^\.\///' | sed -e '/^$/d' -e '/^\.$/d'
split-patterns:
- "\n"
# These are defined after the prompt has executed
action: pen-find-file
{{< /highlight >}}

`pf-recurse-current-directory/2`
: <http://github.com/semiosis/prompts/blob/master/prompts/recurse-current-directory-2.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "recurse current directory"
doc: "imaginarily recurse the current directory"
prompt-version: 3
todo:
- Make a more general version of this
notes:
- parent was removed because for short listings, it was interfering
# Don't include this necessarily
subprompts:
  - dirs: |-
        $ find . -type d -maxdepth 1 # list directories here
        <ls dirs here output>

prompt: |+
  <dirs>

  $ pwd
  <working directory>

  # recursively search for files in this directory
  $ find . -maxdepth 5
  <:pp>./
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
stop-sequences:
- "\n\n"
cache: on
n-completions: 10
vars:
- "working directory"
- "ls dirs here output"
var-defaults:
- "(pen-snc \"pwd\")"
- "(pen-snc \"find . -type d -maxdepth 1\")"
examples:
- "/home/shane/source/git/mobile-shell/mosh"
- "secret documents"
- |-
    drwxrwxr-x   2 shane shane     4096 Oct 13 19:34 contracts
    -rw-rw-r--   1 shane shane      816 Oct 13 02:59 README.org
    -rw-rw-r--   1 shane shane 10166273 Oct 13 02:59 tags
    drwxrwxr-x 271 shane shane    12288 Oct 13 01:08 node_modules
    -rw-r--r--   1 shane shane   262758 Oct 13 01:08 package-lock.json
- |-
    .
    ./.git/
    ./.github
    ./src/
    ./test/
info: on
completion: off
insertion: off
validator: grep -qP "^\\./"
# must come after validator
postpostprocessor: sed 's/^\.\///' | sed -e '/^$/d' -e '/^\.$/d'
split-patterns:
- "\n"
# These are defined after the prompt has executed
action: pen-find-file
{{< /highlight >}}

`pf-generate-the-contents-of-a-new-file/5`
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-the-contents-of-a-new-file-5.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Generate the contents of a new file"
doc: "Prompt for the probable contents of a file with this path and sibling files"
prompt-version: 1
subprompts:
  - dirs: |-
        $ ls -d */ # list directories here
        <ls dirs here output>

prompt: |+
    $ pwd
    <working directory>

    $ ls -a
    <ls output>

    <dirs>

    $ cat <q:filename> <<EOD
    <preceding text>
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 150
top-p: 1.0
# This means that even a long-completion can't change the stop-sequence
force-stop-sequence: EOD
stop-sequences:
- EOD
cache: on
vars:
- "preceding text"
- "filename"
- "working directory"
- "ls output"
- "ls dirs here output"
var-defaults:
- "(pen-preceding-text)"
- "(f-basename (get-path))"
- "(pen-snc \"pwd\")"
- "(pen-snc \"ls -a\")"
- "(pen-snc \"ls -d */ | head -n 5\")"
examples:
- ""
- ".gitignore"
- "/home/mullikine/codex-scaffolding-demo"
- ".\nLICENSE.md"
- |-
    contracts/
    migrations/
    node_modules/
    src/
    test/
filter: off
info: off
completion: on
insertion: on
{{< /highlight >}}