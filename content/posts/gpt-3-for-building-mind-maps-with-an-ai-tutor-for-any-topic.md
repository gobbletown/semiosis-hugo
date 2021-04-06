+++
title = "GPT-3 mind maps with an AI tutor for any topic"
author = ["Shane Mulligan"]
date = 2021-03-29T00:00:00+13:00
keywords = ["GPT-3", "emacs", "learning", "openai", "NLP"]
draft = false
+++

Code
: <http://github.com/semiosis/pen.el>


Prompts
: <http://github.com/semiosis/prompts>


## Summary {#summary}

I combine GPT-3 with `org-brain` to expand on
topics, **suggesting** subtopics and providing an
interactive tutor for any topic.


## Demonstration {#demonstration}


### Subtopic generation {#subtopic-generation}

I demonstrate how to explore arbitrary topics
with GPT-3 by automatically generating
subtopics, and then allowing you to invoke the
GPT-3 tutor to answer questions within that
context.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/3D1xmyAB3wZiPMu3d7rnK8Izd" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/3D1xmyAB3wZiPMu3d7rnK8Izd.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/3D1xmyAB3wZiPMu3d7rnK8Izd.js" id="asciicast-3D1xmyAB3wZiPMu3d7rnK8Izd" async></script>


### Tutor demonstration {#tutor-demonstration}

-   Rolling conversation is a work in progress, but on its way.

<span class="underline">**Learn about AI**</span>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx.js" id="asciicast-tV37yuypzU8C4ttDL4w24HOtx" async></script>

<span class="underline">**Learn about microbiology**</span>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ.js" id="asciicast-R25hFKsdKc1wcfbMGeXnXa0iJ" async></script>


## Code {#code}


### Subtopic prompt {#subtopic-prompt}

{{< highlight yaml "linenos=table, linenostart=1" >}}
title: "subtopic generation"
prompt: |+
    The following is a list of subtopics relating to microbiology:
    - Bacteriology
    - Mycology
    - Protozoology
    - Phycology/algology
    - Parasitology
    - Immunology
    - Virology
    - Nematology
    ###
    The following is a list of subtopics relating to natural language processing / NLP:
    - extractive question answering
    - language modelling
    - named entity recognition
    - sequence classification
    - summarization
    - text generation
    - topic modelling
    - translation
    ###
    The following is a list of subtopics relating to language modelling in NLP:
    - casual language modelling
    - masked language modelling
    - gext generation
    ###
    The following is a list of subtopics relating to <1>:
    -
engine: "davinci"
temperature: 0.8
max-tokens: 60
top-p: 1
frequency-penalty: 0.8
presence-penalty: 0.0
best-of: 1
stop-sequences:
- "###"
chomp-start: on
chomp-end: off
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
# Cache the function by default when running the prompt function
cache: on
vars:
- "topic"
examples:
- "Advanced Type Systems in Haskell"
# External provides an alternate script that performs the same function
# external:
# - "extract-keyphrases"
{{< /highlight >}}


### Tutor prompt {#tutor-prompt}

{{< highlight python "linenos=table, linenostart=1" >}}
title: "Generic tutor for any topic"
prompt: |+
    This is a conversation between a human and a brilliant AI.
    The topic is "<1>".

    Human: Hello, are you my <2> tutor?
    ###
    AI: Hi there. Yes I am. How can I help you today?
    ###
    Human: What questions can I ask you about <1>?
    ###
    AI: You may ask me anything relating to <2>.
    ###
    Human: OK then. <3>
    ###
    AI: I would be happy to answer your question.
engine: "davinci"
# 0.0 = /r/hadastroke
# 1.0 = /r/iamveryrandom
# Use 0.3-0.8
temperature: 0.8
max-tokens: 60
top-p: 1.0
# Not available yet: openai api completions.create --help
frequency-penalty: 0.5
# If I make presence-penalty 0 then it will get very terse
presence-penalty: 0.0
best-of: 3
stop-sequences:
- "###"
inject-start-text: yes
inject-restart-text: yes
show-probabilities: off
vars:
- "topic"
- "in the context of"
- "question"
examples:
- "node js"
- "programming"
- "What was the version of node in 2018?"
chomp-start: on
chomp-end: off
external: ""
conversation-mode: no
filter: no
# Keep stitching together until reaching this limit
# This allows a full response for answers which may need n*max-tokens to reach the stop-sequence.
stitch-max: 0
needs-work: no
{{< /highlight >}}


### elisp for the tutor {#elisp-for-the-tutor}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun org-brain-name-from-list-maybe (l)
  (if (and (listp l)
           (> (length l) 1))
      (second l)
    l))

(defun org-brain-remove-irrelevant-names-from-path (path)
  (-filter
   (lambda
     (e)
     (not
      (string-equal "infogetics" e)))
   path))

(defun org-brain-parent-name ()
  (snc "s join"
       (list2str
        (org-brain-remove-irrelevant-names-from-path
         (mapcar
          'org-brain-name-from-list-maybe
          (org-brain-parents org-brain--vis-entry))))))

(defun org-brain-current-name ()
  (car
   (org-brain-remove-irrelevant-names-from-path
    (mapcar
     'org-brain-name-from-list-maybe
     (list org-brain--vis-entry)))))

(defun org-brain-current-topic (&optional for-external-searching)
  (interactive)
  (let ((path
         (mapcar
          'org-brain-name-from-list-maybe
          (append (org-brain-parents org-brain--vis-entry) (list org-brain--vis-entry)))))

    (setq path
          (if for-external-searching
              (-filter (lambda (e) (not (string-equal "infogetics" e))) path)
            path))
    (let ((topic
           (chomp (apply 'cmd path))))
      (if (interactive-p)
          (etv topic)
        topic))))

(defun org-brain-asktutor (question)
  (interactive (list (read-string-hist (concat (org-brain-current-topic) ": "))))
  (mu
   (etv
    (snc
     (concat
      (cmd
       "oci"
       "openai-complete"
       "$MYGIT/semiosis/prompts/prompts/tutor.prompt"
       (org-brain-current-name)
       (org-brain-parent-name)
       question)
      " | tpp")))))
{{< /highlight >}}


### elisp for subtopic generation {#elisp-for-subtopic-generation}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun org-brain-suggest-subtopics (&optional update)
  (interactive)
  (message "Using pen.el to suggest subtopics...")
  (let ((subtopic-candidates
         ;; (pen-pf-keyword-extraction (org-brain-current-topic t))
         (let ((sh-update (or sh-update
                              update
                              (eq (prefix-numeric-value current-prefix-arg) 4))))
           (let ((s (pen-pf-subtopic-generation (org-brain-pf-topic) (org-brain-existing-subtopics-stringlist))))
             (if (not (sor s))
                 (progn
                   (message "Empty generation 1/3. Trying again.")
                   (setq s (upd (pen-pf-subtopic-generation (org-brain-pf-topic) (org-brain-existing-subtopics-stringlist))))
                   (if (not (sor s))
                       (progn
                         (message "Empty generation 2/3. Trying again.")
                         (setq s (upd (pen-pf-subtopic-generation (org-brain-pf-topic) (org-brain-existing-subtopics-stringlist))))
                         (if (not (sor s))
                             (progn
                               (message "Empty generation 3/3. Giving up.")
                               (error "Empty generation 3/3. Giving up."))
                           s))
                     s)
                   s)
               s)))))

    (setq subtopic-candidates
          (str2list
           (cl-sn
            "sed 's/^- //'"
            :stdin
            (chomp
             (snc
              (cmd "scrape" "^- [a-zA-Z -]+$")
              (concat "- " subtopic-candidates))) :chomp t)))

    ;; (ns current-prefix-arg)
    (if (interactive-p)
        (let ((subtopic-selected
               (try
                (cond
                 ((or (>= (prefix-numeric-value current-prefix-arg) 16)
                      (>= (prefix-numeric-value current-prefix-arg) 32))
                  (let ((b (nbfs (list2str subtopic-candidates))))
                    (with-current-buffer b
                      (let ((r (if (yn "Add all?")
                                   subtopic-candidates)))
                        (kill-buffer b)
                        r))))
                 (t
                  ;; Select one, do not refresh cache
                  (list (fz subtopic-candidates)))))))
          (if subtopic-selected
              (cl-loop for st in subtopic-selected do
                       (org-brain-add-child-headline org-brain--vis-entry st))))
      subtopic-candidates)))
{{< /highlight >}}


## `pen.el` improvements {#pen-dot-el-improvements}

-   The plan is to link `.prompt` (prompt description) files into a graph format where fungible prompts can be noticed.
-   Conversation mode.
    -   Summarize the current conversation scope and also extract facts from it. Use this in the next prompt.
    -   This will create a chatbot with rolling conversation.
    -   <http://github.com/semiosis/prompts/blob/master/prompts/meeting-bullets-to-summary.prompt>
    -   <http://github.com/semiosis/prompts/blob/master/prompts/tldr.prompt>
    -   <http://github.com/semiosis/prompts/blob/master/prompts/tutor.prompt>


## Extra demos {#extra-demos}


### Learning the flamenco guitar -- subtopic generation and tutor {#learning-the-flamenco-guitar-subtopic-generation-and-tutor}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/JA82zhiL4Su0LVufE7gqH7Mej" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/JA82zhiL4Su0LVufE7gqH7Mej.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/JA82zhiL4Su0LVufE7gqH7Mej.js" id="asciicast-JA82zhiL4Su0LVufE7gqH7Mej" async></script>


### Asking the AI professor about Paleoceanography {#asking-the-ai-professor-about-paleoceanography}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/9D6Ws3G0OvSAWnrLMtXJoDXoA" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/9D6Ws3G0OvSAWnrLMtXJoDXoA.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/9D6Ws3G0OvSAWnrLMtXJoDXoA.js" id="asciicast-9D6Ws3G0OvSAWnrLMtXJoDXoA" async></script>