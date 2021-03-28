+++
title = "GPT-3 for building mind maps with an AI tutor for any topic"
author = ["Shane Mulligan"]
date = 2021-03-29T00:00:00+13:00
keywords = ["GPT-3", "emacs", "learning"]
draft = false
+++

## Summary {#summary}

I combine GPT-3 with `org-brain` to expand on
topics, suggesting subtopics and providing an
interactive tutor for any topic.


## Demonstration {#demonstration}


### Learn about AI {#learn-about-ai}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/tV37yuypzU8C4ttDL4w24HOtx.js" id="asciicast-tV37yuypzU8C4ttDL4w24HOtx" async></script>


### Learn about microbiology {#learn-about-microbiology}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/R25hFKsdKc1wcfbMGeXnXa0iJ.js" id="asciicast-R25hFKsdKc1wcfbMGeXnXa0iJ" async></script>


## Code {#code}


### subtopic prompt {#subtopic-prompt}


### `tutor` prompt {#tutor-prompt}

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


### elisp {#elisp}

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

(defun org-brain-suggest-subtopics ()
  (interactive)
  (let ((subtopics
         (pen-pf-keyword-extraction (org-brain-current-topic t))))
    (if (interactive-p)
        (etv subtopics)
      subtopics)))

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