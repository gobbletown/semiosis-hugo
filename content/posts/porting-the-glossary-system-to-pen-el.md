+++
title = "Porting the glossary system to Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-07T00:00:00+12:00
keywords = ["gpt", "pen", "emacs"]
draft = false
+++

## Summary {#summary}

I begin the porting of the glossary system to
`Pen.el`. I feel like it's an important
learning tool to be able to construct
glossaries while working with LMs.

The glossary is also important for helping the
user to create context that enable prompt
functions to be more precise.

Once a user develops their glossary to a
degree, Pen.el will be able to recognise the
context for which you are working within and
be more accurate with its queries.

This will result in more accurate, relevant
and tailored results from LMs.


## Progress {#progress}

So far, the glossary system will help you to
automatically generate glossaries with NLG and
save to your `~/.pen` directory.

The next stage is to include the button
overlay so you may see at a glance while
working what you have already learned and
committed to the glossary.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/6CehKZOERnV2Jhu8PSNZtyhGx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/6CehKZOERnV2Jhu8PSNZtyhGx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/6CehKZOERnV2Jhu8PSNZtyhGx.js" id="asciicast-6CehKZOERnV2Jhu8PSNZtyhGx" async></script>


## Code {#code}

Code
: <http://github.com/semiosis/pen.el/blob/master/src/pen-glossary.el>


Code (to be completed)
: <http://github.com/semiosis/pen.el/blob/master/src/pen-glossary-new.el>

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun pen-add-to-glossary (term &optional take-first definition)
  "C-u will allow you to add to any glossary file"
  (interactive (let ((s (pen-thing-at-point-ask)))
                 (if (not (sor s))
                     (setq s (read-string-hist "glossary term to add: ")))
                 (list s)))
  (deactivate-mark)
  (if (not definition)
      (setq definition (lm-define term t (pen-topic t))))

  (let* ((cb (current-buffer))
         (all-glossaries-fp (pen-mnm (pen-list2str (pen-list-glossary-files))))
         (fp
          (if (pen-is-glossary-file)
              (buffer-file-name)
            (pen-umn (or
                      (and (or (>= (prefix-numeric-value current-prefix-arg) 4)
                               (not (local-variable-p 'glossary-files)))
                           (pen-umn (fz all-glossaries-fp
                                        nil nil "glossary to add to: ")))
                      (and
                       (local-variable-p 'glossary-files)
                       (if take-first
                           (car glossary-files)
                         (pen-umn (fz (pen-mnm (pen-list2str glossary-files))
                                      "$HOME/glossaries/"
                                      nil "glossary to add to: "))))
                      (pen-umn (fz (pen-mnm (pen-list2str (list "$HOME/glossaries/glossary.txt")))
                                   "$HOME/glossaries/"
                                   nil "glossary to add to: ")))))))
    (with-current-buffer
        (find-file fp)
      (progn
        (if (save-excursion
              (beginning-of-line)
              (looking-at-p (concat "^" (pen-unregexify term) "$")))
            (progn
              (end-of-line))
          (progn
            (end-of-buffer)
            (newline)
            (newline)
            (insert term)))
        (newline)
        (if (sor definition)
            (insert (pen-pretty-paragraph (concat "    " definition)))
          (insert "    ")))
      (current-buffer))))
{{< /highlight >}}