+++
title = "Translate the code you're reading into your fav language"
author = ["Shane Mulligan"]
date = 2021-10-19T00:00:00+13:00
keywords = ["pen", "gpt", "openai", "codex"]
draft = false
+++

## Summary {#summary}

The idea is you have a favourite programming
language and while reading code you don't
understand you want a default language to
translate into.


## elisp {#elisp}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defcustom pen-fav-programming-language ""
  "By setting pen-fav-programming-language, you set a default language to translate into.
This is useful for code-understanding when reading languages you don't understand.
| =H-\" U= | =dff-pf-transpile-3-nil-nil-emacs-lisp-= | =pen-map=
"
  :type 'string
  :group 'pen
  :initialize #'custom-initialize-default)

;; U is for understand
(define-key pen-map (kbd "H-\" U") (dff (etv (pf-transpile/3 nil nil (sor pen-fav-programming-language)))))
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/cgN7wDVzbJLhHUeP5d9EpwoBm" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/cgN7wDVzbJLhHUeP5d9EpwoBm.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/cgN7wDVzbJLhHUeP5d9EpwoBm.js" id="asciicast-cgN7wDVzbJLhHUeP5d9EpwoBm" async></script>


### What does this Go code do? {#what-does-this-go-code-do}

{{< highlight go "linenos=table, linenostart=1" >}}
func filterCommands(uri DocumentURI, commands []Command) []Command {
  results := []Command{}
  for _, v := range commands {
  	if v.OS != "" {
  		found := false
  		for _, os := range strings.FieldsFunc(v.OS, func(r rune) bool { return r == ',' }) {
  			if strings.TrimSpace(os) == runtime.GOOS {
  				found = true
  			}
  		}
  		if !found {
  			continue
  		}
  	}
  	results = append(results, Command{
  		Title:     v.Title,
  		Command:   fmt.Sprintf("efm-langserver\t%s\t%s", v.Command, string(uri)),
  		Arguments: []interface{}{string(uri)},
  	})
  }
  return results
}
{{< /highlight >}}


### Ahhh, Elisp :) That makes sense now {#ahhh-elisp--that-makes-sense-now}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun filter-commands (uri commands)
  (let (results)
    (dolist (v commands)
      (when (and (not (string-empty-p v))
                 (not (string-empty-p v.os)))
        (let ((os (split-string v.os ",")))
          (when (member (or (getenv "GOOS") "") os)
            (push (list :title v.title
                        :command (format "efm-langserver %s %s" v.command uri)
                        :arguments (list uri))
                  results)))))
    results))
{{< /highlight >}}