+++
title = "Building a Clojure environment in emacs"
author = ["Shane Mulligan"]
date = 2021-04-28T00:00:00+12:00
keywords = ["clojure", "eamcs"]
draft = false
+++

## Summary {#summary}

-   I install/learn some existing tooling
-   I create some tools for missing functionality


## Missing functionality {#missing-functionality}

-   Google (semantic) search for clojure libaries / versions
    -   `clj-refactor.el` lists all libraries on `clojars`, but you have to lexically select from that
    -   "web framework" should yield lists of web framework libraries
-   Require a library into a source file from the dependencies imported in `project.clj`


## Install tools {#install-tools}

-   <https://github.com/clojure-emacs/clj-refactor.el>


### `clj-refactor.el` {#clj-refactor-dot-el}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/mMVFa96FeZkLVEUeANRqtgYpF" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mMVFa96FeZkLVEUeANRqtgYpF.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/mMVFa96FeZkLVEUeANRqtgYpF.js" id="asciicast-mMVFa96FeZkLVEUeANRqtgYpF" async></script>


## Create tools {#create-tools}


### `antlr4` current buffer parser {#antlr4-current-buffer-parser}

Using `antlr4` I would like to be able to
obtain a parse tree of the current source file
of any language and a clojure `repl` for
visiting that parse tree.

-   <https://github.com/aphyr/clj-antlr>


### `clojure-find-deps` {#clojure-find-deps}

This script searches the `clojars` website for dependencies.

This is useful because it's arduous to find
libraries and the correct version to import.

emacs will ask if you want to search google
for appropriate libraries. This is great
because you can use any search terms, such as
"`web framework`" and the libraries you want
will be suggested.

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
                                     "") { shift; }; ;;
                                     -gl) {
                                         use_google=y
                                         shift
                                     }
                                          ;;

                                     *) break;
                                 esac; done

query="$@"
query="$(p "$query" | urlencode)"

{
    oci elinks-dump "https://clojars.org/search?q=$query" | scrape "^[a-z/]+ [0-9]+\.[0-9]+\.[0-9]+$" | sed 's/^/[/;s/ \(.*\)/ "\1"/;s/$/]/;' | awk 1
    if test "$use_google" = "y"; then
        # Don't do more than 3. It's too slow at the moment.
        # Parallelise it in the future with clojure
        gl site:clojars.org "$@" | head -n 5 | while IFS=$'\n' read -r line; do
            (
                exec 0</dev/null
                timeout 2 oci elinks-dump "$line"
            )
        done | scrape "^\[[a-z/]+ \"[0-9.]+\"\]"
    fi
} | uniqnosort | pavs
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun clojure-find-deps (use-google &rest query)
  (interactive (list (yn "Use google?")
                     (read-string-hist "clojure-find-deps query: ")))

  (if (>= (prefix-numeric-value current-prefix-arg) 4)
      (setq use-google t))

  ;; (tv query)
  (my/copy (fz (snc (apply 'cmd "clojure-find-deps"
                           (if use-google
                               "-gl")
                           (-flatten (mapcar (lambda (e) (s-split " " e)) query)))))))
{{< /highlight >}}

The results come out in `lein add` format.

Top hits for `routing library` on google.

{{< highlight text "linenos=table, linenostart=1" >}}
[compojure "1.6.2"]
[trail "2.3.1"]
[ataraxy "0.4.2"]
[paths "0.1.1"]
[trout "0.1.4"]
[gate "0.0.19"]
[treo "0.2.1"]
[avenue "0.2.7"]
{{< /highlight >}}

Some hits for `web framework`.

{{< highlight text "linenos=table, linenostart=1" >}}
[noir "1.3.0"]
[joodo "2.1.0"]
[borg/compojure "0.3.2"]
[webnf "0.0.0"]
[funkyweb "0.1.0"]
[conjure "0.8.4"]
[tandem "0.0.2"]
{{< /highlight >}}

`DEMO`

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/powkGa61fG4zttGEX723FqSeu" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/powkGa61fG4zttGEX723FqSeu.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/powkGa61fG4zttGEX723FqSeu.js" id="asciicast-powkGa61fG4zttGEX723FqSeu" async></script>


### `clojure-list-deps` {#clojure-list-deps}

This script will find all the `project.clj`
files on my computer and parse them for their
dependencies list.

This makes it easy to find and add
dependencies I have used in the past.

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

lf "project.clj" | umn | awk1 | while IFS=$'\n' read -r line; do
    (
    exec 0</dev/null
    cat "$line" | bb -i "(doseq [l (map str (->> (read-string (clojure.string/join \" \" *input*)) (drop-while (complement #{:dependencies})) next first))] (println l))" -o 2>/dev/null | cat
    )
done | uniqnosort
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun clojure-select-copy-dependency ()
  (interactive)
  (my/copy (fz (snc "cd $NOTES; oci clojure-list-deps"))))
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/4gNTXRw9ifeGZ3WmSQWAdfyki" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/4gNTXRw9ifeGZ3WmSQWAdfyki.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/4gNTXRw9ifeGZ3WmSQWAdfyki.js" id="asciicast-4gNTXRw9ifeGZ3WmSQWAdfyki" async></script>


## Tidy `cider` {#tidy-cider}


### Automatically run cider `jack-in` {#automatically-run-cider-jack-in}

-   ignore it if it's already connected

<!--listend-->

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defun ignore-truthy (&rest _arguments)
  "Do nothing and return nil.
This function accepts any number of ARGUMENTS, but ignores them."
  (interactive)
  t)

(defmacro auto-yes (&rest body)
  ""
  `(cl-letf (((symbol-function 'yes-or-no-p) #'ignore-truthy)
             ((symbol-function 'y-or-n-p) #'ignore-truthy))
     (progn ,@body)))
(defmacro auto-no (&rest body)
  ""
  `(cl-letf (((symbol-function 'yes-or-no-p) #'ignore)
             ((symbol-function 'y-or-n-p) #'ignore))
     (progn ,@body)))

(defvar my-do-cider-auto-jack-in t)

(defun cider-auto-jack-in ()
  ;; Make sure to be more precise. jack in with cljs too if it should
  ;; j:cider-jack-in-cllj
  ;; j:cider-jack-in-cljs
  ;; j:cider-jack-in-clj&cljs

  (if my-do-cider-auto-jack-in
      (auto-no (call-interactively 'cider-jack-in))))

(add-hook 'clojure-mode-hook 'cider-auto-jack-in)
{{< /highlight >}}

-   Add to `toggle-values`

{{< figure src="/ox-hugo/auto-jack-in.png" >}}