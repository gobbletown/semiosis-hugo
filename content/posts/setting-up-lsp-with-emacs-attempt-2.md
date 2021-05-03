+++
title = "Setting up LSP with emacs (Go, Java, Python, Rust, C++, Haskell, JS, TS, Ruby…)"
author = ["Shane Mulligan"]
date = 2019-12-05T00:00:00+13:00
keywords = ["emacs", "LSP", "cpp", "rust", "Python", "tooling"]
draft = false
+++

Guides used
: <https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/> <br />
    <https://github.com/emacs-lsp/lsp-java>

<!--listend-->

{{< highlight text "linenos=table, linenostart=1" >}}
Debug Adaptor Protocol
DAP
    The LSP of debugging.

    https://code.visualstudio.com/blogs/2018/08/07/debug-adapter-protocol-website
{{< /highlight >}}

---


## State of languages with LSP in my own environment {#state-of-languages-with-lsp-in-my-own-environment}

-   Also, I'm using GPT-3 for:
    -   code generation
    -   comment generation

| Language   | LSP Working | Fallback working   | Current user experience | TabNine enabled | REPL / Playground | DAP (debug) Working | Static analysis | Complaints                               | Custom LSP improvements                                    |
|------------|-------------|--------------------|-------------------------|-----------------|-------------------|---------------------|-----------------|------------------------------------------|------------------------------------------------------------|
| Go         | ✓           | ✓                  | Amazing                 | ✓               | ✓                 |                     | ✓               | Go toolchain changes too quickly         |                                                            |
| Python     | ✓           | ✓                  | Amazing                 | ✓               | ✓                 | ✓                   | ✓               |                                          |                                                            |
| Racket     | ✓           | ✓ racket-mode      | Amazing                 |                 | ✓                 |                     | ✓               |                                          |                                                            |
| Java       | ✓           | ✓ eclim            | Good                    | ✓               | ✓ (default)       | ✓                   | ✓               | A little slow. Documentation doesn't fit | Capture documentation from eldoc. Full edit on doc buffer. |
| Rust       | ✓           | ✓                  | Good                    | ✓               | ✓                 |                     | ✓               |                                          |                                                            |
| C++        | ✓           | ✓ clang            | Great                   | ✓               | ✓                 |                     | ✓               |                                          |                                                            |
| Haskell    | ✓           | ✓ intero           | Good                    | ✓               | ✓                 |                     | ✓               |                                          |                                                            |
| PureScript |             | ✓ psc-ide (spago?) | Good                    |                 | ✓                 |                     |                 |                                          |                                                            |
| JavaScript | ✓           |                    | Amazing                 | ✓               | ✓                 |                     |                 |                                          |                                                            |
| TypeScript | ✓           |                    | Amazing                 | ✓               | ✓                 |                     |                 |                                          |                                                            |
| Ruby       | ✓           | ✓ pry / robe       | Good                    | ✓               | ✓                 |                     | ✓               |                                          |                                                            |
| OCaml      | ✓           |                    | Good                    |                 | ✓                 |                     | ✓               |                                          |                                                            |
| Clojure    | ✓           | ✓ cider            | Amazing                 | ✓               | ✓                 |                     | ✓               |                                          |                                                            |
| Prolog     | ✓           | ✓ prolog-mode      | Amazing                 | ✓               | ✓                 |                     | ✓               |                                          |                                                            |


### Prolog {#prolog}

Install prolog.

{{< highlight bash "linenos=table, linenostart=1" >}}
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
{{< /highlight >}}

Install the `lsp_server` package.

-   Start `swipl`
-   Wait for the prompt (`?-`)
-   Enter `pack_install(lsp_server)`
-   Press `Y`
-   Wait for the prompt (`?-`)
-   Press `C-d`

`prolog-pack-install`

{{< highlight bnf "linenos=table, linenostart=1" >}}
pkg="$1"
test -n "$pkg" || exit 1

x -sh swipl -e "?- " -s "pack_install($pkg)." -c m -e "Y/n" -s Y -e "Create directory" -c m -e "?- " -c d -i
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
prolog-pack-install lsp_server
{{< /highlight >}}

Command to start the prolog server:

`prolog-lsp`

{{< highlight bash "linenos=table, linenostart=1" >}}
swipl -g use_module(library(lsp_server)). -g lsp_server:main -t halt -- stdio
{{< /highlight >}}


### Racket {#racket}

I made the emacs plugin myself!

<https://github.com/mullikine/lsp-racket-el>

<a title="asciinema recording" href="https://asciinema.org/a/y3PaPaOClZEBcUZA3XScNtXPL" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/y3PaPaOClZEBcUZA3XScNtXPL.svg" /></a>


### Python {#python}

The python editing experience in emacs is second to none.

<a title="asciinema recording" href="https://asciinema.org/a/0dgW1uTEap2ROGDvEsjlXwi3J" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/0dgW1uTEap2ROGDvEsjlXwi3J.svg" /></a>


### Go {#go}

<a title="asciinema recording" href="https://asciinema.org/a/LE7erREtVHLMaI0cCKNOj1h5c" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/LE7erREtVHLMaI0cCKNOj1h5c.svg" /></a>


### Java {#java}

<a title="asciinema recording" href="https://asciinema.org/a/i89DxN0P786IvjFpWwLXjodpz" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/i89DxN0P786IvjFpWwLXjodpz.svg" /></a>


### Rust {#rust}

For `std:env` the eldoc looks identical to lsp
ui doc.

But this is not the case for everything.

Usually the docs are different.

{{< highlight sh "linenos=table, linenostart=1" >}}
sp +/"use std::env;" "$MYGIT/mosuka/bayard/src/util/log.rs"
{{< /highlight >}}

<a title="asciinema recording" href="https://asciinema.org/a/0jXz47NyBa7TdmVVe9kb9oPIL" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/0jXz47NyBa7TdmVVe9kb9oPIL.svg" /></a>


### C++ {#c-plus-plus}

LSP for C++ works quite well.

<a title="asciinema recording" href="https://asciinema.org/a/ALx6GoyRapW0MrHZbvU9M8z95" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/ALx6GoyRapW0MrHZbvU9M8z95.svg" /></a>


#### I need to figure out how to get auto-completion after `->` {#i-need-to-figure-out-how-to-get-auto-completion-after}

This is not an LSP problem but a problem with `company-mode`.

It should be possible to accomplish.


### Haskell {#haskell}

{{< highlight sh "linenos=table, linenostart=1" >}}
cd "$MYGIT/haskell/haskell-ide-engine"; shx stack ./install.hs hie-8.6.4
{{< /highlight >}}

Installation complete! LSP for emacs requires
`HIE` to be installed. The results are quite
nice.

<a title="asciinema recording" href="https://asciinema.org/a/StPWXED28BqyYZo2f5plXEmQr" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/StPWXED28BqyYZo2f5plXEmQr.svg" /></a>


### JavaScript {#javascript}

<a title="asciinema recording" href="https://asciinema.org/a/Kur7UOJ881rOioWCNRRCzlVY2" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Kur7UOJ881rOioWCNRRCzlVY2.svg" /></a>


### TypeScript {#typescript}

<a title="asciinema recording" href="https://asciinema.org/a/WKUsKTPwUjQvByaytrb3dox4M" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/WKUsKTPwUjQvByaytrb3dox4M.svg" /></a>


### Ruby {#ruby}

<a title="asciinema recording" href="https://asciinema.org/a/WkFmVTqUu5gn2T07VT6Qo8w9m" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/WkFmVTqUu5gn2T07VT6Qo8w9m.svg" /></a>


### `c++` and `clang` {#c-plus-plus-and-clang}


#### Do not use `cquery`. Apparently, `clangd` is better {#do-not-use-cquery-dot-apparently-clangd-is-better}

<https://github.com/cquery-project/cquery>


#### `clangd` {#clangd}

<http://releases.llvm.org/download.html>

<span class="underline">**Download and install the pre-built binary**</span>

<span class="underline">**Write some elisp**</span>

Without `-background-index`, it will only keep
an in-memory index of the files that are
active in Emacs buffers, but to be able to
find references and symbols in any project
file the background index is recommended. It
is placed at the project root as the “.clangd”
folder.

`compile_commands.json`
: Clangd tries to locate the
    “compile\_commands.json” file in the root of
    the project, so it’s useful to make a symlink in the
    project root and to where it’s located in
    a build folder.

    Most build tools can output
    “compile\_commands.json”.

    In CMake you write:

    {{< highlight cmake "linenos=table, linenostart=1" >}}
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
    {{< /highlight >}}


## Disable `lsp-mode` auto-formatting {#disable-lsp-mode-auto-formatting}

`lsp-mode` autoformats when the buffer is changed.

It was breaking `C++`.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(add-hook 'lsp--managed-mode-hook (lambda nil (interactive) (remove-hook 'post-self-insert-hook 'lsp--on-self-insert t)))
{{< /highlight >}}


## `lsp-ui` uses `<return>` rather than `RET` when setting bindings {#lsp-ui-uses-return-rather-than-ret-when-setting-bindings}

Set `RET` too. I want all `<return>` bindings
to be `RET` bindings everywhere in my
`emacs.d`.

On the surface level it might appear to work
fine but you might scratch your head when
things go wrong wondering why prefix keys are
not working or bindings are not being
overridden, etc.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--view)
(define-key lsp-ui-imenu-mode-map (kbd "RET") 'lsp-ui-imenu--view)

(define-key lsp-ui-flycheck-list-mode-map (kbd "<M-RET>") 'lsp-ui-flycheck-list--visit)
(define-key lsp-ui-flycheck-list-mode-map (kbd "RET") 'lsp-ui-flycheck-list--view)
{{< /highlight >}}


## `lsp-peek` would close when I hit `PgUp` or `PgDown` {#lsp-peek-would-close-when-i-hit-pgup-or-pgdown}

Set these to make the keys do something useful.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key lsp-ui-peek-mode-map (kbd "<prior>") #'lsp-ui-peek--select-prev-file)
(define-key lsp-ui-peek-mode-map (kbd "<next>") #'lsp-ui-peek--select-next-file)
{{< /highlight >}}


## For sanity, group all the `custom` config together {#for-sanity-group-all-the-custom-config-together}

They were being set in different `use-package` blocks.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(use-package lsp-mode
  :custom
  ;; ...
  )
{{< /highlight >}}

Some of these switches toggle various UI
features.

If it's tucked away inside
an automatically generated `custom-set-
variables` somewhere, this could be confusing.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(custom-set-variables
 ;; debug
 '(lsp-print-io t)
 '(lsp-trace t)
 '(lsp-print-performance t)

 ;; general
 '(lsp-auto-guess-root t)
 '(lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
 '(lsp-response-timeout 10)

 ;; (lsp-prefer-flymake t)
 '(lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
 ;; flymake is shit. do not use it

 ;; go-client
 '(lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))

 '(company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
 '(company-lsp-async t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)

                                        ;top right docs
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 120)
 '(lsp-ui-doc-position (quote at-point))
 '(lsp-ui-doc-use-childframe t)

 ;; If this is true then you can't see the docs in terminal
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-flycheck-enable t)

 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)

 '(lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-code-actions-prefix "" t)

                                        ;inline right flush docs
 '(lsp-ui-sideline-enable t)

 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t))
{{< /highlight >}}


## `handle.el` {#handle-dot-el}

If you want to use `handle.el` then do something like this.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(define-key prog-mode-map (kbd "M-=") 'handle-repls)
(define-key prog-mode-map (kbd "M-)") 'handle-assignments)
(define-key prog-mode-map (kbd "M-*") 'handle-references)
(define-key prog-mode-map (kbd "M-^") 'handle-errors)
;; This is reserved
(define-key prog-mode-map (kbd "M-+") nil)
(define-key prog-mode-map (kbd "M-_") nil)
(define-key prog-mode-map (kbd "M-9") 'handle-docs)
(define-key prog-mode-map (kbd "M-.") 'handle-godef)

(define-key prog-mode-map (kbd "M-p") 'handle-prevdef)
(define-key prog-mode-map (kbd "M-n") 'handle-nextdef)

(define-key prog-mode-map (kbd "M-P") 'handle-preverr)
(define-key prog-mode-map (kbd "M-N") 'handle-nexterr)

(define-key prog-mode-map (kbd "M-l M-j M-w") 'handle-spellcorrect)
{{< /highlight >}}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(handle '(prog-mode)
        :complete '(indent-for-tab-command)
        :repls '()
        :formatters '(lsp-format-buffer)
        :docs '(
                ;; lsp-ui-doc-show
                my/doc-thing-at-point)
        :docsearch '(my/doc-ask)
        :godec '(lsp-find-declaration)
        :godef '(lsp-find-definition
                 xref-find-definitions
                 helm-gtags-dwim)
        :nextdef '(my-prog-next-def)
        :prevdef '(my-prog-prev-def)
        :nexterr '(flycheck-next-error)
        :preverr '(flycheck-previous-error)
        ;; select from multiple
        :errors '(lsp-ui-flycheck-list)
        :assignments '()
        :references '(lsp-ui-peek-find-references)
        :definitions '(lsp-ui-peek-find-definitions)
        :implementations '(lsp-ui-peek-find-implementation))
{{< /highlight >}}


## Common lisp {#common-lisp}

LSP is insufficient to support SLIME-like
levels of interaction.


## Latest config (`2021`) {#latest-config--2021}

Pick out what you want.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
; https://vxlabs.com/2018/06/08/python-language-server-with-emacs-and-lsp-mode/

;; ;; This really helps with lsp-mode
;; ;; But I have enabled it only for certain modes
;; (electric-pair-mode 1)

;; gopls is the official go language server

;; (defvar my-disable-lsp nil)

(custom-set-variables
 ;; debug
 '(lsp-print-io t)
 '(lsp-trace t)
 '(lsp-print-performance t)

 ;; general
 '(lsp-auto-guess-root t)
 '(lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
 '(lsp-response-timeout 10)

 ;; (lsp-prefer-flymake t)
 '(lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
 ;; flymake is shit. do not use it

 ;; go-client
 ;; '(lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))

 '(company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
 '(company-lsp-async t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 ;; '(lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))
 '(lsp-document-sync-method (quote incremental))

                                        ;top right docs
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 120)
 '(lsp-ui-doc-position (quote at-point))
 '(lsp-ui-doc-use-childframe t)

 ;; If this is true then you can't see the docs in terminal
 '(lsp-ui-doc-use-webkit nil)
 '(lsp-ui-flycheck-enable t)

 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)

 '(lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-code-actions-prefix "" t)

                                        ;inline right flush docs
 '(lsp-ui-sideline-enable t)

 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics t)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t))



(require 'lsp-mode)
(require 'my-lsp-clients)



(require 'el-patch)
(el-patch-feature lsp-mode)
(el-patch-defun lsp (&optional arg)
  "Entry point for the server startup.
When ARG is t the lsp mode will start new language server even if
there is language server which can handle current language. When
ARG is nil current file will be opened in multi folder language
server if there is such. When `lsp' is called with prefix
argument ask the user to select which language server to start."
  (interactive "P")

  (lsp--require-packages)

  (when (buffer-file-name)
    (let (clients
          (matching-clients (lsp--filter-clients
                             (-andfn #'lsp--matching-clients?
                                     #'lsp--server-binary-present?))))
      (cond
       (matching-clients
        (when (setq lsp--buffer-workspaces
                    (or (and
                         ;; Don't open as library file if file is part of a project.
                         (not (lsp-find-session-folder (lsp-session) (buffer-file-name)))
                         (lsp--try-open-in-library-workspace))
                        (lsp--try-project-root-workspaces (equal arg '(4))
                                                          (and arg (not (equal arg 1))))))
          (lsp-mode 1)
          (when lsp-auto-configure (lsp--auto-configure))
          (setq lsp-buffer-uri (lsp--buffer-uri))
          (lsp--info "Connected to %s."
                     (apply 'concat (--map (format "[%s]" (lsp--workspace-print it))
                                           lsp--buffer-workspaces)))))
       ;; look for servers which are currently being downloaded.
       ((setq clients (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                   #'lsp--client-download-in-progress?)))
        (lsp--info "There are language server(%s) installation in progress.
The server(s) will be started in the buffer when it has finished."
                   (-map #'lsp--client-server-id clients))
        (seq-do (lambda (client)
                  (cl-pushnew (current-buffer) (lsp--client-buffers client)))
                clients))
       ;; look for servers to install
       ((setq clients (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                   #'lsp--client-download-server-fn
                                                   (-not #'lsp--client-download-in-progress?))))
        (let ((client (lsp--completing-read
                       (concat "Unable to find installed server supporting this file. "
                               "The following servers could be installed automatically: ")
                       clients
                       (-compose #'symbol-name #'lsp--client-server-id)
                       nil
                       t)))
          (cl-pushnew (current-buffer) (lsp--client-buffers client))
          (lsp--install-server-internal client)))
       ;; no clients present
       ((setq clients (unless matching-clients
                        (lsp--filter-clients (-andfn #'lsp--matching-clients?
                                                     (-not #'lsp--server-binary-present?)))))
        (lsp--warn "The following servers support current file but do not have automatic installation configuration: %s
You may find the installation instructions at https://emacs-lsp.github.io/lsp-mode/page/languages.
(If you have already installed the server check *lsp-log*)."
                   (mapconcat (lambda (client)
                                (symbol-name (lsp--client-server-id client)))
                              clients
                              " ")))
       ;; no matches
       ((-> #'lsp--matching-clients? lsp--filter-clients not)
        (lsp--error
         (el-patch-swap
           "There are no language servers supporting current mode `%s' registered with `lsp-mode'.
This issue might be caused by:
1. The language you are trying to use does not have built-in support in `lsp-mode'. You must install the required support manually. Examples of this are `lsp-java' or `lsp-metals'.
2. The language server that you expect to run is not configured to run for major mode `%s'. You may check that by checking the `:major-modes' that are passed to `lsp-register-client'.
3. `lsp-mode' doesn't have any integration for the language behind `%s'. Refer to https://emacs-lsp.github.io/lsp-mode/page/languages and https://langserver.org/ ."
           "No LSP server for current mode")
         major-mode major-mode major-mode))))))



(defun maybe-lsp ()
  (interactive)
  (cond
   ((and org-src-mode (major-mode-p 'haskell-mode))
    (message "Disabled lsp because i want haskell babel blocks to be fast"))
   ;; ((and org-src-mode (major-mode-p 'c-mode))
   ;;  (message "Disabled lsp because I need to set up ccls again"))
   ((string-match "/emacs-mirror/.*\\.c$" (or (get-path-nocreate) ""))
    (message "Disabled lsp because i haven't got it going for emacs source C code yet"))
   (t (call-interactively 'lsp))))

(use-package lsp-mode
  :ensure t
  :commands lsp-register-client
  :init (setq lsp-gopls-server-args '("--debug=localhost:6060"))
  :config
  (setq lsp-prefer-flymake :none)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t))))

                                        ;(use-package lsp-mode
                                        ; has to not fail when emacs 24
(my/with 'lsp-mode
         ;; Definitely do not want this -- it's very outdated
         ;; I blacklisted it
         ;; /home/shane/var/smulliga/source/git/config/emacs/config/my-package-blacklist.el
         ;; lsp-mode now provides lsp-go so I can't blacklist it like this
         ;; anymore.
         ;; (require 'lsp-go)

         (setq lsp-gopls-staticcheck t)
         (setq lsp-eldoc-render-all t)
         (setq lsp-gopls-complete-unimported t)

         ;; (setq lsp-gopls-staticcheck t)

         ;; Make this nil so I don't get duplication on the ui-doc
         ;; This was a problem especially in python
         ;; (setq lsp-eldoc-render-all nil)

         ;; (setq lsp-gopls-complete-unimported t)

         (use-package lsp-ui
           :ensure t
           ;; :demand t
           :config
           (setq lsp-ui-sideline-ignore-duplicate t)
           (add-hook 'lsp-mode-hook 'lsp-ui-mode)
           (require 'lsp-ui-imenu))

         (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
         ;; get lsp-python-enable defined
         ;; NB: use either projectile-project-root or ffip-get-project-root-directory
         ;;     or any other function that can be used to find the root directory of a project

         ;; deprecated
         ;; (lsp-define-stdio-client lsp-python "python"
         ;;                          #'projectile-project-root
         ;;                          '("pyls"))
         ;; this is the new way. but it's automatic now
         ;;(lsp-register-client
         ;; (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
         ;;                  :major-modes '(python-mode)
         ;;                  :server-id 'pyls))

         ;; make sure this is activated when python-mode is activated
         ;; lsp-python-enable is created by macro above

         ;; ;; Is it built-in now?
         ;; (add-hook 'python-mode-hook
         ;;           (lambda ()
         ;;             (lsp-python-enable)))

         (use-package lsp-mode
           :ensure t
           :commands (lsp lsp-deferred)
           :hook (go-mode . lsp-deferred))


         (remove-hook 'before-save-hook 'gofmt-before-save)
         (defun lsp-go-install-save-hooks ()
           (add-hook 'before-save-hook #'lsp-format-buffer t t)
           (add-hook 'before-save-hook #'lsp-organize-imports t t))
         (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

         (require 'lsp-haskell)


                                        ; (setq lsp-haskell-process-path-hie "hie-wrapper")

         (use-package lsp-haskell
           :ensure t
           :config
           (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
           ;; Comment/uncomment this line to see interactions between lsp client/server.
           ;;(setq lsp-log-io t)
           )


         (progn
           (require 'julia-mode)
           ;; (push "/path/to/lsp-julia" load-path)
           (require 'lsp-julia)
           (require 'lsp-mode)
           ;; Configure lsp + julia
           (add-hook 'julia-mode-hook #'lsp-mode)
           (add-hook 'julia-mode-hook #'lsp))



         (require 'lsp-racket)
         ;; (add-hook 'racket-mode-hook #'lsp-racket-enable)


         (use-package lsp-mode
           ;; :demand t
           :config
           (add-hook 'c++-mode-hook #'lsp)
           ;; I can't keep it on because when projects dont work, its super annoying
           ;; (remove-hook 'c++-mode-hook #'lsp)

           ;; ccls is uninstallable on ubuntu16
           ;; https://repology.org/project/ccls/versions
           ;; I have tried. I need ubuntu20
           ;; (remove-hook 'c-mode-hook #'lsp)
           ;; I did it
           ;; (add-hook 'c-mode-hook #'lsp)
           (add-hook 'c-mode-hook #'maybe-lsp)
           (add-hook 'python-mode-hook #'lsp)
           ;; Just install this manually and then it will work
           ;; https://github.com/richterger/Perl-LanguageServer
           (add-hook 'perl-mode-hook #'lsp)
           (add-hook 'dockerfile-mode-hook #'lsp)
           (add-hook 'java-mode-hook #'lsp)
           (add-hook 'kotlin-mode-hook #'lsp)
           (add-hook 'yaml-mode-hook #'lsp)
           (add-hook 'sql-mode-hook #'lsp)
           (add-hook 'php-mode-hook #'lsp)
           (add-hook 'clojure-mode-hook #'lsp)
           ;; (remove-hook 'clojure-mode-hook #'lsp)
           (add-hook 'julia-mode-hook #'lsp)
           (add-hook 'ess-julia-mode-hook #'lsp)
           (add-hook 'go-mode-hook #'lsp)
           (add-hook 'cmake-mode-hook #'lsp)
           (add-hook 'ruby-mode-hook #'lsp)
           (add-hook 'gitlab-ci-mode-hook #'lsp)

           ;; this seems to be broken
           ;; (add-hook 'dockerfile-mode-hook #'lsp)

           (add-hook 'sh-mode-hook #'lsp)
           (add-hook 'rust-mode-hook #'lsp)
           (add-hook 'vimrc-mode-hook #'lsp)
           (add-hook 'racket-mode-hook #'lsp)
           ;; (remove-hook 'racket-mode-hook #'lsp)
           (add-hook 'rustic-mode-hook #'lsp)
           (add-hook 'nix-mode-hook #'lsp)
;;;  prolog-pack-install lsp_server
           ;; build-swi-ls
           (add-hook 'prolog-mode-hook #'lsp)
           (add-hook 'js-mode-hook #'lsp)
           (add-hook 'typescript-mode-hook #'lsp)
           ;; (add-hook 'haskell-mode-hook #'lsp)
           (add-hook 'haskell-mode-hook #'maybe-lsp)
           ;; (remove-hook 'haskell-mode-hook #'lsp)
           (add-hook 'purescript-mode-hook #'lsp)
           ;; (remove-hook 'haskell-mode-hook #'lsp)
           )

         ;; (use-package lsp-mode
         ;;   :ensure t
         ;;   :hook ((clojure-mode . lsp)
         ;;          (clojurec-mode . lsp)
         ;;          (clojurescript-mode . lsp))
         ;;   :config
         ;;   ;; add paths to your local installation of project mgmt tools, like lein
         ;;   (setenv "PATH" (concat
         ;;                   "/usr/local/bin" path-separator
         ;;                   (getenv "PATH")))
         ;;   (dolist (m '(clojure-mode
         ;;                clojurec-mode
         ;;                clojurescript-mode
         ;;                clojurex-mode))
         ;;     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
         ;;   (setq lsp-enable-indentation nil
         ;;         lsp-clojure-server-command '("bash" "-c" "clojure-lsp")))


         ;; These modes are "clojure"
         (dolist (m '(clojure-mode
                      clojurec-mode
                      clojurescript-mode
                      clojurex-mode))
           (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

         (require 'lsp-clojure)
         (setq lsp-enable-indentation nil
               lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))

         (require 'ccls)
         ;; (setq ccls-executable "/usr/local/bin/ccls")
         (setq ccls-executable "/home/shane/scripts/ccls")



         ;; This might be outdated now
         ;; (use-package company-lsp
         ;;   :config
         ;;   (push 'company-lsp company-backends))


         (use-package lsp-ui-peek
           :config)

         ;; NB: only required if you prefer flake8 instead of the default
         ;; send pyls config via lsp-after-initialize-hook -- harmless for
         ;; other servers due to pyls key, but would prefer only sending this
         ;; when pyls gets initialised (:initialize function in
         ;; lsp-define-stdio-client is invoked too early (before server
         ;; start)) -- cpbotha
         (defun lsp-set-cfg ()
           (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
             ;; TODO: check lsp--cur-workspace here to decide per server / project
             (lsp--set-configuration lsp-cfg)))

         ;; (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
         ;; (remove-hook 'lsp-after-initialize-hook 'lsp-set-cfg)


         ;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
         (use-package lsp-mode
           :config
           ;; `-background-index' requires clangd v8+!
           (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

           ;; ..
           )

         ;; (defun lsp-ui-doc--callback (hover bounds buffer)
         ;;            "Process the received documentation.
         ;; HOVER is the doc returned by the LS.
         ;; BOUNDS are points of the symbol that have been requested.
         ;; BUFFER is the buffer where the request has been made.")
         )

;; rust
;; This solved all rust lsp problems
(progn

  (require 'lsp-mode) ;; language server protocol
  (with-eval-after-load 'lsp-mode
    (add-hook 'rust-mode-hook #'lsp))
  ;; (add-hook 'rust-mode-hook #'flycheck-mode))

  ;; excessive UI feedback for light reading between coding
  (require 'lsp-ui)
  (with-eval-after-load 'lsp-ui
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  ;; autocompletions for lsp (available with melpa enabled)
  ;; (require 'company-lsp)
  ;; (push 'company-lsp company-backends)

  ;; tell company to complete on tabs instead of sitting there like a moron
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

  (use-package lsp-mode
    :hook
    ((go-mode c-mode c++-mode) . lsp)
    :bind
    (:map lsp-mode-map
          ("C-c r" . lsp-rename))
    :config

    ;; (require 'lsp-clients)

    ;; LSP UI tools
    (use-package lsp-ui
      :preface
      (defun ladicle/toggle-lsp-ui-doc ()
        (interactive)
        (if lsp-ui-doc-mode
            (progn
              (lsp-ui-doc-mode -1)
              (lsp-ui-doc--hide-frame))
          (lsp-ui-doc-mode 1)))
      :bind
      (:map lsp-mode-map
            ("C-c C-r" . lsp-ui-peek-find-references)
            ("C-c C-j" . lsp-ui-peek-find-definitions)
            ("C-c i" . lsp-ui-peek-find-implementation)
            ("C-c m" . lsp-ui-imenu)
            ("C-c s" . lsp-ui-sideline-mode)
            ("C-c d" . ladicle/toggle-lsp-ui-doc))
      :hook
      (lsp-mode . lsp-ui-mode))

    ;; (lsp-register-client
    ;;  (make-lsp-client :new-connection (lsp-stdio-connection
    ;;                                    (lambda () (cons "bingo"
    ;;                                                     lsp-clients-go-server-args)))
    ;;                   :major-modes '(go-mode)
    ;;                   :priority 2
    ;;                   :initialization-options 'lsp-clients-go--make-init-options
    ;;                   :server-id 'go-bingo
    ;;                   :library-folders-fn (lambda (_workspace)
    ;;                                         lsp-clients-go-library-directories)))

    ;; DAP
    (use-package dap-mode
      :custom
      (dap-go-debug-program `("node" "~/.extensions/go/out/src/debugAdapter/goDebug.js"))
      :config
      (dap-mode 1)
      (require 'dap-hydra)
      (require 'dap-gdb-lldb) ; download and expand lldb-vscode to the =~/.extensions/webfreak.debug=
      (require 'dap-go) ; download and expand vscode-go-extenstion to the =~/.extensions/go=
      (use-package dap-ui
        :ensure nil
        :config
        (dap-ui-mode 1)))

    ;; Lsp completion
    ;; (use-package company-lsp)
    )

  ;; FAQ -- [[https://github.com/emacs-lsp/lsp-java][GitHub - emacs-lsp/lsp-java]]
  ;; LSP Java is showing to many debug messages, how to stop that? Add the
  ;; following configuration.
  (setq lsp-inhibit-message t)


  ;; (lsp--on-self-insert t)

(defun my-lsp--managed-mode-hook-body ()
  (interactive)
  (remove-hook 'post-self-insert-hook 'lsp--on-self-insert t)
  (setq-local indent-region-function (function handle-formatters)))


;; But how do I remove it for all lsp buffers?
;; (remove-hook 'post-self-insert-hook 'lsp--on-self-insert)
;; (remove-hook 'post-self-insert-hook 'lsp--on-self-insert t)
(add-hook 'lsp--managed-mode-hook #'my-lsp--managed-mode-hook-body)

  ;; (remove-hook 'lsp--managed-mode-hook (lm (remove-hook 'post-self-insert-hook 'lsp--on-self-insert t)))


(defun lsp-ui-flycheck-list ()
  "List all the diagnostics in the whole workspace."
  (interactive)
  (let ((buffer (get-buffer-create "*lsp-diagnostics*"))
        (workspace lsp--cur-workspace)
        (window (selected-window)))
    (with-current-buffer buffer
      (lsp-ui-flycheck-list--update window workspace))
    (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck-list--refresh nil t)
    (setq lsp-ui-flycheck-list--buffer buffer)
    (display-buffer
     buffer)))



(define-key lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--view)
(define-key lsp-ui-imenu-mode-map (kbd "RET") 'lsp-ui-imenu--view)


(define-key lsp-ui-flycheck-list-mode-map (kbd "<M-RET>") 'lsp-ui-flycheck-list--visit)
(define-key lsp-ui-flycheck-list-mode-map (kbd "RET") 'lsp-ui-flycheck-list--view)


(define-key lsp-ui-peek-mode-map (kbd "<prior>") #'lsp-ui-peek--select-prev-file)
(define-key lsp-ui-peek-mode-map (kbd "<next>") #'lsp-ui-peek--select-next-file)

(require 'helm-lsp)


;; This is extremely slow.
;; It may be quite nice, but it's not nice enough. I can't do any programming with it
;; I have broken python-language-server (pyls) now, so I have to use this
;; pyls wasnt even broken. lsp-mode is broken, i think

;; the microsoft server a) doesn't free resources
;; b)  breaks after reopening a file'
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (progn (setq lsp-python-ms-auto-install-server t)
;;                (setq lsp-python-ms-executable
;;                      "/home/shane/scripts/ms-pyls"))
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))
                                        ; or lsp-deferred





;; For some reason this variable doesnt change when I set it
;; It only updates when i redefine =dap-python-debug-and-hydra=
(setq dap-python-executable "python-for-lsp")


;; This must be out of date
(defun dap-python-debug-and-hydra (&optional cmd pyver)
  (interactive)
  (if cmd
      (progn
        (if pyver
            (sh-notty (concat "cd /home/shane/scripts; ln -sf `which " pyver "` python-for-lsp-sym")))

        (let* ((cmdwords (s-split " " cmd))
               (scriptname (car cmdwords))
               (args (umn (s-join " " (cdr cmdwords)))))
          (with-current-buffer (find-file (umn scriptname))
            (save-excursion
              (dap-debug `(:type "python" :args ,args :cwd OBnil :target-module nil :request "launch" :name "Python :: Run Configuration")))
            (find-file (umn scriptname)))))
    (progn
      (let ((cbuf (current-buffer)))
        ;; (message "hi")
        (dap-debug `(:type "python" :args "" :cwd nil :target-module nil :request "launch" :name "Python :: Run Configuration"))
        (switch-to-buffer cbuf)))))


(define-key lsp-ui-peek-mode-map "j" (kbd "<down>"))
(define-key lsp-ui-peek-mode-map "k" (kbd "<up>"))

(define-key lsp-ui-peek-mode-map "h" (kbd "<left>"))
(define-key lsp-ui-peek-mode-map "l" (kbd "<right>"))

;; my-mode-map overrides this
(define-key lsp-ui-peek-mode-map (kbd "M-p") (kbd "<left>"))
(define-key lsp-ui-peek-mode-map (kbd "M-n") (kbd "<right>"))

;; (defun lsp:marked-string-value
;;       (object)
;;     (when
;;         (ht\? object)
;;       (gethash "value" object)))
;; (defun lsp:set-marked-string-value
;;     (object value)
;;   (puthash "value" value object)
;;   object)



;; This minimises the doc string
(defun lsp-ui-doc--extract-marked-string (marked-string &optional language)
  "Render the MARKED-STRING with LANGUAGE."
  (string-trim-right
   (let* ((string (if (stringp marked-string)
                      (mnm marked-string)
                    (lsp:markup-content-value marked-string)))
          (with-lang (lsp-marked-string? marked-string))
          (language (or (and with-lang
                             (or (lsp:marked-string-language marked-string)
                                 (lsp:markup-content-kind marked-string)))
                        language))
          (markdown-hr-display-char nil))
     (cond
      (lsp-ui-doc-use-webkit
       (if (and language (not (string= "text" language)))
           (format "```%s\n%s\n```" language string)
         string))
      (t (lsp--render-element (lsp-ui-doc--inline-formatted-string string)))))))


;; This minimises the sideline strings
(defun lsp-ui-sideline--extract-info (contents)
  "Extract the line to print from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We prioritize string with a language (which is probably a type or a
function signature)."
  (when contents
    (cond
     ((lsp-marked-string? contents)
      (lsp:set-marked-string-value contents (mnm (lsp:marked-string-value contents)))
      contents)
     ((vectorp contents)
      (seq-find (lambda (it) (and (lsp-marked-string? it)
                                  (lsp-get-renderer (lsp:marked-string-language it))))
                contents))
     ((lsp-markup-content? contents)
      ;; This successfully minimises haskell sideline strings
      (lsp:set-markup-content-value contents (mnm (lsp:markup-content-value contents)))
      contents))))


;; nadvice - proc is the original function, passed in. do not modify
;; (defun lsp-ui-peek-find-references-around-advice (proc &rest args)
;;   (let ((res (apply proc args)
;;              (tvipe "hi")))
;;     res)

;;   ;; (cl-letf (((symbol-function 'sexp-at-point) #'my/thing-at-point))
;;   ;;   (let ((res (apply proc args)))
;;   ;;     res))
;;   )
;; (advice-add 'lsp-ui-peek-find-references :around #'lsp-ui-peek-find-references-around-advice)
;; (advice-remove 'lsp-ui-peek-find-references #'lsp-ui-peek-find-references-around-advice)

(advice-remove-all-from 'lsp-ui-peek-find-references)


(defun lsp-ui-peek--find-xrefs (input method param)
  "Find INPUT references.
METHOD is ‘references’, ‘definitions’, `implementation` or a custom kind.
PARAM is the request params."
  (setq lsp-ui-peek--method method)
  (let ((xrefs (lsp-ui-peek--get-references method param)))
    (unless xrefs
      (user-error "Not found for: %s"  input))
    (xref-push-marker-stack)
    (when (featurep 'evil-jumps)
      (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
    (if (and (not lsp-ui-peek-always-show)
             (not (cdr xrefs))
             (= (length (plist-get (car xrefs) :xrefs)) 1))
        (error "Here is the only instance.")
        ;; (let ((x (car (plist-get (car xrefs) :xrefs))))
        ;;   (-if-let (uri (lsp:location-uri x))
        ;;       (-let (((&Range :start (&Position :line :character)) (lsp:location-range x)))
        ;;         (lsp-ui-peek--goto-xref `(:file ,(lsp--uri-to-path uri) :line ,line :column ,character)))
        ;;     (-let (((&Range :start (&Position :line :character)) (or (lsp:location-link-target-selection-range x)
        ;;                                                              (lsp:location-link-target-range x))))
        ;;       (lsp-ui-peek--goto-xref `(:file ,(lsp--uri-to-path (lsp:location-link-target-uri x)) :line ,line :column ,character)))))
        (lsp-ui-peek-mode)
        (lsp-ui-peek--show xrefs))))


;; The threshold didn't work, so I've disabled them
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-threshold 10)


(lsp-defun lsp-ui-doc--callback ((hover &as &Hover? :contents) bounds buffer)
  "Process the received documentation.
HOVER is the doc returned by the LS.
BOUNDS are points of the symbol that have been requested.
BUFFER is the buffer where the request has been made."
  (if
      (not (and
            hover
            (>= (point) (car bounds)) (<= (point) (cdr bounds))
            (eq buffer (current-buffer))))
      (setq contents "-")
    (setq contents (or (-some->>
                        ;; "shane"
                        contents
                        lsp-ui-doc--extract
                        (replace-regexp-in-string "\r" ""))
                       ;; (replace-regexp-in-string "\r" "" (lsp-ui-doc--extract contents))
                       "Cant extract or docs are empty")))

  (progn
    (setq lsp-ui-doc--bounds bounds)
    (lsp-ui-doc--display
     (thing-at-point 'symbol t)
     contents))
  ;; (lsp-ui-doc--hide-frame)
  )


(defun lsp-ui-doc--extract (contents)
  "Extract the documentation from CONTENTS.
CONTENTS can be differents type of values:
MarkedString | MarkedString[] | MarkupContent (as defined in the LSP).
We don't extract the string that `lps-line' is already displaying."
  ;; (tvipe contents)
  (cond
   ((vectorp contents) ;; MarkedString[]
    (mapconcat 'lsp-ui-doc--extract-marked-string
               (lsp-ui-doc--filter-marked-string (seq-filter #'identity contents))
               "\n\n"
               ;; (propertize "\n\n" 'face '(:height 0.4))
               ))
   ;; when we get markdown contents, render using emacs gfm-view-mode / markdown-mode
   ((and (lsp-marked-string? contents)
         (lsp:marked-string-language contents))
    (lsp-ui-doc--extract-marked-string (lsp:marked-string-value contents)
                                       (lsp:marked-string-language contents)))
   ((lsp-marked-string? contents) (lsp-ui-doc--extract-marked-string contents))
   ((and (lsp-markup-content? contents)
         (string= (lsp:markup-content-kind contents) lsp/markup-kind-markdown))
    (lsp-ui-doc--extract-marked-string (lsp:markup-content-value contents) lsp/markup-kind-markdown))
   ((and (lsp-markup-content? contents)
         (string= (lsp:markup-content-kind contents) lsp/markup-kind-plain-text))
    (lsp:markup-content-value contents))
   (t
    ;; This makes python work
    contents)))


;; lsp-ui-doc--extract
;; TODO Keep markdown
(defun my-lsp-get-hover-docs ()
  (interactive)
  (let* ((ht (lsp-request "textDocument/hover" (lsp--text-document-position-params)))
         (docs
          (if (hash-table-p ht)
              (lsp-ui-doc--extract (gethash "contents" ht))
            "")))
    (if (and docs (not (string-empty-p docs))) (if (called-interactively-p 'interactive)
                                                   ;; (tvd docs)
                                                   (new-buffer-from-string docs)
                                                 docs)
      (error "No docs"))))

(define-key lsp-mode-map (kbd "s-l 9") 'my-lsp-get-hover-docs)
(define-key lsp-mode-map (kbd "s-9") 'my-lsp-get-hover-docs)
(define-key global-map (kbd "s-i") 'lsp-install-server)


(setq lsp-enable-on-type-formatting nil)


;; (define-key lsp-ui-imenu-mode-map (kbd "M-n") (kbd "<down>"))

(advice-add 'lsp--document-highlight :around #'ignore-errors-around-advice)

;; (advice-add 'lsp--build-workspace-configuration-response :around #'ignore-errors-around-advice)
;; (advice-remove 'lsp--build-workspace-configuration-response #'ignore-errors-around-advice)


(defun lsp-list-all-servers ()
  (mapcar 'car (--map (cons (funcall
                             (-compose #'symbol-name #'lsp--client-server-id) it) it)
                      (or (->> lsp-clients
                               (ht-values)
                               (-filter (-andfn
                                         (-orfn (-not #'lsp--server-binary-present?)
                                                (-const t))
                                         (-not #'lsp--client-download-in-progress?)
                                         #'lsp--client-download-server-fn)))
                          (user-error "There are no servers with automatic installation")))))


(defun lsp-get-server-for-install (name)
  (interactive (list (fz (lsp-list-all-servers))))
  (cdr (car (-filter (lambda (sv) (string-equal (car sv) name))
                     (--map (cons (funcall
                                   (-compose #'symbol-name #'lsp--client-server-id) it) it)
                            (or (->> lsp-clients
                                     (ht-values)
                                     (-filter (-andfn
                                               (-orfn (-not #'lsp--server-binary-present?)
                                                      (-const t))
                                               (-not #'lsp--client-download-in-progress?)
                                               #'lsp--client-download-server-fn)))
                                (user-error "There are no servers with automatic installation")))))))

(defun lsp-install-server-by-name (name)
  (interactive (list (fz (lsp-list-all-servers))))
  (lsp--install-server-internal (lsp-get-server-for-install name)))


(defun lsp--sort-completions (completions)
  (lsp-completion--sort-completions completions))

(defun lsp--annotate (item)
  (lsp-completion--annotate item))

(defun lsp--resolve-completion (item)
  (lsp-completion--resolve item))

(defun lsp-install-server-update-advice (proc update)
  (cond
   (update (setq update nil))
   ((not update) (setq update t)))
  (let ((res (apply proc (list update))))
    res))
(advice-add 'lsp-install-server :around #'lsp-install-server-update-advice)
;; (advice-remove 'lsp-install-server #'lsp-install-server-update-advice)

;; (advice-add 'lsp-install-server :around #'invert-prefix-advice)
;; (advice-remove 'lsp-install-server #'invert-prefix-advice)


;; These commands should only be run when lsp is running
;; But I may want them here just to look them up
(define-key global-map (kbd lsp-keymap-prefix) lsp-command-map)




;; one of these breaks
(setq lsp-completion-no-cache t)
(setq lsp-display-inline-image nil)
;; (setq lsp-document-sync-method 'incremental)
(setq lsp-document-sync-method nil)
;; (setq lsp-document-sync-method 'full)
;; This makes bash look bad with an entire
(setq lsp-eldoc-render-all nil)
;; (setq lsp-eldoc-render-all nil)
(setq lsp-enable-dap-auto-configure t)
;; (setq lsp-enable-file-watchers t)
(setq lsp-enable-file-watchers nil)
(setq lsp-enable-folding t)
(setq lsp-enable-imenu t)
;; (setq lsp-enable-indentation nil)
(setq lsp-enable-indentation t)
(setq lsp-enable-links t)
;; (setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-on-type-formatting t)
;; (setq lsp-enable-semantic-highlighting nil)
(setq lsp-enable-semantic-highlighting t)
(setq lsp-enable-snippet t)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-text-document-color t)
(setq lsp-enable-xref t)
(setq lsp-folding-line-folding-only t)
;; nil no limit
(setq lsp-lens-debounce-interval 0.2)
(setq lsp-folding-range-limit nil)
;; (setq lsp-lens-enable nil)
(setq lsp-lens-enable t)
;; (setq lsp-log-io t)
(setq lsp-log-io nil)
;; (setq lsp-server-trace t)
(setq lsp-server-trace nil)
;; (setq lsp-print-performance t)
(setq lsp-print-performance nil)

(setq lsp-modeline-code-actions-enable t)

(setq lsp-modeline-code-actions-segments '(count name))
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file))


(defun lsp-on-change-around-advice (proc &rest args)
  (message "lsp-on-change called with args %S" args)
  (let ((res (apply proc args)))
    (message "lsp-on-change returned %S" res)
    res))
;; (advice-add 'lsp-on-change :around #'lsp-on-change-around-advice)
(advice-remove 'lsp-on-change #'lsp-on-change-around-advice)


(require 'lsp-headerline)
(defun lsp-headerline--arrow-icon ()
  "Build the arrow icon for headerline breadcrumb."
  ;; (if (require 'all-the-icons nil t)
  ;;     (all-the-icons-material "chevron_right"
  ;;                             :face 'lsp-headerline-breadcrumb-separator-face)
  ;;   (propertize "›" 'face 'lsp-headerline-breadcrumb-separator-face))
  (propertize "›" 'face 'lsp-headerline-breadcrumb-separator-face))

;; I'm not sure why, but it wont overload normally, so do this
;; (defun ... (defun does actually work
;; (add-hook 'lsp-mode-hook 'define-my-lsp-overridden)


(defun dired-lsp-binaries ()
  (interactive)
  (dired lsp-server-install-dir))


(defun lsp-ui-peek-find-references (&optional include-declaration extra)
  "Find references to the IDENTIFIER at point."
  (interactive)

  ;; (try-deselected-and-maybe-reselect
  ;;  (let ((thing (str2sym (my/thing-at-point))
  ;;               ;; (symbol-at-point)
  ;;               ))
  ;;    (lsp-ui-peek--find-xrefs thing
  ;;                             "textDocument/references"
  ;;                             (append extra (lsp--make-reference-params nil include-declaration)))))

  (let ((thing (str2sym (my/thing-at-point)))
        (p (point))
        (m (mark))
        (s (selected-p)))
    (deselect)
    (eval
     `(try
       ;; Try this, otherwise, reselect
       (lsp-ui-peek--find-xrefs ',thing
                                "textDocument/references"
                                (append ,extra (lsp--make-reference-params nil ,include-declaration)))
       (progn
         (set-mark ,m)
         (goto-char ,p)
         ,(if s
              '(progn
                 (activate-mark))
            '(progn
               (deactivate-mark)))
         (error "lsp-ui-peek-find-references failed"))))))



;; (defcustom lsp-racket-langserver-command '("racket" "--lib" "racket-langserver")
;;   "Command to start the server."
;;   :type 'string
;;   :package-version '(lsp-mode . "7.1"))
(defcustom lsp-racket-langserver-command '("racket-langserver")
  "Command to start the server."
  :type 'string
  :package-version '(lsp-mode . "7.1"))
(setq lsp-racket-langserver-command "racket-langserver")



;; This didn't work
;; These modes are yaml
;; (dolist (m '(yaml-mode
;;              gitlab-ci-mode))
;;   (add-to-list 'lsp-language-id-configuration `(,m . "yaml")))


;; I needed this
;; Yaml
;; vim +/"(lsp-register-client" "$EMACSD/packages26/lsp-mode-20200925.18/lsp-yaml.el"
;; Added gitlab-ci-mode
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     `(,(or (executable-find (cl-first lsp-yaml-server-command))
                                            (lsp-package-path 'yaml-language-server))
                                       ,@(cl-rest lsp-yaml-server-command))))
                  :major-modes '(yaml-mode
                                 gitlab-ci-mode)
                  :priority 0
                  :server-id 'yamlls
                  :initialized-fn (lambda (workspace)
                                    (with-lsp-workspace workspace
                                      (lsp--set-configuration
                                       (lsp-configuration-section "yaml"))))
                  :download-server-fn (lambda (_client callback error-callback _update?)
                                        (lsp-package-ensure 'yaml-language-server
                                                            callback error-callback))))



(defun lsp--create-default-error-handler-around-advice (proc &rest args)
  (lambda (e) nil)
  ;; (let ((res (apply proc args)))
  ;;   res)
  )
(advice-add 'lsp--create-default-error-handler :around #'lsp--create-default-error-handler-around-advice)


(defun lsp--error-string-around-advice (proc &rest args)
  nil
  ;; (let ((res (apply proc args)))
  ;;   (if res
  ;;       (progn
  ;;         ;; (message res)
  ;;         "error"))
  ;;   ;; res
  ;;   )
  )
(advice-add 'lsp--error-string :around #'lsp--error-string-around-advice)


(defun lsp-around-advice (proc &rest args)
  (if (myrc-test "lsp" ;; my-disable-lsp
                 )
      (let ((res (apply proc args)))
        res)))
(advice-add 'lsp :around #'lsp-around-advice)

(defun lsp-lens-refresh-around-advice (proc &rest args)
  (if (myrc-test "lsp_lens")
      (let ((res (apply proc args)))
        res)))
(advice-add 'lsp-lens-refresh :around #'lsp-lens-refresh-around-advice)

(provide 'my-lsp)
{{< /highlight >}}