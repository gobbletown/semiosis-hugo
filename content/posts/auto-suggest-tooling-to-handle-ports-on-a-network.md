+++
title = "Auto-suggest tooling to handle ports on a network"
author = ["Shane Mulligan"]
date = 2021-04-30T00:00:00+12:00
keywords = ["infra", "emacs"]
draft = false
+++

## Summary {#summary}

Given a server (default localhost), suggest tooling to handle said ports.

For example, if you are running a postgres
server, suggest `pgcli` (a `TUI`) to allow you
to connect to it.

Be more intelligent than this, though.


## Tools to accommodate {#tools-to-accommodate}

-   <https://github.com/dbcli/mycli>
-   <https://github.com/dbcli/pgcli>


### Files too somehow? {#files-too-somehow}

-   <https://github.com/dbcli/litecli>


## elisp {#elisp}


### Detect ports {#detect-ports}

<span class="underline">**shell**</span>
`n-list-open-ports`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

( hs "$(basename "$0")" "$@" "#" "<==" "$(ps -o comm= $PPID)" 0</dev/null ) &>/dev/null

hn="$1"
: "${hn:="localhost"}"

{
echo "Port State Service"
sudo nmap -sT -O "$hn" | sed "0,/^PORT /{d}" | sed "/^[^0-9]\\+/,\$d"
} | sed "s/ \\+/,/g" | pavs
{{< /highlight >}}

`ports-tablist`
This generates a CSV.

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

hn="$1"
: "${hn:=""}"

create-tablist n-list-open-ports ports t
{{< /highlight >}}

Add to the tablist mode list.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defset my-tablist-modes-cmds-or-paths
  '(
    ;; "arp -a | spaces2tabs | tsv2csv"
    "arp"
    "prompts"
    "ports"
    "aws-policies"
    "aws-users"
    "mygit")
  "A list of commands or csv paths to create tablist minor modes for")
{{< /highlight >}}

Create handlers in `my-server-suggest.el`.

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defset server-command-tuples '((sql-mysql 3306)
                                (sql-postgres 5432)))

(provide 'my-server-suggest)
{{< /highlight >}}


### Handle ports {#handle-ports}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(require 'sql)

;; Like format but quotes string arguments
;; This would be nice. It would be easy to achieve in racket
;; (defmacro formatq (&rest body)
;;   "Do not return anything."
;;   ;; body
;;   `(progn (,@body) nil))

(defun sql-mode-sps-tui ()
  (interactive)
  (if (major-mode-p 'sql-interactive-mode)
      (cond ((string-equal "mysql" sql-product)
             (sps (format "mycli -u %s --pass %s %s" (q sql-user) (q sql-password) (q sql-database))))
            ((string-equal "postgres" sql-product)
             (sps (format "pgcli -u %s -W %s -d %s" (q sql-user) (q sql-password) (q sql-database)))))
    (message "need to be in sql-mode")))

;; export PGPASSWORD=$POSTGRES_PASSWORD
  ;; - psql -h "postgres" -U "$POSTGRES_USER" -d "$POSTGRES_DB" -c "SELECT 'OK' AS status;"
(defun sql-mode-sps-cli ()
  (interactive)
  (if (major-mode-p 'sql-interactive-mode)
      (cond ((string-equal "mysql" sql-product)
             ;; (zrepl (format "mysql --user=%s --password=%s %s" (q sql-user) (q sql-password) (q sql-database)))
             (zreplcm (format "mysql --user=%s --password=%s %s" (q sql-user) (q sql-password) (q sql-database))))
            ((string-equal "postgres" sql-product)
             (sps (format "psql -U %s -W %s -d %s" (q sql-user) (q sql-password) (q sql-database)))))
    (message "need to be in sql-mode")))

(define-key sql-interactive-mode-map (kbd "M-i") 'sql-mode-sps-cli)
(define-key sql-interactive-mode-map (kbd "M-o") 'sql-mode-sps-tui)

(provide 'my-sql-mode)
{{< /highlight >}}


### Add to `tablist-modes` {#add-to-tablist-modes}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(defset my-tablist-mode-tuples
  '(("list-venv-dirs-csv" . ("venv" t "30 40 20"))
    ("n-list-open-ports" . ("ports" t))
    ("mygit-tablist" . ("mygit" t))
    ("arp-details" . ("arp" t))
    ("list-aws-iam-policies-csv" . ("aws-policies" t "30 80"))
    ("oci prompts-details -csv" . ("prompts" t "30 30 20 10 15 15 15 10"))
    ("upd list-aws-iam-users-csv" . ("aws-users" t "20 60 20"))))

(defun my-start-tablist ()
  (interactive)
  (let* ((tlname (fz (mapcar 'car my-tablist-mode-tuples)))
        (args
         (if (sor tlname)
             (assoc tlname my-tablist-mode-tuples)
             ;; (cdr (assoc tlname my-tablist-mode-tuples))
           )))
    (apply 'create-tablist args)))
{{< /highlight >}}


### Detect open ports and suggest {#detect-open-ports-and-suggest}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
(require 'my-net)

(defun connect-to-mysql (&optional hn port user dbname pass)
  (interactive (list (read-string-hist "mysql hn: ")
                     (read-string-hist "mysql port: ")
                     (read-string-hist "mysql user: ")
                     (read-string-hist "mysql db: ")
                     (read-string-hist "mysql pw: ")))

  (if (not (sor hn)) (setq hn "localhost"))
  (if (not (sor port)) (setq port "3306"))
  (if (not (sor user)) (setq user "admin"))
  (if (not (sor dbname)) (setq dbname "main"))
  (if (not (sor pass)) (setq pass "admin"))

  (sps (concat (cmd
                "mycli"
                "-initcmd" "\\d"
                "-h" hn
                "-p" port
                "-D" dbname
                "-y" user
                "-p" pass)
               "; pak")))

(defun connect-to-postgres (&optional hn port user dbname pass)
  (interactive (list (read-string-hist "pg hn: ")
                     (read-string-hist "pg port: ")
                     (read-string-hist "pg user: ")
                     (read-string-hist "pg db: ")
                     (read-string-hist "pg pw: ")))

  (if (not (sor hn)) (setq hn "localhost"))
  (if (not (sor port)) (setq port "5432"))
  (if (not (sor user)) (setq user "admin"))
  (if (not (sor dbname)) (setq dbname "main"))
  (if (not (sor pass)) (setq pass "admin"))

  (sps (concat (cmd
                "pgcli"
                "-pw" pass
                "-initcmd" "\\d"
                "-h" hn
                "-p" port
                "-d" dbname
                "-U" user
                "-W")
               "; pak")))

(defset server-command-tuples '((3306 . ((call-interactively 'connect-to-mysql)
                                         (call-interactively 'sql-mysql)))
                                (5432 . ((connect-to-postgres hn port "admin" "main" "admin")
                                         (connect-to-postgres hn port "ahungry" "ahungry" "ahungry")
                                         (call-interactively 'connect-to-postgres)
                                         (call-interactively 'sql-postgres)))))

(defun server-suggestions (hostname)
  (interactive (list (read-string-hist "hostname: ")))
  (let* ((open (n-list-open-ports hostname))
         (hnopen
          (mapcar
           (lambda (tp) (-drop 1 tp))
           (-filter (lambda (tp) (string-equal hostname (car tp)))
                    (n-list-open-ports hostname))))
         (openmap
          (mapcar
           (lambda (tp)
             (cons (string-to-int (car tp))
                   (cdr tp)))
           hnopen))
         ;; (etv openmap)
         (suggestions
          (flatten-once
           (-filter
            'identity
            (cl-loop
             for tp in
             openmap
             collect
             ;; https://en.wikipedia.org/wiki/Relational_algebra
             (let* ((cand (assoc (car tp) server-command-tuples))
                    (hnport (list hostname (car cand)))
                    (cs (cdr cand)))
               (if cand
                   (cl-loop for subtp in cs collect
                            (append hnport subtp)))
               ))))))
    suggestions))

(defun server-suggest (hostname)
  (interactive (list (read-string-hist "hostname: ")))
  (let* ((ss (server-suggestions hostname))
         (c (fz (mapcar 'pp-oneline ss))))
    (if c
        (let* ((sel (my-eval-string (concat "'" c)))
               (hn (car sel))
               (port (str (second sel)))
               (c2 (-drop 2 sel)))
          (eval c2)))))
{{< /highlight >}}


## Testing it out {#testing-it-out}


### Start postgres with docker {#start-postgres-with-docker}

{{< highlight sh "linenos=table, linenostart=1" >}}
docker \
    run \
    --rm \
    -p 5432:5432 \
    -e POSTGRES_PASSWORD=admin \
    -e POSTGRES_USER=admin \
    -e POSTGRES_DB=main \
    -v "$(pwd):/$(pwd | slugify)" \
    -w "/$(pwd | slugify)" \
    -ti \
    --entrypoint= \
    postgres:alpine \
    docker-entrypoint.sh \
    postgres
{{< /highlight >}}


### Demo {#demo}

emacs will suggest programs to connect to open
ports for a host.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qkK74e4Y4IbLSEYJm6SLOd0gm" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qkK74e4Y4IbLSEYJm6SLOd0gm.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qkK74e4Y4IbLSEYJm6SLOd0gm.js" id="asciicast-qkK74e4Y4IbLSEYJm6SLOd0gm" async></script>

Thanks to this, I will no longer need to
remember which programs I commonly associate
with certain ports.

As I am browsing the network through emacs, I
will be able to click on a host and select
from suggested programs to interact with ports
on that host.