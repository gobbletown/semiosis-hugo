+++
title = "Auto-suggest tooling to handle ports on a network"
author = ["Shane Mulligan"]
date = 2021-04-30T00:00:00+12:00
keywords = ["infra"]
draft = false
+++

<span class="underline">**Work in progress**</span>


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

<span class="underline">**Demo of tablist modes**</span>

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/a0wET1hJtxz3CqANFRWuRHu8c" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/a0wET1hJtxz3CqANFRWuRHu8c.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/a0wET1hJtxz3CqANFRWuRHu8c.js" id="asciicast-a0wET1hJtxz3CqANFRWuRHu8c" async></script>


## Testing it out {#testing-it-out}

Start postgres with docker.

{{< highlight sh "linenos=table, linenostart=1" >}}
docker \
    run \
    --rm \
    -p 5432:5432 \
    -e POSTGRES_PASSWORD=admin \
    -v "$(pwd):/$(pwd | slugify)" \
    -w "/$(pwd | slugify)" \
    -ti \
    --entrypoint= \
    postgres:alpine \
    docker-entrypoint.sh \
    postgres
{{< /highlight >}}