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


## elisp {#elisp}


### Detect ports {#detect-ports}

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