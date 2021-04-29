+++
title = "Creating a clojure app with terraform and AWS Cloud"
author = ["Shane Mulligan"]
date = 2021-04-27T00:00:00+12:00
keywords = ["aws"]
draft = false
+++

## Summary {#summary}

These are the steps I have taken to automate
the process of building a Web Application in
Clojure and deploying said application to an
AWS cluster defined by a Terraform module.

This is a work in progress.


## Original article {#original-article}

<https://dzone.com/articles/deploy-a-clojure-web-application-to-aws-using-terr>

I have taken steps and copied code from this article.


## Design {#design}

I have chosen Clojure as the language to
implement the blob uploader for the following reasons:

-   It is very reliable
-   It is easy to understand
-   REPL workflow
    -   Live updates
-   One language for both the back-end and front-end
-   It's the language I'm growing into for most of my work
-   It has very good support for concurrency
-   Access to all of Java's libraries
-   Access to all of Python's libraries via `clj-python/libpython-clj`

I was initially working with `Terraform 0.15` on `Ubuntu 20.04`,
but I regressed to `Terraform v0.14.0-dev` and `Ubuntu 16.04` due to my computer breaking down.


## Project Code {#project-code}

-   <http://github.com/mullikine/blob-uploader-terraform>
-   <http://github.com/mullikine/blob-uploader>
-   Notes:
    -   <https://github.com/mullikine/code-org-tidbits>


## Current state {#current-state}


### What's working {#what-s-working}

-   Cluster is successfully created using `terraform apply`.
-   Deployment of the web app to the cluster is working.


### Things I fixed from the original code {#things-i-fixed-from-the-original-code}

-   `UserData` and installing `amazon-efs-utils` on the Ubuntu instances.
    -   The original code used the Amazon ECS image.


### What's not working yet {#what-s-not-working-yet}

-   So far no communication with the web app on port 3000.


## Problems so far {#problems-so-far}

-   The computer I was using died one day into starting this project, so I've had to continue
    on an older computer, but it has a much older environment and is lacking many conveniences.


## Things I have automated for the purpose of this project {#things-i-have-automated-for-the-purpose-of-this-project}

-   Terraform editor
    -   Parsing terraform files with `json2hcl`.
        <https://asciinema.org/a/1lqm2POncL3NmuqmYIynRurMH>
-   Diagram editing with `plantuml` for AWS infrastructure diagrams
-   Cluster creation
    -   Ubuntu 20.04
-   Deployment to the cluster
-   aws cli command completion
-   aws iam users and policies management in emacs
    -   Add and remove policies from users
-   ec2 instance management in emacs
    -   inspect ec2 instances
    -   display `UserData`
    -   log into instances
    -   start and stop instances
-   Clojure editing
    -   Parsing `project.clj` with babashka to automate dependency imports.


## Automations {#automations}


### Parsing `project.clj` with babashka to automate dependency imports {#parsing-project-dot-clj-with-babashka-to-automate-dependency-imports}

`clojure-list-deps`

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
<!-- <a title="asciinema recording" href="https://asciinema.org/a/WT5T7OVcNYk9CENb8gVAH0DHa" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/WT5T7OVcNYk9CENb8gVAH0DHa.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/WT5T7OVcNYk9CENb8gVAH0DHa.js" id="asciicast-WT5T7OVcNYk9CENb8gVAH0DHa" async></script>


### `direnv` {#direnv}

I have stored the actual passwords for `tf` in my
private password store, but direnv can still
retrieve them.

{{< highlight sh "linenos=table, linenostart=1" >}}
curl -sfL https://direnv.net/install.sh | bash
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
export TF_VAR_aws_access_key_id="$(myrc .tf_aws_access_key_id)"
export TF_VAR_aws_secret_access_key="$(myrc .tf_aws_secret_access_key)"
{{< /highlight >}}


### Automate terraform editor {#automate-terraform-editor}

While I am editing `terraform` or any type of document, I may
click on things that have been automatically
highlighted according to rules I specify.

This runs commands based on the text I selected.

I've automated the following:

-   Describing any `ami` used in terraform.
-   Selecting different types of instances.
-   Selecting different regions.
-   Selecting an `ami` from an owner such as Canonical.
-   Displaying terraform documention for `resources` and other things inside `tf` files.
-   migrating terraform files

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/fneAXIjLJhseQhikfTRw546kQ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/fneAXIjLJhseQhikfTRw546kQ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/fneAXIjLJhseQhikfTRw546kQ.js" id="asciicast-fneAXIjLJhseQhikfTRw546kQ" async></script>

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Given an external filter script, which finds substrings of a file,
;; buttonize those strings within in the buffer. Clicking on one of
;; these buttons should do something useful

(defsetface filter-cmd-button-face
  '((t :foreground "#66cc00"
       ;; It's better for the glossary buttons to have no background, so normal syntax things, such as LSP highlighting can still be visible
       ;; underline is enough
       ;; :background "#2e2e2e"
       :background nil
       :weight bold
       :underline t))
  "Face for filter-cmd buttons.")

(define-button-type 'filter-cmd-button 'follow-link t 'help-echo "Click to run command" 'face 'filter-cmd-button-face)

(defset filter-cmd-buttonize-2-tuples
  ;; Replace %q with a quoted argument
  '(("scrape \"\\bami-[a-z0-9]+\\b\"" "sps zrepl -cm pavit aws ec2 describe-images --image-ids %q")
    ;; ("sed -n 's/.*instance_type\\s*=\\s*\"\\([^\"]*\\)\".*/\\1/p'" "sps zrepl -cm pavit aws ec2 describe-instance-types --instance-types")
    ("sed -n 's/.*instance_type\\s*=\\s*\"\\([^\"]*\\)\".*/\\1/p'" "sps aws-list-instance-types")
    ("sed -n 's/\\bregion\\s*=\\s*\"\\([^\"]*\\)\".*/\\1/p'" "sps aws-list-regions")
    ;; ("sed -n 's/.*\\bowners\\s*=\\s*\\[\"\\([^\"]*\\)\"\\].*/\\1/p'" "sps aws-list-image-names-from-owner")
    ("json2hcl -reverse | jq -r '.data[].aws_ami[][][].owners[]'" "sps aws-list-image-names-from-owner")
    ;; ("sed -n 's/^resource \\s*\"\\([^\\\"]*\\)\" \"[^\\\"]*\" *{$/\\1/p'" "go-to-terraform-resource %q")
    ("scrape-terraform-resource" "go-to-terraform-resource %q")))


(add-hook 'terraform-mode-hook 'make-buttons-for-all-filter-cmds)


(defun remove-filter-cmd-buttons-over-region (beg end)
  (interactive "r")
  (remove-overlays beg end 'face 'filter-cmd-button-face))

(defun remove-all-filter-cmd-buttons (beg end)
  (interactive "r")
  (remove-filter-cmd-buttons-over-region (point-min) (point-max)))
(defalias 'clear-filter-cmd-buttons 'remove-all-filter-cmd-buttons)

(defun get-filter-cmd-button-data-at (p)
  (interactive (list (point)))
  (-filter
   (l (tp)
     (apply 'gnus-and tp))
   (cl-loop
    for
    o
    in
    (overlays-at p)
    collect
    (list
     (button-get o 'term)
     (button-get o 'runfunc)
     (button-get o 'filtercmd)))))

(defun filter-cmd-button-pressed (button)
  "When I press a filtercmd button, it should run the button's function"
  (let* (
         ;; (term (button-get-text button))
         (term (button-get button 'term))
         (runfunc (button-get button 'runfunc))
         (start (button-start button))
         (filtercmd (button-get button 'filtercmd))
         (buttons-data-here (get-filter-cmd-button-data-at start)))

    (if (< 1 (length buttons-data-here))
        (let* ((button-line (umn (fz (mnm (pp-map-line buttons-data-here)))))
               (button-tuple (if button-line
                                 (my-eval-string (concat "'" button-line))))
               (selected-button (if button-tuple
                                    (car (-filter (l (li) (and (equal (first button-tuple) (button-get li 'term))
                                                               (equal (second button-tuple) (button-get li 'runfunc))
                                                               (equal (third button-tuple) (button-get li 'filtercmd))))
                                                  (overlays-at start))))))
          (if selected-button
              (progn
                (setq button selected-button)
                ;; (setq term (button-get-text button))
                (setq term (button-get button 'term))
                (setq runfunc (button-get button 'runfunc))
                (setq start (button-start button))
                ;; filtercmd isnt used here
                (setq filtercmd (button-get button 'filtercmd))
                (setq buttons-data-here (get-filter-cmd-button-data-at start)))
            (backward-char))))
    (cond
     ((equal current-prefix-arg (list 4)) (setq current-prefix-arg nil))
     ((not current-prefix-arg) (setq current-prefix-arg (list 4))))

    (funcall runfunc term)))

(defun create-buttons-for-filtrate (term beg end filtercmd runfunc buttontype)
  ""
  (if (not buttontype)
      (setq buttontype 'filter-cmd-button))

  (goto-char beg)
  (let ((pat
         (concat
          "\\(\\b\\|[. ']\\|^\\)"
          (regexp-quote term)
          "s?\\(\\b\\|[. ']\\|$\\)")))
    (while (re-search-forward pat end t)
      (progn
        ;; (message "%s" (concat "searching forward " (str (point))))
        (let ((contents (match-string 0))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (make-button
           (if (string-match "^[ '.].*" contents)
               (+ beg 1)
             beg)
           (if (string-match ".*[' .]$" contents)
               (- end 1)
             end)
           'term term
           'runfunc runfunc
           'filtercmd filtercmd
           'action 'filter-cmd-button-pressed
           'type buttontype))))))


(defun make-buttons-for-filter-cmd (beg end filtercmd runcmd &optional clear-first)
  "Makes buttons for terms found by filter-cmd in this buffer."
  (interactive (list (point-min)
                     (point-max)
                     (read-string-hist "filter-cmd: ")
                     (read-string-hist "runcmd %s: ")))

  (if clear-first (remove-all-filter-cmd-buttons))

  (let* ((terms (-filter 'sor (-uniq (str2list (snc filtercmd (region2string beg end))))))
         (runfunc (eval `(lambda (term) (sn
                                         (if (re-match-p "%q" ,runcmd)
                                             (s-replace-regexp "%q" (q term) ,runcmd)
                                           (concat ,runcmd " " (q term))))))))
    (if (not (or (major-mode-p 'org-modmfse)
                 (major-mode-p 'outline-mode)
                 (string-equal (buffer-name) "*glossary cloud*")))
        (save-excursion
          (cl-loop for term in terms do
                   (progn
                     (message "creating for %s" term)
                     (create-buttons-for-filtrate
                      term
                      beg end
                      ;; This is just to make it easy to introspect
                      filtercmd
                      runfunc
                      'filter-cmd-button)))))))

(defun make-buttons-for-all-filter-cmds (&optional clear-first)
  (interactive)
  (cl-loop for tp in filter-cmd-buttonize-2-tuples do
           (make-buttons-for-filter-cmd
            (point-min) (point-max)
            (car tp)
            (second tp)
            clear-first)))

(provide 'my-filter-cmd-buttonize)
{{< /highlight >}}


### Key management {#key-management}

{{< highlight bash "linenos=table, linenostart=1" >}}
aws ec2 describe-key-pairs
{{< /highlight >}}

```bash
{
    "KeyPairs": [
        {
            "KeyPairId": "key-09fb6b77288849f3b",
            "KeyFingerprint": "f2:77:ec:1f:8c:3d:65:23:12:1a:65:70:5a:0b:6f:c6:fc:46:d4:cd",
            "KeyName": "blob_uploader_key_pair",
            "Tags": []
        }
    ]
}
```

Because the private key isn't stored in AWS and can
be retrieved only when it's created, you can't
recover it later.

When I create the key pair, I should also
download and save the json.


### Create key {#create-key}

A caveat
: There is no way yet to specify a region for the cli subcommand, so this step must be done manually via the AWS console
    <https://docs.aws.amazon.com/cli/latest/reference/ec2/create-key-pair.html>

<!--listend-->

{{< highlight sh "linenos=table, linenostart=1" >}}
# The would-be way to create the key
aws ec2 create-key-pair --key-name blob_uploader_key_pair
{{< /highlight >}}

-   Steps automated:
    -   Download key to ssh file when it's created

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
jq -r .KeyMaterial > ~/.ssh/ids/AdminKey.pem
{{< /highlight >}}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/yIZ9z3L36dOVmr1Br90Rvm9dN" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/yIZ9z3L36dOVmr1Br90Rvm9dN.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/yIZ9z3L36dOVmr1Br90Rvm9dN.js" id="asciicast-yIZ9z3L36dOVmr1Br90Rvm9dN" async></script>


### Set up `aws` user accounts {#set-up-aws-user-accounts}

<https://docs.aws.amazon.com/IAM/latest/UserGuide/getting-started%5Fcreate-admin-group.html>

I have chosen to automate the process with the `aws` `cli`.

{{< highlight bash "linenos=table, linenostart=1" >}}
oci aws iam create-user --user-name Administrator
# Enable console login
oci aws iam create-login-profile --user-name Administrator --password "$(pwgen 30 1 | tee -a $NOTES/personal/passwords/aws-administrator.txt)"
# Change the pasword
oci aws iam update-login-profile --user-name Administrator --password "$(pwgen 30 1 | tee -a $NOTES/personal/passwords/aws-administrator.txt)"
# Enable programmatic access
oci aws iam create-access-key --user-name Administrator >> $NOTES/personal/passwords/aws-administrator-programmatic.json
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
oci aws iam create-user --user-name Administrator
{{< /highlight >}}

```bash
{
    "User": {
        "Path": "/",
        "UserName": "Administrator",
        "UserId": "AIDAR55HCH7KNSLMHLBLO",
        "Arn": "arn:aws:iam::132957487060:user/Administrator",
        "CreateDate": "2021-04-26T00:07:09Z"
    }
}
```

{{< highlight text "linenos=table, linenostart=1" >}}
AdministratorAccess
{{< /highlight >}}


### Select the `AdministratorAccess` policy and add to `Administrator` {#select-the-administratoraccess-policy-and-add-to-administrator}

{{< highlight bash "linenos=table, linenostart=1" >}}
oci aws iam list-policies | jq -r ".Policies[] | [ .PolicyName, .Arn ] | @csv"
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
"AdministratorAccess","arn:aws:iam::aws:policy/AdministratorAccess"
{{< /highlight >}}

Automated user and policies management with `tabulated-list-mode`
: Using emacs, tablist and this tool (<http://harelba.github.io/q/>), create tabulated list modes for managing aws.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/g6Cc64u1p5lF9geoSmCKgwmd7" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/g6Cc64u1p5lF9geoSmCKgwmd7.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/g6Cc64u1p5lF9geoSmCKgwmd7.js" id="asciicast-g6Cc64u1p5lF9geoSmCKgwmd7" async></script>

The above is a demonstration of adding the `AdministratorAccess` policy to a user account.

`aws-policy-tablist`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

create-tablist list-aws-iam-policies-csv aws-policies t "30 80"
{{< /highlight >}}

`list-aws-iam-policies-csv`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

{
echo name,arn
unbuffer oci aws iam list-policies | jq -r ".Policies[] | [ .PolicyName, .Arn ] | @csv"
} | mnm | pavs
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
"AdministratorAccess","arn:aws:iam::aws:policy/AdministratorAccess"
{{< /highlight >}}

{{< highlight bash "linenos=table, linenostart=1" >}}
oci aws iam attach-user-policy --user-name Administrator --policy-arn "arn:aws:iam::aws:policy/AdministratorAccess"
{{< /highlight >}}


## Create a diagram of the cluster {#create-a-diagram-of-the-cluster}

Automate the process of building infra diagrams with plantuml.

`:$SCRIPTS/plantuml-list-sprites`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

IFS= read -r -d '' puml <<HEREDOC
!include <awslib/AWSCommon>
!include <awslib/Compute/all.puml>
!include <awslib/mobile/all.puml>
!include <awslib/general/all.puml>

listsprites
HEREDOC

printf -- "%s\n" "$puml" | plantuml | sed 's/\s\+/\n/g' | sed '/^$/d' | pavs
{{< /highlight >}}

The above script lists the available `aws` sprites.

It's a standard lib (code: <https://github.com/plantuml/plantuml-stdlib/tree/master/awslib>).

This markdown file contains a more comprehensive list of sprites.

<https://github.com/awslabs/aws-icons-for-plantuml/blob/main/AWSSymbols.md>

{{< highlight plantuml "linenos=table, linenostart=1" >}}
'!include <tupadr3/common>
'!include <office/Servers/application_server>

!include <awslib/AWSCommon>
!include <awslib/AWSSimplified.puml>
!include <awslib/Compute/all.puml>
!include <awslib/mobile/all.puml>
!include <awslib/general/all.puml>
!include <awslib/GroupIcons/all.puml>
!include <awslib/NetworkingAndContentDelivery/all.puml>

skinparam linetype polyline

' EC2()
ELBApplicationLoadBalancer(ALBLoadBalancer, "ALB (Application Load Balancer)", " ")
ELBNetworkLoadBalancer(NLBLoadBalancer, "NLB (Network Load Balancer)", " ")
EC2AutoScaling(AutoscalingGroup, "Autoscaling Group", " ")
EC2InstancewithCloudWatch(InstanceCW, "Instance with CloudWatch", " ")
General(App1, "App", " ")
'OFF_APPLICATION_SERVER(App1, "App")
General(App2, "App", " ")
GenericDatabase(DB, "Database", " ")

' ELBApplicationLoadBalancer
' ELBNetworkLoadBalancer
ALBLoadBalancer -down-> InstanceCW: Port 3000
NLBLoadBalancer -down-> InstanceCW: Port 5432
AutoscalingGroup -right-> InstanceCW
InstanceCW -down-> App1: Port 3000
InstanceCW -down-> App2: Port 3000
InstanceCW -down-> DB: Port 5432
{{< /highlight >}}

{{< figure src="/ox-hugo/terraformclojure.svg" >}}


## Set up more tools {#set-up-more-tools}


### Set up `saws` {#set-up-saws}

{{< highlight sh "linenos=table, linenostart=1" >}}
x -sh saws -z "saws>" -s "aws " -i
{{< /highlight >}}

I used this to query the `aws` `cli` for arguments and options.

I have automated the initial entry and copying back from the REPL.

<https://asciinema.org/a/3IZcJYMYcC1q0Z8fSBrCCMO3K>


### `aws-fuzzy-finder` {#aws-fuzzy-finder}

Instead of setting up the following tool, I extended emacs' `aws-instances` plugin.

<https://github.com/pmazurek/aws-fuzzy-finder>


## Cluster successfully created with terraform {#cluster-successfully-created-with-terraform}

-   Ubuntu 20.04
-   `t2.micro` (free tier)


### First attempt at `terraform apply` (creating the cluster) failed {#first-attempt-at-terraform-apply--creating-the-cluster--failed}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Jiq2phUugU6LnPMFcYQXxl8Tc" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Jiq2phUugU6LnPMFcYQXxl8Tc.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Jiq2phUugU6LnPMFcYQXxl8Tc.js" id="asciicast-Jiq2phUugU6LnPMFcYQXxl8Tc" async></script>

{{< highlight text "linenos=table, linenostart=1" >}}
Error: Error creating launch configuration: ValidationError: The key pair 'blob_uploader_key_pair' does not exist
        status code: 400, request id: 42206521-5721-44ce-9a11-7bc24d1b440c

  on launch-configuration.tf line 1, in resource "aws_launch_configuration" "ecs-launch-configuration":
   1: resource "aws_launch_configuration" "ecs-launch-configuration" {
{{< /highlight >}}


### Successful `terraform apply`. Cluster created. {#successful-terraform-apply-dot-cluster-created-dot}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/zrCqjoxfv1h0n6PshGRCjrBDx" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/zrCqjoxfv1h0n6PshGRCjrBDx.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/zrCqjoxfv1h0n6PshGRCjrBDx.js" id="asciicast-zrCqjoxfv1h0n6PshGRCjrBDx" async></script>


## Automate adding my public key to an instance and ssh into the box {#automate-adding-my-public-key-to-an-instance-and-ssh-into-the-box}

-   Inspect from emacs `aws-instances`
-   Get region and availability zone

<https://aws.amazon.com/blogs/compute/new-using-amazon-ec2-instance-connect-for-ssh-access-to-your-ec2-instances/>

{{< highlight bash "linenos=table, linenostart=1" >}}
aws ec2-instance-connect send-ssh-public-key --region us-west-1 --availability-zone us-west-1a --instance-id i-034950c831ac772a5 --instance-os-user ubuntu --ssh-public-key file://$HOME/.ssh/pub/id_rsa.pub
{{< /highlight >}}

After running above command, for the next 60 seconds I can ssh in to the box.

{{< highlight sh "linenos=table, linenostart=1" >}}
ssh -oBatchMode=no -vvv -i $HOME/.ssh/ids/default.pem ubuntu@ec2-3-101-73-201.us-west-1.compute.amazonaws.com
{{< /highlight >}}


### I had to make some modifications to the `UserData` of `launch-configuration.tf` {#i-had-to-make-some-modifications-to-the-userdata-of-launch-configuration-dot-tf}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/zaKN40J2LyMjxnR1qIetAikvQ" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/zaKN40J2LyMjxnR1qIetAikvQ.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/zaKN40J2LyMjxnR1qIetAikvQ.js" id="asciicast-zaKN40J2LyMjxnR1qIetAikvQ" async></script>

-   Destroy the cluster and reapply terraform to see if it works now/sticks.
-   This fixed the problem.
-   I also needed to unindent the HEREDOC for `user_data`.

<!--listend-->

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
sudo mkdir -m 777 -p /etc/ecs; sudo chown $USER:$USER /etc/ecs
# echo ECS_CLUSTER=${var.ecs_cluster} >> /etc/ecs/ecs.config
echo ECS_CLUSTER=${var.ecs_cluster} >> /etc/ecs/ecs.config
sudo mkdir -p /mnt/efs/postgres; sudo chown $USER:$USER /mnt/efs/postgres
cd /mnt
# sudo yum install -y amazon-efs-utils
(
sudo apt-get update
sudo apt-get -y install git binutils
sudo chmod 777 /mnt
git clone https://github.com/aws/efs-utils
cd efs-utils
./build-deb.sh
sudo sh -c 'apt-get update && apt-get install stunnel4'
sudo apt-get -y install ./build/amazon-efs-utils*deb
)

# I have confirmed this command works
sudo mount -t efs ${aws_efs_mount_target.blobdbefs-mnt.0.dns_name}:/ efs
# sudo mount -t efs fs-3d3ad725.efs.us-west-1.amazonaws.com:/ efs
{{< /highlight >}}


#### Reapply to see if it worked {#reapply-to-see-if-it-worked}

It takes significant time to shut down.


### Automated showing the `userData` of an instance in emacs {#automated-showing-the-userdata-of-an-instance-in-emacs}

{{< highlight emacs-lisp "linenos=table, linenostart=1" >}}
;; Make an ssh into box script

(defun aws-ssh-into-box (id)
  (interactive (list (tabulated-list-get-id)))

  (if (major-mode-p 'aws-instances-mode)
      (sps (concat "aws-ssh-into-box " id))))

(defun aws-show-user-data (id)
  (interactive (list (tabulated-list-get-id)))

  (if (major-mode-p 'aws-instances-mode)
      ;; https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/user-data.html
      (etv (snc (concat
                 "aws ec2 describe-instance-attribute --instance-id "
                 id
                 " --attribute userData --output text --query \"UserData.Value\" | base64 --decode")))
    ;;
    ;; (sps (concat "aws-ssh-into-box " id))
    ))

(define-key aws-instances-mode-map (kbd ";") 'aws-ssh-into-box)
(define-key aws-instances-mode-map (kbd "D") 'aws-show-user-data)

(provide 'my-aws)
{{< /highlight >}}


### Automate collection of `terraform apply output` {#automate-collection-of-terraform-apply-output}

-   Specifically variables
    -   Then I can collect database of cluster states
-   Partially done
    -   All outputs of the `aws` and `terraform` commands are stored in a database.


## automate terraform file migration {#automate-terraform-file-migration}

Terraform has auto-upgrade tooling e.g.
`terraform 0.12upgrade` but they only work
from version to consecutive version. I was
migrating older `tf` files so I made my own
migration script.

`migrate-terraform`

{{< highlight bash "linenos=table, linenostart=1" >}}
#!/bin/bash
export TTY

# Terraform has auto-upgrade tooling
# terraform 0.12upgrade

while [ $# -gt 0 ]; do opt="$1"; case "$opt" in
    "") { shift; }; ;;
    -f) {
        force=y
        shift
    }
    ;;

    *) break;
esac; done

stdin_exists() {
    ! [ -t 0 ] && ! test "$(readlink /proc/$$/fd/0)" = /dev/null
}

# Interestingly, heredocs should not be 'uninterpolated'
# sp +/"container_definitions = <<DEFINITION" "$MYGIT/chrishowejones/film-ratings-terraform/film-ratings-db-task-definition.tf"

if stdin_exists; then
    # sed '/"\${/{s/"\${\([^}]*\)}"/\1/g;s/\\"/"/g;s/\bvar\.//}'
    sed '/"\${/{s/"\${\([^}]*\)}"/\1/g;s/\\"/"/g;}' |
        sed '/ \[ *"[^"]\+\.[^"]\+" *\]/{s/\[ *"\([^"]\+\.[^"]\+\)" *\]/[ \1 ]/g}'
elif test "$force" = y || yn "Recursively migrate?"; then
    find . -type f -name '*.tf' | awk1 | while IFS=$'\n' read -r line; do
        (
        exec 0</dev/null
        cat "$line" | migrate-terraform | sponge "$line"
        )
    done
fi
{{< /highlight >}}