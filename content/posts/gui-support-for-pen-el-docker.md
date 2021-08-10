+++
title = "GUI support for Pen.el Docker"
author = ["Shane Mulligan"]
date = 2021-08-10T00:00:00+12:00
keywords = ["pen", "emacs", "gpt"]
draft = false
+++

## Summary {#summary}

I add GUI support for the Pen.el Docker image.
Copy and paste works to and from the host.

{{< figure src="/ox-hugo/pen-gui.png" >}}

This functionality is enabled by default.

{{< highlight bash "linenos=table, linenostart=1" >}}
case "$docker_cmd" in
    run) {
IFS= read -r -d '' shcode <<HEREDOC
    # --user "$(id -u):$(id -g)"
    docker "$docker_cmd" \
        $(test -n "$OPENAI_API_KEY" && printf -- "%s " -e "OPENAI_API_KEY:$OPENAI_API_KEY" ) \
        $(test -n "$PEN_CONFIG_DIR" && printf -- "%s " -v "$PEN_CONFIG_DIR:/root/.pen" ) \
        $(test -n "$PROMPTS_DIR" && printf -- "%s " -v "$PROMPTS_DIR:/root/.emacs.d/host/prompts" ) \
        $(test -n "$PENEL_DIR" && printf -- "%s " -v "$PENEL_DIR:/root/.emacs.d/host/pen.el" ) \
        $(test -n "$GLOSSARIES_DIR" && printf -- "%s " -v "$GLOSSARIES_DIR:/root/.emacs.d/host/glossaries" ) \
        $(test -n "$OPENAI_API_EL_DIR" && printf -- "%s " -v "$OPENAI_API_EL_DIR:/root/.emacs.d/host/openai-api.el" ) \
        $(test "$HAS_TTY" = y && printf -- "%s " -ti ) \
        --privileged \
        --env COLORFGBG \
        --env DISPLAY \
        --env EMAIL \
        --env GIT_AUTHOR_EMAIL \
        --env GIT_AUTHOR_NAME \
        --env GIT_COMMITTER_EMAIL \
        --env GIT_COMMITTER_NAME \
        --env SSH_AUTH_SOCK \
        --env TERM \
        --env "TIMEZONE=UTC" \
        --env "VIDEO_GROUP_ID=44" \
        -v /dev/dri:/dev/dri \
        -v /dev/shm:/dev/shm \
        -v /tmp/.X11-unix:/tmp/.X11-unix \
        -v /run/user/1000/keyring/ssh:/run/user/1000/keyring/ssh \
        "--cap-add=SYS_PTRACE" \
        "--cap-add=SYS_ADMIN" \
        "--cap-add=NET_ADMIN" \
        --ulimit "rtprio=100:100" \
        --network host \
        -v /var/log/coredumps:/var/log/coredumps \
        --expose 57575 -p 57575:57575 \
        --entrypoint= --name=pen semiosis/pen.el:latest "$remote_cmd"
HEREDOC
    }
    ;;
{{< /highlight >}}

The following is the `pen-hf.py` script open
in GUI emacs, where I have used `pen.el` to
transpile a segment of code from Python into
Ruby using GPT-3. The tranpilation is very
accurate.

{{< figure src="/ox-hugo/pen-gui-translation.png" >}}