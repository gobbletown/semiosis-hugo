+++
title = "cterm (Complex Terminal)"
author = ["Shane Mulligan"]
date = 2021-11-15T00:00:00+13:00
keywords = ["𝑖i", "imaginary", "pen"]
draft = false
+++

| Install with Pen |                                                      |
|------------------|------------------------------------------------------|
| Pen.el on GitHub | <https://github.com/semiosis/pen.el/>                |
| Tutorial         | <https://mullikine.github.io/posts/pen-el-tutorial/> |


## Summary {#summary}

The complex terminal built into `Pen.el` is a
regular terminal that has an imaginary
component.

You may connect the dockerized `cterm` to any
host process simply by prefixing `ct` to any
shell command.

Original article
: <https://mullikine.github.io/posts/imaginary-real-codex-complex/>


## This is how easy it is to use {#this-is-how-easy-it-is-to-use}

-   First start the pen server
-   then prefix `ct` before vim or bash, or any command which runs on your host
    -   it will then run that command inside `pen.el` and you get all the autocompletion, etc. so it's a real terminal with `Pen.el` wrapped around it
-   Now you can use pen with vim, or nano, or codexify anything!

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/qf4EMRKxaKNZAB23SaVHciiES" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/qf4EMRKxaKNZAB23SaVHciiES.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/qf4EMRKxaKNZAB23SaVHciiES.js" id="asciicast-qf4EMRKxaKNZAB23SaVHciiES" async></script>


### Another example {#another-example}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/38xoJzrUrBC1dJrsIXeOD3Sni" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/38xoJzrUrBC1dJrsIXeOD3Sni.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/38xoJzrUrBC1dJrsIXeOD3Sni.js" id="asciicast-38xoJzrUrBC1dJrsIXeOD3Sni" async></script>