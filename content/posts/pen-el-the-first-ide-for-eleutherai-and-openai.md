+++
title = "Pen.el the first IDE for both EleutherAI and OpenAI"
author = ["Shane Mulligan"]
date = 2021-08-03T00:00:00+12:00
keywords = ["gpt", "pen", "openai", "eleutherai", "emacs"]
draft = false
+++

## Summary {#summary}

I add support for EleutherAI's GPT-j model via
<https://apps.aixsolutionsgroup.com/>.

Get yourself a key from the link above if you'd like to try it out.

To try this out, firstly follow the setup instructions here:

-   <https://mullikine.github.io/posts/pen-tutorial/>

To update / remove the keys at any time, use
the following key bindings.

| kb    | f                    |                              |
|-------|----------------------|------------------------------|
| `M-o` | `pen-add-key-openai` | `pen-acolyte-minor-mode-map` |
| `M-a` | `pen-add-key-aix`    | `pen-acolyte-minor-mode-map` |

If you remove one of the keys then Pen will
attempt to use the other engine.

Prompts may still prefer a given engine.

Learn to write prompt description files here:

-   <https://github.com/semiosis/prompts/>

`Pen.el` supports arbitrary backends and takes
care of prompt engine preferences and your own
preferences (local(private), libre and commercial).