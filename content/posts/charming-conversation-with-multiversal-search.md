+++
title = "Charming conversation with multiversal search"
author = ["Shane Mulligan"]
date = 2021-11-23T00:00:00+13:00
keywords = ["openai", "pen"]
draft = false
+++

## Summary {#summary}

I add multiversal search to `Pen.el`, to
filter generated conversation candidates for
those which result in constructive/better
responses from the other party.

This way you can avoid stuff-ups in your
conversation.


### Textual semantic search {#textual-semantic-search}

prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/textual-semantic-search-2.prompt>

Textual semantic search is the starting point
for filtering text according to semantic predicates.

This also works on the command-line so you can
conveniently perform semantic filtering in
your day-to-day tasks.

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/mCgjNfkRHM7gEyI7c1dlSlu9g" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/mCgjNfkRHM7gEyI7c1dlSlu9g.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/mCgjNfkRHM7gEyI7c1dlSlu9g.js" id="asciicast-mCgjNfkRHM7gEyI7c1dlSlu9g" async></script>


### Multiversal search {#multiversal-search}

The next step is to generate possible
conversation candidates and filter them out
based on downstream responses.


## Next steps {#next-steps}


### Contextual vs semantic similarity {#contextual-vs-semantic-similarity}

{{< highlight bash "linenos=table, linenostart=1" >}}
echo '["That sounds interesting", "I would love to hear more about that", "Boring."]' |
    "penf" "-u" "pf-textual-semantic-search-filter/2" "what a bored person would say"
{{< /highlight >}}

```bash
Boring.
```

Unfortunately, this type of semantic search
works well for only contextual similarity. I
plan on improving semantic similarity though
by running the semantic search twice, given
both a query and a counter-query. Those two
searches yield different likelihoods, and so
I'll do something with that difference.