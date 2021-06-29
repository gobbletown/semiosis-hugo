+++
title = "Representing multiverses with Datomic"
author = ["Shane Mulligan"]
date = 2021-06-26T00:00:00+12:00
keywords = ["gpt", "clojure"]
draft = false
+++

## Related articles {#related-articles}

-   [Language models are multiverse generators `::`  Moire](https://generative.ink/posts/language-models-are-multiverse-generators/)
    -   `|:ϝ∷¦ϝ`'s blog post on exploring language models.
-   [Datomic with Rich Hickey - YouTube](https://www.youtube.com/watch?v=9TYfcyvSpEQ)
-   [Writing Datomic in Clojure - Rich Hickey - YouTube](https://www.youtube.com/watch?v=7Fi-UvrRpyI)
-   [richhickey.md GitHub](https://gist.github.com/prakhar1989/1b0a2c9849b2e1e912fb)
    -   Rich Hickey on becoming a better developer
-   Notes: <https://github.com/semiosis/code-org-tidbits/blob/master/datomic/basic-query.org>


## Summary {#summary}

After reading `|:ϝ∷¦ϝ`'s blog article
[LMs are multiverse generators `::` Moire](https://generative.ink/posts/language-%20models-are-multiverse-generators/), I
decided to take a closer look into Datomic as
a possible store for LM generations.

Something which caught my attention which Rich
Hickey mentioned was the need for immutability
and to **not** throw away data, in order to make progress.

`|:ϝ∷¦ϝ` also mentioned this when we talked
about keeping our notes around and one day
fine-tuning LMs on them.

Since we are now using LM's to facilitate our
creativity, we need a rock solid and
distributed foundation for retaining memories.
Datomic is a great candidate for such a
database.

-   It's a time-travelling database
    -   <https://stackoverflow.com/questions/49309263/how-to-achieve-time-travel-with-clojure/49310124>
-   An entire database is considered a value.
-   Transactions are broken down into functions.
-   A local copy is kept inside the application.
-   Datomic has a lisp domain specific logic-oriented language embedded for doing querying.
    -   [Generate graphviz and prolog from org-brain // Bodacious Blog](https://mullikine.github.io/posts/generate-graphviz-and-prolog-from-org-brain/)
        -   Datomic is a semantic subset of prolog.
        -   It's a lisp, so is naturally multidimensional in structure.

{{< figure src="/ox-hugo/datomic-memory-engine.png" >}}


## [Datomic with Rich Hickey - YouTube](https://www.youtube.com/watch?v=9TYfcyvSpEQ) {#datomic-with-rich-hickey-youtube}

-   Datomic can harness storage from many different storage and database services
    -   Even relational databases
-   Has pluggable comparators
    -   Two sorts that are always maintained
        -   Primary by entity
        -   Primary by attribute
-   Has both the characteristics of a document store and a column store
-   assert and retract are insufficient
    -   So we have tx-fn (transaction functions)
-   stable basis
    -   same query (including time), same result
-   transactions well defined
    -   A transaction is a function to a database
    -   functional accretion
-   Embedded Datalog
    -   [Datomic with Rich Hickey - YouTube](https://youtu.be/9TYfcyvSpEQ)
    -   Subset of Prolog.
    -   guaranteed termination.
    -   Set oriented.
        Has set orientation, unlike prolog which
        is tuple at a time.
    -   A datalog program contains
        -   db(s) + rules + queries
            -   one or more databases,
            -   a set of rules which you can map to views
            -   a set of queries
    -   Completely data-driven interface to datalog
    -   has the power of relational algebra
    -   It's embedded in clojure
        -   syntactically subset of Clojure
        -   semantically subset of prolog


### Example datalog code {#example-datalog-code}

-   [Datomic with Rich Hickey - YouTube](https://youtu.be/9TYfcyvSpEQ)

Can read this as a map where each key maps to a list.

{{< highlight clojure "linenos=table, linenostart=1" >}}
{:find [?customer ?product]
 :where [[?customer :shipAddress ?addr]
         [?addr :zip ?zip]
         [?product :product/weight ?weight]

         [?product :product/price ?price]
         [(Shipping/estimate ?zip ?weight) ?shipCost]
         [(<= ?price ?shipCost)]]}
{{< /highlight >}}

No joins -- all joins are implicit.

Unification unifies all the values of customer
to be the same customer as the product ~~...

{{< highlight text "linenos=table, linenostart=1" >}}
unification
    [#prolog]
    [operator]

    Essentially a combination of assignment
    and equality.

    vimlinks +/"?- 2 = 3." "https://learnxinyminutes.com/docs/prolog/"

    If both sides are bound (ie, defined),
    check equality.
    2 = 3

    IMPORTANT (the essence of unification):
    If one side is free (ie, undefined),
    assign to match the other side.
    X = 3

    If both sides are free, the assignment is
    remembered. With some luck, one of the two
    sides will eventually be bound, but this
    isn't necessary.
    X = Y are both free, so Prolog remembers
    it. Therefore assigning X will also assign
    Y. IS is another assigment operator in prolog.
    X = Y, X = 2, Z is Y + 3.

    See:
        vim +/"modus ponens" "$NOTES/ws/logic/glossary.txt"
{{< /highlight >}}


## [Writing Datomic in Clojure - Rich Hickey - YouTube](https://www.youtube.com/watch?v=7Fi-UvrRpyI) {#writing-datomic-in-clojure-rich-hickey-youtube}

Database is distributed.

Brings power from database (where
traditionally it would be found), to the
application logic.

-   The application gets its own brain (datomic database copy)
    -   The app becomes a peer
    -   The db is effectively local


### datalog {#datalog}

-   The big advantage over `core.logic` or `prolog`
    -   The semantics of those are tuple at a time
    -   The semantics of datalog is set at a time
        -   This means that underneath the hood, entire sets are being merge-joined


#### Peer implementation {#peer-implementation}

[Writing Datomic in Clojure - Rich Hickey - YouTube](https://youtu.be/7Fi-UvrRpyI)


#### Consistency and Scale {#consistency-and-scale}


#### Testing {#testing}

-   Functional tests
-   Simulation-based testing
    -   I like.


## Querying Datomic {#querying-datomic}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/4EMtFoULbap3OtMKg9HqFojf5" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/4EMtFoULbap3OtMKg9HqFojf5.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/4EMtFoULbap3OtMKg9HqFojf5.js" id="asciicast-4EMtFoULbap3OtMKg9HqFojf5" async></script>


## Learning with Datomic {#learning-with-datomic}

In the markdown file linked to above and in
his videos about Datomic Rich Hickey tells us
that tells us that mastery comes from always
working slightly beyond your comfort/ability
zone, pushing it ever forward.

He also designed Datomic to efficiently
accrete and distribute knowledge, whilst
remaining queryable. It allows you to both
offload logic to the database and run logic
locally which traditionally would've been
conducted remotely on the database.


## Additional reading {#additional-reading}


### [DOMAIN MODELING WITH DATALOG by Norbert Wojtowicz - YouTube](https://www.youtube.com/watch?v=oo-7mN9WXTw) {#domain-modeling-with-datalog-by-norbert-wojtowicz-youtube}

{{< figure src="/ox-hugo/datomic-stream.png" >}}

Complete notes
: <https://github.com/semiosis/code-org-tidbits/blob/master/datomic/domain-modeling-with-datalog.org>

Example: github.
New users.
There are 3 users here in our system.
3 ids means they are different things.
Each row is a `datum`.

| entity | attribute    | value      |
|--------|--------------|------------|
| 11     | `:user/name` | richhickey |
| 22     | `:user/name` | tonsky     |
| 33     | `:user/name` | pithyless  |

We use pattern-matching for querying (Datomic).

A pattern in this case will be a vector of 3
elements that represents the EAV structure in
our database.

Underscore means I don't care (wildcard).

`[11 _ _]`

Variables start with a question mark and some
name i.e. `?entity`.


## Annex {#annex}

I want to mention this excerpt about the
origins of surreal numbers because I think
there is some allegory here.


### Surreal numbers {#surreal-numbers}

<https://ianopolous.peergos.me/maths/surreal>

{{< highlight text "linenos=table, linenostart=1" >}}
In 1972, Conway described his system of
numbers to computer scientist Donald Knuth at
Stanford.

Knuth (creator of the $\TeX$ typesetting
system) then went away and wrote a short
novelette introducing these numbers.

It was the first time for a major mathematical
discovery to be published in a work of fiction
first.

Knuth coined the term surreal numbers; taking
“Sur” from the French for “above”.

The surreal numbers satisfy the axioms for a
field (but the question of whether or not they
constitute a field is complicated by the fact
that, collectively, they are too large to form
a set).
{{< /highlight >}}