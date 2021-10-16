+++
title = "Create external prompt commands with Pen.el, interactively"
author = ["Shane Mulligan"]
date = 2021-10-16T00:00:00+13:00
keywords = ["pen", "codex", "gpt"]
draft = false
+++

## Summary {#summary}

While interactively using `Pen.el`, commands
are recorded to be run later as standalone
applications.


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/O1DcVOa3i1wM6S7jS8XO5a51d" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/O1DcVOa3i1wM6S7jS8XO5a51d.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/O1DcVOa3i1wM6S7jS8XO5a51d.js" id="asciicast-O1DcVOa3i1wM6S7jS8XO5a51d" async></script>


### Explaining Haskell {#explaining-haskell}

{{< highlight haskell "linenos=table, linenostart=1" >}}
getResults :: String -> IO (Maybe [String])
getResults product = do
  bks <- listOf 10 "Books by Noam Chomsky"
  jbks <- listOf 10 "Books by Jean Baudrillard"
  l <- listOf 10 "Butterfly species"
  pickuplines <- pickUpLine product
  -- return ((fromMaybe [] (decodeResultsList l)) ++ (fromMaybe [] (decodeResultsList pickuplines)))
  return (decodeResultsList pickuplines)

main :: IO ()
main = do
  output <- getResults "Weather"
  print $ unlines $ fromMaybe [] output
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
We are using listOf 10 to create 10 lists of
10 items each.

We are using pickUpLine to pick up a random
line from a list of lines.

We are using decodeResultsList to convert the
list of strings to a list of Maybe strings.

We are using fromMaybe to convert the list of
Maybe strings to a list of strings.

We are using unlines to convert the list of
strings to a single string.

We are using print to print the string.
{{< /highlight >}}

{{< highlight haskell "linenos=table, linenostart=1" >}}
data List a = Nil | Cons a (List a)
    deriving Show
{{< /highlight >}}

{{< highlight text "linenos=table, linenostart=1" >}}
This is a list data type.

"Nil" is the empty list, and "Cons" is a
function that takes an element of type a and a
list of type a and produces a new list of type
a.
{{< /highlight >}}

The function is automatically recorded after
performing some interactive editing in
`Pen.el`.

The underlying editing functions are recorded
as standalone functions which you can use with
different parameters, or to extract the json
results.

{{< highlight bash "linenos=table, linenostart=1" >}}
"penf" "-u" "pf-explain-haskell-code/1" "data List a = Nil | Cons a (List a)\n    deriving Show"
{{< /highlight >}}

```bash
This is a list data type.

It is recursive.
```

{{< highlight bash "linenos=table, linenostart=1" >}}
"pena" "pf-explain-haskell-code/1" "data List a = Nil | Cons a (List a)\n    deriving Show" | jq .
{{< /highlight >}}

```bash
[
  "This is a list of type \"a\".\n\nWe have two constructors, \"Nil\" and \"Cons\".\n\n\"Nil\" represents the empty list and \"Cons\"\nrepresents a nonempty list.",
  "We have created a new type called List.\n\nIt is a recursive type, meaning that it can be\ndefined in terms of itself.\n\nThe type parameter \"a\" is a type variable.\n\nIt can be any type.",
  "This is a simple datatype called List that\nhas two constructors, \"Nil\" and \"Cons\".\n\n\"Nil\" represents the empty list and \"Cons\"\nrepresents a list with a head and a tail.",
  "We have created a new type called List.\n\nNext, we are using this type to calculate the\nsum of a list of numbers.\n\n\"sum\" is a function that takes List as an\ninput and produces Float as the output.\n\nKeep in mind that \"data\" is a keyword here and\nall user-defined types in Haskell always start\nwith a capital letter.",
  "We have created a new type called List.\n\nThis type can either be Nil (which means empty\nlist) or Cons (which means a list that has a\nvalue and a list).\n\n\"deriving Show\" is a function that tells\nHaskell that we want to be able to print out\nthe value of this type."
]
```