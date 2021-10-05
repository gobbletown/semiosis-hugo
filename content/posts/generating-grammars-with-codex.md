+++
title = "Generating grammars with Codex"
author = ["Shane Mulligan"]
date = 2021-10-05T00:00:00+13:00
keywords = ["gpt", "codex", "openai"]
draft = false
+++

## Summary {#summary}

I generate formal language parsers with Codex.

Prompt
: <http://github.com/semiosis/prompts/blob/master/prompts/generate-a-grammar-from-a-description-of-the-language-2.prompt>


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/JeXqNrSF0VLeqyu8TH1dvukDS" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/JeXqNrSF0VLeqyu8TH1dvukDS.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/JeXqNrSF0VLeqyu8TH1dvukDS.js" id="asciicast-JeXqNrSF0VLeqyu8TH1dvukDS" async></script>


## Antlr {#antlr}

These grammars would need continuation.


### basic {#basic}

Example of a generation.

{{< highlight bnf "linenos=table, linenostart=1" >}}
grammar basic;

prog : program EOF

# lexer rule
options {
language = basic;
}


program : PROGRAM statement_list
;

statement_list : statement (',' statement)*
;

statement : (LIST|MAT|PRINT) ';'
| IF '(' condition ')' THEN statement ELSE statement
| FOR '(' init ';' condition ';' step ')' statement
| FOR '(' init ';' condition ')' statement
| FOR '(' init ')' statement
| FOR '(' ')' statement
| COLON '(' statement_list ')'
| '{' statement_list '}'
| WHILE '(' condition ')' statement
| ';'
| INPUT '(' INPUT_TYPE ')' statement
| INPUT_TYPE '=' statement
;

condition : expression (':=' expression)?
;

init : expression (',' expression)*
;

step : (expression (',' expression)*
| ';') ';'
;

INPUT_TYPE : 'STRING' | 'NUMBER'
;

expression : expression '+' expression
| expression '-' expression
| expression '*' expression
| expression '/' expression
{{< /highlight >}}


### json {#json}

{{< highlight bnf "linenos=table, linenostart=1" >}}
grammar json;

prog : program EOF

# lexer rule
options {
language = json;
}


program : array | object

array : '[' elements ']'

elements : value (',' value)*

value : STRING | NUMBER | object | array | true | false | null

object : '{' members '}'

members : pair (',' pair)*

pair : STRING ':' value

STRING : '"' ([^"])* '"'

NUMBER : (-)? ('0'..'9')+

true : 'true'

false : 'false'

null : 'null'
{{< /highlight >}}


## Racket {#racket}