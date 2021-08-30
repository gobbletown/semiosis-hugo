+++
title = "Prompts to search for functions in Pen.el"
author = ["Shane Mulligan"]
date = 2021-08-30T00:00:00+12:00
keywords = ["codex", "openai", "pen", "gpt"]
draft = false
+++

## Summary {#summary}

Here are some prompts for generating functions and signatures from NL.
What functions do you need?
What are their signatures?


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/hHYVidJpuEhqBu8PLcc6UBH7E" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/hHYVidJpuEhqBu8PLcc6UBH7E.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/hHYVidJpuEhqBu8PLcc6UBH7E.js" id="asciicast-hHYVidJpuEhqBu8PLcc6UBH7E" async></script>


## Prompts {#prompts}

use case
: <http://github.com/semiosis/prompts/blob/master/prompts/find-a-function-given-a-use-case-2.prompt>

signature
: <http://github.com/semiosis/prompts/blob/master/prompts/get-the-signatures-for-a-list-of-functions-2.prompt>

`find-a-function-given-a-use-case-2.prompt`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Find a function given a use-case"
doc: "Given a language and a use-case, find a function for that use-case"
prompt-version: 1
prompt: |+
  This is a list of the names of standard functions for <use case> in <language>:
  -
engine: "OpenAI Codex"
temperature: 0.1
max-tokens: 50
top-p: 1.0
cache: on
n-collate: 1
n-completions: 3
stop-sequences:
- "A list"
- "\n\n"
vars:
- "language"
- "use case"
var-defaults:
- "(pen-detect-language-ask)"
examples:
- "haskell"
- "convert from int to double"
# delete the last
postprocessor: sed -e 's/^- //' -e '$d' | uniqnosort | grep -oP "^[a-zA-Z_-]+$"
# The 'end' split patterns are used because
# we want it to run 'after' teh postprocessor
end-split-patterns:
- "\n"
filter: no
completion: on
insertion: on
{{< /highlight >}}

`get-the-signatures-for-a-list-of-functions-2.prompt`

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "Get the signatures for a list of functions"
doc: "Given a list of functions, get their signatures"
prompt-version: 1
prompt: |+
  This is a table of standard functions and their full signatures (including argument types) in <language>:
  | Functions: | <function list>
  | Full signature: |
engine: "OpenAI Codex"
temperature: 0.1
top-p: 1.0
cache: on
n-collate: 1
n-completions: 3
stop-sequences:
- "\n"
vars:
- "language"
- "function list"
var-defaults:
- "(pen-detect-language-ask)"
preprocessors:
- cat
- pen-str join " | "
examples:
- "haskell"
- "fromIntegral\nfromRational\nfromReal\nfromRealFrac"
# delete the last
postprocessor: sed "s/ | /\\n/g" | sed "s/^\s*//"
# The 'end' split patterns are used because
# we want it to run 'after' teh postprocessor
filter: no
completion: on
insertion: on
{{< /highlight >}}


## Example {#example}


### Haskell {#haskell}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 haskell "$(pen pf-find-a-function-given-a-use-case/2 haskell "convert from int to double")"
{{< /highlight >}}

```bash
fromIntegral :: Integral a => a -> Integer
fromRational :: Rational a => a -> Integer
fromInteger :: Integer -> a
fromReal :: Real a => a -> a
fromFloating :: Floating a => a -> a
fromDouble :: Double -> a
fromRealFrac :: RealFrac a => a -> a
fromIntegralFloat :: IntegralFloat a => a -> a
fromFloatingFloat :: Floating a => a -> a
```


### Python {#python}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "Python" "$(pen -u pf-find-a-function-given-a-use-case/2 "Python" "convert int to double")"
{{< /highlight >}}

```bash
int(x)
float(x)
str(x)
bool(x)
```


### Typed Python {#typed-python}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-find-a-function-given-a-use-case/2 "Typed Python" "convert from int to double"
{{< /highlight >}}

```bash
int_to_float
int_to_double
int_to_long_double
int_to_float_complex
int_to_double_complex
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "Typed Python" "$(pen -u pf-find-a-function-given-a-use-case/2 "Typed Python" "convert from int to double")"
{{< /highlight >}}

```bash
int -> float
int -> double
int -> long double
int -> float complex
int -> double complex
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "Typed Python" "$(pen -u pf-find-a-function-given-a-use-case/2 "Typed Python" "convert from int to double")"
{{< /highlight >}}

```bash
int_to_float(int) -> float
int_to_double(int) -> double
int_to_long_double(int) -> long double
int_to_float_complex(int) -> float complex
int_to_double_complex(int) -> double complex
```


### TypeScript {#typescript}

To get TypeScript to work, I had to add '.' to the filter `grep -oP "^[a-z.A-Z_-]+$"` in `find-a-function-given-a-use-case-2.prompt`.

The particular query _"convert from int to
double"_ definitely has some kind of afinity
for coming up with math functions, but the prompt is generally fine, I think..

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "TypeScript" "$(pen -u pf-find-a-function-given-a-use-case/2 "TypeScript" "convert from int to double")"
{{< /highlight >}}

```bash
Math.abs(x: number): number
Math.acos(x: number): number
Math.asin(x: number): number
Math.atan(x: number): number
Math.atan2(y: number, x: number): number
Math.ceil(x: number): number
Math.cos(x: number): number
Math.exp(x: number): number
Math.floor(x: number): number
```

I'm still unsure about the suggested
functions. Well, I have fixed the 'Math' issue
for most languages.


### Julia {#julia}

Interestingly, Julia continually returned function names in backticks.

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "Julia" "$(pen -u pf-find-a-function-given-a-use-case/2 "Julia" "convert from int to double")"
{{< /highlight >}}

```bash
sqrt(x::Float64)
exp(x::Float64)
log(x::Float64)
floor(x::Float64)
ceil(x::Float64)
round(x::Float64)
trunc(x::Float64)
```

Not the best results for Julia, but it may be
a bad question for Julia because what I'm
asking for may not exist.


### C++ {#c-plus-plus}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 "C++" "$(pen pf-find-a-function-given-a-use-case/2 "C++" "convert from int to double")"
{{< /highlight >}}

```bash
int atoi(const char *str)
long atol(const char *str)
long long atoll(const char *str)
double strtod(const char *str, char **endptr)
float strtof(const char *str, char **endptr)
long double strtold(const char *str, char **endptr)
long int strtol(const char *str, char **endptr, int base)
long long int strtoll(const char *str, char **endptr, int base)
unsigned long strtoul(const char *str, char **endptr, int base)
unsigned long long strtoull(const char *str, char **endptr, int base)
```


### C {#c}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "standard C" "$(pen -u pf-find-a-function-given-a-use-case/2 "standard C" "convert from int to double")"
{{< /highlight >}}

```bash
int atoi(const char *nptr)
long atol(const char *nptr)
long long atoll(const char *nptr)
double strtod(const char *nptr, char **endptr)
float strtof(const char *nptr, char **endptr)
long double strtold(const char *nptr, char **endptr)
long int strtol(const char *nptr, char **endptr, int base)
long long int strtoll(const char *nptr, char **endptr, int base)
unsigned long int strtoul(const char *nptr, char **endptr, int base)
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "standard C" "$(pen -u pf-find-a-function-given-a-use-case/2 "standard C" "transform strings")"
{{< /highlight >}}

```bash
strcpy(char *dest, const char *src)
strcat(char *dest, const char *src)
strlen(const char *str)
strcmp(const char *str1, const char *str2)
strncpy(char *dest, const char *src, size_t n)
strncmp(const char *str1, const char *str2, size_t n)
strncat(char *dest, const char *src, size_t n)
strstr(const char *haystack, const char *needle)
strchr(const char *str, int c)
strrchr(const char *str, int c)
```


### C# {#c}

It is peculiar that results continue to come
from the `Math` pool of functions.

Ahh, that's better. I had to surround the use-case in quotes.

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-find-a-function-given-a-use-case/2 "C#" "convert from int to double"
{{< /highlight >}}

```bash
int.TryParse
double.Parse
double.TryParse
Convert.ToDouble
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "C#" "$(pen -u pf-find-a-function-given-a-use-case/2 "C#" "convert from int to double")"
{{< /highlight >}}

```bash
int TryParse(string s, out int result)
double Parse(string s)
double TryParse(string s, out double result)
object Convert.ToDouble(object value, IFormatProvider provider)
object Convert.ToInt32(object value, IFormatProvider provider)
object Convert.ToInt64(object value, IFormatProvider provider)
```


### PureScript {#purescript}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 PureScript "$(pen pf-find-a-function-given-a-use-case/2 PureScript "convert from int to double")"
{{< /highlight >}}

```bash
double :: Double -> Double
realToFrac :: Double -> Double
truncate :: Double -> Double
round :: Double -> Double
ceiling :: Double -> Double
floor :: Double -> Double
isInteger :: Double -> Bool
isRational :: Double -> Bool
isNaN :: Double -> Bool
isInfinite :: Double -> Bool
```


### AppleScript {#applescript}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 AppleScript "$(pen pf-find-a-function-given-a-use-case/2 AppleScript "convert from int to double")"
{{< /highlight >}}

```bash
intToFloat(int)
intToString(int)
intToUnsigned(int)
intToLong(int)
intToLongLong(int)
intToShort(int)
intToUnsignedLong(int)
```


### Ruby {#ruby}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 ruby "$(pen pf-find-a-function-given-a-use-case/2 ruby "convert from int to double")"
{{< /highlight >}}

```bash
Integer(x)
Float(x)
Rational(x)
Complex(x)
x.to_i
x.to_f
x.to_r
x.to_c
x.to_d
x.to_s
x.to_sym
x.to_str
x.to_int
x.to_num
```


## Java {#java}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 Java "$(pen -u pf-find-a-function-given-a-use-case/2 Java "convert int to double")"
{{< /highlight >}}

```bash
longValue(long)
floatValue(float)
doubleValue(double)
byteValue(byte)
shortValue(short)
charValue(char)
intValueExact(int)
longValueExact(long)
floatValueExact(float)
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 Java "$(pen pf-find-a-function-given-a-use-case/2 Java "transform a string")"
{{< /highlight >}}

```bash
String toUpperCase()
String trim()
String startsWith(String)
String endsWith(String)
String contains(String)
int indexOf(String)
int lastIndexOf(String)
String replace(String, String)
String[] split(String)
String substring(int) |
```


## Kotlin {#kotlin}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 Kotlin "$(pen -u pf-find-a-function-given-a-use-case/2 Kotlin "convert from int to double")"
{{< /highlight >}}

```bash
toFloat(x: Double)
toLong(x: Double)
toInt(x: Double)
toShort(x: Double)
toByte(x: Double)
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 Kotlin "$(pen -u pf-find-a-function-given-a-use-case/2 Kotlin "transform a string")"
{{< /highlight >}}

```bash
String.toLowerCase(String): String
String.capitalize(String): String
String.decapitalize(String): String
String.plus(String): String
String.plusAssign(String): Unit
String.Companion.valueOf(String): String
String.repeat(Int): String
String.trimMargin(String): String
String.trimIndent(): String
String.replace(Regex, String): String
```


## Hack {#hack}

Hack uses `2` inside of its function names for specifying conversion.

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 Hack "$(pen -u pf-find-a-function-given-a-use-case/2 Hack "convert from int to double")"
{{< /highlight >}}

```bash
int2float(int $x)
int2char(int $x)
int2string(int $x)
```

Hack also specified docstrings in the results lists sometimes.

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 Hack "$(pen -u pf-find-a-function-given-a-use-case/2 Hack "transform strings")"
{{< /highlight >}}

```bash
strcat(char *, char *)
strcmp(char *, char *)
strcpy(char *, char *)
```


## OCaml {#ocaml}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 OCaml "$(pen -u pf-find-a-function-given-a-use-case/2 OCaml "convert from int to double")"
{{< /highlight >}}

```bash
float_of_int (int) -> float
int_of_float (float) -> int
int_of_char (char) -> int
int_of_string (string) -> int
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen pf-get-the-signatures-for-a-list-of-functions/2 OCaml "$(pen pf-find-a-function-given-a-use-case/2 OCaml "transform strings")"
{{< /highlight >}}

```bash
String.concat : string -> string -> string
String.sub : string -> int -> string
String.substring : string -> int -> int -> string
String.of_char : char -> string
String.of_int : int -> string
String.of_float : float -> string
```


## F# {#f}

This took a while to divulge.

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "F#" "$(pen -u pf-find-a-function-given-a-use-case/2 "F#" "convert an int into a double")"
{{< /highlight >}}

```bash
System.Int32.ToDouble(System.Int32)
```


## Cryptographic functions {#cryptographic-functions}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-find-a-function-given-a-use-case/2 "haskell" "cryptography"
{{< /highlight >}}

```bash
crypto-md5
crypto-md5-unpack
crypto-sha1
crypto-sha1-unpack
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "haskell" "$(pen pf-find-a-function-given-a-use-case/2 "haskell" "cryptography")"
{{< /highlight >}}

```bash
crypto-md5 :: String -> String
crypto-md5-unpack :: String -> [Word8]
crypto-sha1 :: String -> String
crypto-sha1-unpack :: String -> [Word8]
```

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "haskell" "$(pen -u pf-find-a-function-given-a-use-case/2 "haskell" "hash functions")"
{{< /highlight >}}

```bash
hash :: String -> String -> String
hashWithSalt :: String -> String -> String -> String
hashWithSalt1 :: String -> String -> String -> String -> String
hashWithSalt2 :: String -> String -> String -> String -> String -> String
hashWithSalt3 :: String -> String -> String -> String -> String -> String -> String
hashWithSalt4 :: String -> String -> String -> String -> String -> String -> String -> String
hashWithSalt5 :: String -> String -> String -> String -> String -> String -> String -> String -> String
hashWithSalt6 :: String -> String -> String -> String -> String -> String -> String -> String -> String -> String
hashWithSalt7 :: String -> String -> String -> String -> String -> String -> String -> String -> String -> String -> String
```


## Concurrency {#concurrency}


### Haskell {#haskell}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "haskell" "$(pen -u pf-find-a-function-given-a-use-case/2 "haskell" "concurrency")"
{{< /highlight >}}

```bash
forkIO :: IO a -> IO a
forkOn :: Fork -> IO a -> IO a
threadDelay :: Int -> IO ()
threadId :: IO ThreadId
threadIO :: IO () -> IO ()
threadInterrupt :: ThreadId -> IO ()
threadKill :: ThreadId -> IO ()
threadWait :: ThreadId -> IO ()
```


### Erlang {#erlang}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "erlang" "$(pen -u pf-find-a-function-given-a-use-case/2 "erlang" "concurrency")"
{{< /highlight >}}

```bash
spawn(Fun, Args) -> Pid
spawn_link(Fun, Args) -> Pid
spawn_monitor(Fun, Args) -> Pid
spawn_opt(Fun, Args, Options) -> Pid
self() -> Pid
whereis(RegName) -> Pid
register(RegName, Pid)
unregister(RegName)
link(Pid)
unlink(Pid)
monitor_node(Node)
```


### Python {#python}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "python" "$(pen -u pf-find-a-function-given-a-use-case/2 "python" "concurrency")"
{{< /highlight >}}

```bash
threading.Thread(group=None, target=None, name=None, args=(), kwargs={}, *, daemon=None)
multiprocessing.Process(group=None, target=None, name=None, args=(), kwargs={}, *, daemon=None)
asyncio.create_task(coro, *, name=None)
gevent.spawn(func, *args, **kwargs)
twisted.internet.threads.deferToThread(func, *args, **kwargs)
tornado.concurrent.run_on_executor(executor, func, *args, **kwargs)
concurrent.futures.ThreadPoolExecutor(max_workers=None, thread_name_prefix='').submit(func, *args, **kwargs)
subprocess.Popen(args, bufsize=0, executable=None, stdin=None, stdout=None, stderr=None, preexec_fn=None, close_fds=False, shell=False, cwd=None, env=None, universal_newlines=False, startupinfo=None, creationflags=0, restore_signals=True, start_new_session=False, pass_fds=(), *, encoding=None, errors=None)
```


### JavaScript {#javascript}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "javascript" "$(pen -u pf-find-a-function-given-a-use-case/2 "javascript" "concurrency")"
{{< /highlight >}}

```bash
setTimeout(callback, delay)
setInterval(callback, delay)
clearTimeout(timeoutId)
clearInterval(intervalId)
setImmediate(callback)
process.nextTick(callback)
new MessageChannel()
new MessagePort()
new WebWorker()
new XMLHttpRequest()
```


### TypeScript {#typescript}

{{< highlight bash "linenos=table, linenostart=1" >}}
pen -u pf-get-the-signatures-for-a-list-of-functions/2 "typescript" "$(pen -u pf-find-a-function-given-a-use-case/2 "typescript" "concurrency")"
{{< /highlight >}}

```bash
async (fn: () => Promise<any>): Promise<any>
await (promise: Promise<any>): any
awaitAll (promises: Promise<any>[]): Promise<any[]>
awaitAny (promises: Promise<any>[]): Promise<any>
delay (ms: number): Promise<void>
fork (fn: () => Promise<any>): Promise<any>
join (promises: Promise<any>[]): Promise<any[]>
race (promises: Promise<any>[]): Promise<any>
select (cases: {| case<T>(promise: Promise<T>): Promise<T> |}): Promise<any>
spawn (fn: () => Promise<any>): Promise<any>
spawnAll (fns: () => Promise<any>[]): Promise<any[]>
spawnAny (fns: () => Promise<any>[]): Promise<any>
timeout (ms: number): Promise<void>
timeoutAll (ms: number): Promise<void>
```