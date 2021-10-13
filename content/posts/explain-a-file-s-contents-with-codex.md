+++
title = "Explain a file's contents with Codex"
author = ["Shane Mulligan"]
date = 2021-10-13T00:00:00+13:00
keywords = ["openai", "pen", "code"]
draft = false
+++

## Summary {#summary}

This prompt explains a file's contents by imagining a readme for that file.

It takes into account the file's filesystem context.


## Prompt {#prompt}

-   <http://github.com/semiosis/prompts/blob/master/prompts/explain-a-file-4.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "explain a file"
doc: "Explain a file"
prompt-version: 1
prompt: |+
  $ ls | head -n 10
  <ls here output>

  $ ls ..
  <ls parent dir output>

  $ cat <q:path> <<EOD
  <contents>
  EOD

  $ cat "README for <path>.md" <<EOD
  <:pp># <q:path>
  ## Explanation of code
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: 300
top-p: 1.0
stop-sequences:
- EOD
cache: on
vars:
- "contents"
- "path"
- "ls here output"
- "ls parent dir output"
var-defaults:
- "(buffer-string)"
- "(f-basename (get-path))"
- "(pen-snc \"ls | head -n 10\")"
- "(pen-snc \"ls .. | head -n 10\")"
examples:
- |-
    // SPDX-License-Identifier: MIT
    pragma solidity >= 0.4.22 < 0.8.5;

    contract Migrations {
      address public owner = msg.sender;
      uint public last_completed_migration;

      modifier restricted() {
        require(msg.sender == owner,
                "This function is restricted to the contract's owner");
        _;
      }

      function setCompleted(uint completed) public restricted {
        last_completed_migration = completed;
      }
    }
- Migrations.sol
- Migrations.sol
- |-
    box-img-lg.png
    box-img-sm.png
    bs-config.json
    contracts
    LICENSE
    migrations
    node_modules
    package.json
    package-lock.json
    README.org
    src
    tags
    test
    truffle-config.js
info: on
completion: off
insertion: off
{{< /highlight >}}


## Demo {#demo}


### Entire prompt {#entire-prompt}

{{< highlight text "linenos=table, linenostart=1" >}}
$ ls | head -n 10
Migrations.sol

$ ls ..
box-img-lg.png
box-img-sm.png
bs-config.json
contracts
LICENSE
migrations
node_modules
package.json
package-lock.json
README.org

$ cat "Migrations.sol" <<EOD
// SPDX-License-Identifier: MIT
pragma solidity >= 0.4.22 < 0.8.5;

contract Migrations {
  address public owner = msg.sender;
  uint public last_completed_migration;

  modifier restricted() {
    require(msg.sender == owner,
            "This function is restricted to the contract's owner");
    _;
  }

  function setCompleted(uint completed) public restricted {
    last_completed_migration = completed;
  }
}
EOD

$ cat "README for Migrations.sol.md" <<EOD
# "Migrations.sol"
## Explanation of code<END>
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/7pfbLHE5prdI988udMzEebNZc" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/7pfbLHE5prdI988udMzEebNZc.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/7pfbLHE5prdI988udMzEebNZc.js" id="asciicast-7pfbLHE5prdI988udMzEebNZc" async></script>