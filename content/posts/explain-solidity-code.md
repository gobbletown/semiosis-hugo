+++
title = "Explain solidity code"
author = ["Shane Mulligan"]
date = 2021-10-14T00:00:00+13:00
keywords = ["codex", "pen", "emacs", "ethereum"]
draft = false
+++

## Summary {#summary}

I make a solidity code explainer.


## Example {#example}


### Contract {#contract}

{{< highlight text "linenos=table, linenostart=1" >}}
// SPDX-License-Identifier: MIT
pragma solidity >= 0.4.22 < 0.8.5;

contract Migrations {
  address public owner = msg.sender;
  uint public last_completed_migration[<0;29;7M;

  modifier restricted() {
    require(msg.sender == owner,
            "This function is restricted to the contract's owner");
    _;
  }

  function setCompleted(uint completed) public restricted {
    last_completed_migration = completed;
  }
}
{{< /highlight >}}


### Explanation {#explanation}

{{< highlight text "linenos=table, linenostart=1" >}}
We declare a new contract called Migrations.

This contract will be an abstract contract
that we'll use to store the version number of
our smart contracts.

We'll use this contract to keep track of which
version of our smart contracts we've deployed
to the blockchain.

We declare a state variable called
last_completed_migration that will store the
version number of the smart contract that
we've deployed to the blockchain.

We also declare a modifier that will restrict
access to the setCompleted function to the
owner of the contract.

This is a way to prevent other people from
modifying the state variable.

We create a function called setCompleted that
will set the value of the state variable
last_completed_migration to the value passed
in as an argument.

We restrict access to this function by using
the modifier restricted.
{{< /highlight >}}


## Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/XwY4kQDEngYsiyhhl4Disohs4" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/XwY4kQDEngYsiyhhl4Disohs4.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/XwY4kQDEngYsiyhhl4Disohs4.js" id="asciicast-XwY4kQDEngYsiyhhl4Disohs4" async></script>


## Prompt {#prompt}

`pf-explain-solidity-code/1`
: <http://github.com/semiosis/prompts/blob/master/prompts/explain-solidity-code-1.prompt>

<!--listend-->

{{< highlight yaml "linenos=table, linenostart=1" >}}
task: "explain solidity code"
doc: "Given some solidity code, explain it"
prompt-version: 1
prompt: |+
    Solidity explanations.

    <delim>Code example 1
    pragma solidity 0.4.2;

    contract Election {
        // Read/write candidate
        string public candidate;

        // Constructor
        function Election () public {
            candidate = "Candidate 1";
        }
    }
    <delim>

    <delim>Explanation of 1
    Let me explain the above code. We start by declaring the solidity version with the pragma solidity statement. Next, we declare the smart contract with the "contract" keyword, followed by the contract name. Next, we declare a state variable that will store the value of the candidate name. State variables allow us to write data to the blockchain. We have declared that this variable will be a string, and we have set its visibility to public.  Because it is public, solidity will give us a getter function for free that will allow us to access this value outside of our contract. We'll see that in action later in the console!

    Then, we create a constructor function that will get called whenever we deploy the smart contract to the blockchain. This is where we'll set the value of the candidate state variable that will get stored to the blockchain upon migration. Notice that the constructor function has the same name as the smart contract. This is how Solidity knows that the function is a constructor.
    <delim>

    Here is some new code:

    <delim>Code example 2
    <solidity code>
    <delim>

    <delim>Explanation of 2
    Let me also explain this code.
engine: "OpenAI Codex"
temperature: 0.3
max-generated-tokens: prompt-length
top-p: 1.0
stop-sequences:
- "<delim>"
cache: on
vars:
- "solidity code"
examples:
- |+
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
info: on
postprocessor: chomp | pen-str remove-starting-and-trailing-whitespace | pen-pretty-paragraph
external-related: "https://www.dappuniversity.com/articles/the-ultimate-ethereum-dapp-tutorial/"
{{< /highlight >}}