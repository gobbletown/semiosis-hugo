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


## Let's try on some `openzeppelin` contracts {#let-s-try-on-some-openzeppelin-contracts}


### `contracts › access ›  Ownable.sol` {#contracts-access-ownable-dot-sol}

Contract
: <https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/access/Ownable.sol>

<!--listend-->

{{< highlight solidity "linenos=table, linenostart=1" >}}
// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

import "../utils/Context.sol";

/**
 * @dev Contract module which provides a basic access control mechanism, where
 * there is an account (an owner) that can be granted exclusive access to
 * specific functions.
 *
 * By default, the owner account will be the one that deploys the contract. This
 * can later be changed with {transferOwnership}.
 *
 * This module is used through inheritance. It will make available the modifier
 * `onlyOwner`, which can be applied to your functions to restrict their use to
 * the owner.
 */
abstract contract Ownable is Context {
    address private _owner;

    event OwnershipTransferred(address indexed previousOwner, address indexed newOwner);

    /**
     * @dev Initializes the contract setting the deployer as the initial owner.
     */
    constructor() {
        _transferOwnership(_msgSender());
    }

    /**
     * @dev Returns the address of the current owner.
     */
    function owner() public view virtual returns (address) {
        return _owner;
    }

    /**
     * @dev Throws if called by any account other than the owner.
     */
    modifier onlyOwner() {
        require(owner() == _msgSender(), "Ownable: caller is not the owner");
        _;
    }

    /**
     * @dev Leaves the contract without owner. It will not be possible to call
     * `onlyOwner` functions anymore. Can only be called by the current owner.
     *
     * NOTE: Renouncing ownership will leave the contract without an owner,
     * thereby removing any functionality that is only available to the owner.
     */
    function renounceOwnership() public virtual onlyOwner {
        _transferOwnership(address(0));
    }

    /**
     * @dev Transfers ownership of the contract to a new account (`newOwner`).
     * Can only be called by the current owner.
     */
    function transferOwnership(address newOwner) public virtual onlyOwner {
        require(newOwner != address(0), "Ownable: new owner is the zero address");
        _transferOwnership(newOwner);
    }

    /**
     * @dev Transfers ownership of the contract to a new account (`newOwner`).
     * Internal function without access restriction.
     */
    function _transferOwnership(address newOwner) internal virtual {
        address oldOwner = _owner;
        _owner = newOwner;
        emit OwnershipTransferred(oldOwner, newOwner);
    }
}
{{< /highlight >}}


### Explanations {#explanations}

1

{{< highlight text "linenos=table, linenostart=1" >}}
We start by importing the Context contract
from the utils folder.

This contract will give us access to the
_msgSender() function, which will return the
address of the account that called the
function.

We then declare the abstract contract Ownable,
which will allow us to use the access
modifiers onlyOwner.

The constructor function is where we set the
owner of the contract to the account that
deployed it.

This is done by calling the
_transferOwnership() function, which is an
internal function that will change the owner
of the contract.

The onlyOwner modifier will make sure that
only the owner of the contract can call the
function.

The renounceOwnership function will make the
contract ownerless.

The transferOwnership function will change the
owner of the contract to a new account.
{{< /highlight >}}

2

{{< highlight text "linenos=table, linenostart=1" >}}
We start by importing the Context smart
contract.

This is a library that we'll use to make sure
that the owner of the contract is the one who
is calling it.

We can do this by using the _msgSender()
function.

Next, we declare the contract as an abstract
contract.

This means that it will be an abstract class
that cannot be instantiated.

We do this with the "abstract" keyword.

Then, we declare the constructor function.

This is the only function that will be called
when the contract is instantiated.

We use the constructor keyword to declare it.

Next, we declare the owner state variable.

This is the only state variable that will be
stored on the blockchain.

It will store the address of the owner of the
contract.

Then, we declare the event that will be called
when the owner is changed.

This is a function that is called when an
event is emitted.

Finally, we declare the onlyOwner modifier.

This is a modifier that is used to restrict
the use of the function to the owner of the
contract.
{{< /highlight >}}

3

{{< highlight text "linenos=table, linenostart=1" >}}
We start by importing the Context smart
contract.

This is a library of code that we'll be using
in our own smart contract.

We'll be using it to restrict access to the
contract to only the owner.

Next, we declare the abstract contract that
we'll be inheriting from.

This is a contract that we'll be using as a
base for our own smart contract.

We'll be using it to restrict access to the
contract to only the owner.

The constructor function is where we'll set
the owner of the contract.

We'll be using the _transferOwnership function
to set the owner of the contract.

This function is an internal function that is
used by the contract to change the owner of
the contract.

We'll see how to use it later in the console.

The onlyOwner modifier is a modifier that
restricts access to the function to only the
owner of the contract.

We'll use this modifier in our functions to
restrict access to the contract to only the
owner.

The renounceOwnership function is used to
remove the owner of the contract.

It can only be called by the owner of the
contract.

The transferOwnership function is used to
change the owner of the contract.

It can only be called by the owner of the
contract.
{{< /highlight >}}

4

{{< highlight text "linenos=table, linenostart=1" >}}
This is the same code as the first example,
except that it is abstract.

This means that it will not be directly
deployed to the blockchain.

Instead, it will be used as a base class to
create a new smart contract.

This is useful for creating contracts that are
based on a common base contract.

We start by declaring the solidity version
with the pragma solidity statement.

Next, we declare the smart contract with the
"contract" keyword, followed by the contract
name.

Next, we declare a state variable that will
store the value of the candidate name.

State variables allow us to write data to the
blockchain.

We have declared that this variable will be a
string, and we have set its visibility to
public.

Because it is public, solidity will give us a
getter function for free that will allow us to
access this value outside of our contract.

We'll see that in action later in the console!
Then, we create a constructor function that
will get called whenever we deploy the smart
contract to the blockchain.

This is where we'll set the value of the
candidate state variable that will get stored
to the blockchain upon migration.

Notice that the constructor function has the
same name as the smart contract.

This is how Solidity knows that the function
is a constructor.

We also declare an event called
OwnershipTransferred.

This event will be emitted whenever ownership
of the contract changes.

This is useful for tracking changes in
ownership.

Next, we declare a modifier that will restrict
access to the contract's functions.

This modifier is called onlyOwner.

It will make sure that only the owner of the
contract can call the function.

Finally, we declare a function called owner.

This function will return the address of the
owner of the contract.

Then, we declare a function called onlyOwner.

This function will throw an exception if it is
called by anyone other than the owner of the
contract.

Next, we declare a function called
renounceOwnership.

This function will leave the contract without
an owner.

It will not be possible to call onlyOwner
functions anymore.

Can only be called by the current owner.

Finally, we declare a function called
transferOwnership.

This function will transfer the ownership of
the contract to a new account.

Can only be called by the current owner.
{{< /highlight >}}

5

{{< highlight text "linenos=table, linenostart=1" >}}
We start by declaring the solidity version
with the pragma solidity statement.

Next, we import the Context contract, which is
a smart contract that we'll use to store the
owner of the contract.

This is a good practice to follow, as it will
make it easier to migrate to newer versions of
solidity.

Then, we declare the abstract Ownable
contract, which is a smart contract that we'll
use to restrict access to functions.

It will also give us a constructor function,
which will allow us to set the owner of the
contract.

The constructor function will set the owner of
the contract to the address of the account
that deploys the contract.

The onlyOwner modifier will restrict the
access to the function to the owner of the
contract.

The renounceOwnership function will make the
contract ownerless.

The transferOwnership function will transfer
the ownership of the contract to a new
account.
{{< /highlight >}}


### Demo {#demo}

<!-- Play on asciinema.com -->
<!-- <a title="asciinema recording" href="https://asciinema.org/a/Vazur9S01MeMW3EGr7E23sOzR" target="_blank"><img alt="asciinema recording" src="https://asciinema.org/a/Vazur9S01MeMW3EGr7E23sOzR.svg" /></a> -->
<!-- Play on the blog -->
<script src="https://asciinema.org/a/Vazur9S01MeMW3EGr7E23sOzR.js" id="asciicast-Vazur9S01MeMW3EGr7E23sOzR" async></script>