# What is this?

This is not a shell, as you might think.
It does not have the underlying framework for a shell.
There is basically this pure function (side effect free ⇒ may not execute or
even print anything) that gets passed the input and returns the output using
[`interact`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#v:interact).

Hence there may not be any interaction with anything outside memory.

This is an upside.
You can use this program and plug it into your favourite SSH server and grant
people access to computing resources or memory storage.

Yeah, there may not be much usage for the program as is, but you can adapt it.
You can pass some resources as long as they do not need any monadic operation
to operate.
You could pass in some lazily loaded weather data to build an SSH-interface for
interactive query.

# Why would this be useful?

You remember [*nethack*](http://www.nethack.org/)?
There's a [telnet and SSH version](https://alt.org/nethack/).

Now you can build your own CLI-like game.
Just give me some time to work on the whole command-thing and it'll turn out
okay I guess.

# Usage

Please download/install
[stack](https://docs.haskellstack.org/en/stable/README/).
Then you can clone this repository and compile the program as follows:

```bash
# your favourite way of cloning
git clone git@github.com:benaryorg/haskell-cli.git
# go to the directory
cd haskell-cli
# build
stack build
# execute
stack exec haskell-cli
```

