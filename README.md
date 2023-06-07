# Brain Assistant

[![Build Status](https://github.com/spelbryggeriet/brain-assistant/workflows/CI/badge.svg)](https://github.com/spelbryggeriet/brain-assistant/actions?query=workflow:CI)
[![Crate Status](https://img.shields.io/crates/v/brain-assistant.svg)](https://crates.io/crates/brain-assistant)

Brain Assistant is a CLI tool intended for computing mathematical expressions
and being an encyclopedia for mathematical concepts.

## Installation

Install using cargo:

```sh
cargo install brain-assistant
```

## Usage

Invoke the `brain` command without any arguments to start the graphical user
interface, which contains a REPL pane and an information pane. You can write
any number of expressions and get the reduced expression as well as extra
info regarding the calculation. Exit the program by pressing `Ctrl-C` or
`Esc`:

[![asciicast](https://asciinema.org/a/MgDzdoPdN7JqwfSZpsV8Uyydu.svg)](https://asciinema.org/a/MgDzdoPdN7JqwfSZpsV8Uyydu?autoplay=1&loop=1)

You can also use it in single line mode by passing an expression as an
argument to the `brain` command, in which case it will parse and reduce the
input as an expression:

```sh
❯ brain 1+2+3
6
❯ brain 1*2*3
6
❯ brain (1+2)*3
9
```

Sometimes an expression can be misinterpreted as a flag argument, in which
case you will need to use `--` to separate flags from input:

```sh
❯ brain -1+1
error: unexpected argument '-1' found

  tip: to pass '-1' as a value, use '-- -1'

Usage: brain [expression]...

For more information, try '--help'.
❯ brain -- -1+1
0
```

## Tips and Tricks

If you are using zsh, parenthesises and multiplication might collide with the
glob pattern match settings. To prevent this, you can either quote the input
(e.g. `brain "(1+2)*3"`), or prefix the command with `noglob` (e.g 
`noglob brain (1+2)*3`). Even better, you can create an alias that always
disables glob pattern matching for `brain` and save it in your zsh profile: 

```zsh
echo "alias brain='noglob brain'" >> ~/.zshrc
```
