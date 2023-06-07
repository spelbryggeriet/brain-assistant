# Brain Assistant

Brain Assistant is a CLI tool intended for computing mathematical expressions
and being an encyclopedia for mathematical concepts.

## Installation

Install using cargo:

```sh
cargo install brain-assistant
```

## Usage

Pass an expression to the `brain` command to parse and reduce an expression:

```sh
$ brain 1+2+3
6
$ brain 1*2*3
6
$ brain (1+2)*3
9
```

Invoke the `brain` command without any arguments to start the graphical user
interface, with a REPL pane and an extra information pane. Exit the REPL by
pressing `Ctrl-C` or `Esc`.

## Tips and Tricks

If you are using zsh, parenthesises and multiplication might collide with the
glob pattern match settings. To prevent this, you can either quote the input
(e.g. `brain "(1+2)*3"`), or prefix the command with `noglob` (e.g 
`noglob brain (1+2)*3`). Even better, you can create an alias that always
disables glob pattern matching for `brain` and save it in your zsh profile: 

```zsh
echo "alias brain='noglob brain'" >> ~/.zshrc
```
