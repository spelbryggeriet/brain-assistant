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
```

Invoke the `brain` command without any arguments to start the REPL:

```sh
$ brain
>> 1+1
2
>> 1/2*5
5/2
>> 2*x^2+3*x^2
5*x^2
```

Exit the REPL by pressing `Ctrl-C`.
