# Many Lox

This repo is a collection of my implementations of the Lox interpreter from the book [Crafting Interpreters](https://craftinginterpreters.com/), _by Bob Nystrom_.

I don't know if I will complete any of the implementations, but I want to attempt to implement it in as many languages as possible to be able to compare the languages across each step of the process, and with the hopes that it makes it easier for anyone looking to build their own interpreter to have a point of reference for starting.

| Legend            |     |
| ----------------- | --- |
| In Progress       | 🚧  |
| Complete          | ✅  |
| Stuck / Need Help | ⚠️  |

### Tree-Walk Interpreter

| Phase / Chapter        | Elm ([elm-lox](./elm-lox/))                    | Haskell ([haskell-lox](./haskell-lox)) |
| ---------------------- | ---------------------------------------------- | -------------------------------------- |
| Scanning               | ✅ https://www.youtube.com/watch?v=dA10oUBGk0A | 🚧                                     |
| Representing Code      |                                                |                                        |
| Parsing Expressions    |                                                |                                        |
| Evaluating Expressions |                                                |                                        |
| Statements and State   |                                                |                                        |
| Control Flow           |                                                |                                        |
| Functions              |                                                |                                        |
| Resolving and Binding  |                                                |                                        |
| Classes                |                                                |                                        |
| Inheritance            |                                                |                                        |

### Bytecode Virtual Machine

TBD

## Notes

When implementing the Scanner, use the `scanner_*.lox` files in the order:

1. `scanner_minimal.lox`
1. `scanner_with_operators.lox`
1. `scanner_with_whitespace.lox`
1. `scanner_with_comments.lox`
1. `scanner_with_strings.lox`
1. `scanner_with_numbers.lox`
1. `scanner_with_identifiers.lox`
