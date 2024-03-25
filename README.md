# purist

The development suite for the Pure programming language.

## Install

**NOTE:** This relies on the `stack` build tool.

```bash
git clone git@github.com:prog-lang/purist.git
cd purist
stack install
```

## Usage Basics

**NOTE:** Executing `purist` with no arguments displays the _help_ message.

```bash
purist c < someModule.pure > someModule.js
```

## Research

- [Duet][duet] - a subset of Haskell aimed at aiding teachers teach Haskell
- [PureScript][ps] - a strongly-typed Haskell-esque language that compiles to JavaScript
- [Typing Haskell in Haskell][thih] - a renovated version of the legendary paper
- [Typing Haskell in Haskell PDF][thih-pdf] - PDF of the original paper

[duet]: https://github.com/chrisdone/duet
[ps]: https://github.com/purescript/purescript
[thih]: https://github.com/ocramz/thih
[thih-pdf]: https://github.com/ocramz/thih
