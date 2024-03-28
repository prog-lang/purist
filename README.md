# `purist` - Pure Dev Suite 🧰

Pure is a

- Purely functional programming language that
- Transpiles to Node.js code (of the CommonJS dialect)
- Intended for backend developers who wish to create _type-safe_ and _elegant_
  server applications

We hope that Pure will be able to target the following use cases:

1. 🧩 **Plug & Play** - write a Pure module, transpile it, use its functions
   from within your regular Node.js modules/programs.
2. 🌩️ **Cloud Functions** - you can incorporate Pure modules into your existing
   codebase - _that's great!_ Why not use Pure to substitute some of your GCP or
   AWS Lambda endpoints?
3. 🚀 **Microservices** - well... If you can write a Pure Cloud Function... Why
   not a _Pure Microservice?_ After all, microservices are just cloud functions
   with some custom setup around them.

## Install ✳️

**NOTE:** This relies on the [`stack`][stack] build tool.

[stack]: https://docs.haskellstack.org/en/stable/

You may use the installation script:

```bash
source <(curl -s https://raw.githubusercontent.com/prog-lang/purist/main/install.sh)
```

Or execute these commands manually (the script does the same as the following
code snippet):

```bash
git clone git@github.com:prog-lang/purist.git
cd purist
stack install
```

## Uninstall ❌

During installation, `stack` builds an executable and copies it over into some
folder on your system where it thinks global executables should be located. That
folder is baked into `stack` and is usually something like `$HOME/.local/bin/`
on UNIX systems and probably something similar on Windows.

Therefore, uninstalling is extremely straightforward:

```bash
rm $(which purist)
```

## Usage Basics 👷‍♀️

The following command will transpile `someModule.pure` into `someModule.js`.

```bash
purist c < someModule.pure > someModule.js
```

**NOTE:** Executing `purist` with no arguments displays the _help_ message. Use
it to get acquainted with its capabilities.

## Develop 👨‍💻

🚧 **CAUTION: ACTIVE CONSTRUCTION SITE** 🚧

All great projects started somewhere and Pure isn't an exception to this rule.
We are working hard to give you a tool you can be excited about.

> Our vision is simple: we want backend development to feel like a game.

With a strict but flexible type system and a powerful set of dev tools, all you
have to do is use compiler hints to make the puzzle pieces fit together. If you
ever touched Elm or Haskell, you will know what I'm on about.

If you wish to help out, please reach out via my [personal email][email].

[email]: mailto:sharp.vik@gmail.com

## Research 👩‍🔬

- [Duet][duet] - a subset of Haskell aimed at aiding teachers teach Haskell
- [PureScript][ps] - a strongly-typed language that compiles to JavaScript
- [Typing Haskell in Haskell][thih] - a renovated version of the legendary paper
- [Typing Haskell in Haskell PDF][thih-pdf] - the original paper (PDF)
- [Warnings for pattern matching][warn] - pattern matching algorithms (PDF)

[duet]: https://github.com/chrisdone/duet
[ps]: https://github.com/purescript/purescript
[thih]: https://github.com/ocramz/thih
[thih-pdf]: https://github.com/ocramz/thih
[warn]: http://moscova.inria.fr/~maranget/papers/warn/warn.pdf
