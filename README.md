# Tsundoku

Track your books.

Tsundoku is used through the `doku` command, which takes a number of
subcommands like `add` and `list`. See the [manual][] for full instructions.

[manual]: https://github.com/isaacazuelos/tsundoku/blob/master/docs/manual.md

## Requirements and Installation

You can install `doku` by cloning the repo and `stack install`ing.

See the [manual][firstrun] for full details.

Installation and development is handled by [`stack`][stack].

[stack]: https://github.com/commercialhaskell/stack
[firstrun]: https://github.com/isaacazuelos/tsundoku/blob/master/docs/manual.md#first-run

I've built things on NixOS before, here's how.

```sh
# build a default.nix file based on the cabal manifest
nix-shell -p cabal2nix --run "cabal2nix ." > default.nix
nix-env -i -f release.nix
```

I'm still leanring nix, so this could be bad.

## License

It's [MIT][] Licensed. See the included `LICENSE` file.

[MIT]: https://opensource.org/licenses/MIT
