## Prerequisite

* nix

## Getting into an environment

```
$ nix-shell
```

Or, if you have direnv:

```
$ direnv allow
```

## Running tests

```
$ cabal v2-test --test-show-details=direct
```
