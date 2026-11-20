# chez-fmt

A formatter for Chez Scheme that changes only whitespace, styled by the project's `.chez-tools`.

```sh
make SCHEME=/path/to/chez-10.4.1/scheme deps build test
make SCHEME=/path/to/chez-10.4.1/scheme install PREFIX=~/.local
chez-fmt --help
```

It needs [chez-analysis](https://github.com/jaybonthius/chez-analysis) beside it, or at `ANALYSIS_DIR`.
