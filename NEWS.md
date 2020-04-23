# Package: rcli

## Version: 0.0.0-9000 [2019-04-21]

### New Features

 * Add `r_cmd_call()`.

 * Add support for `R CMD build --renviron={name}`, which sets
   `R_BUILD_ENVIRON=~/.R/{name}.build.Renviron`.

 * Add support for `R CMD check --renviron={name}`, which sets
   `R_CHECK_ENVIRON=~/.R/{name}.check.Renviron`.

 * Package now supports `R CMD check --as={style}`. If `{style}` is not a
   built-in style, then the package **rcli.addon.{style}** is loaded if it
   is available, which should then register a corresponding "as" function.
   A built-in example is `R CMD check --as=rcli-test --help`.
