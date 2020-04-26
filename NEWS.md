# Package: rcli

## Version: 0.1.0-9000 [2019-04-26]

### New Features

 * Add `R CMD check --as=cran --offline ...`.
 

## Version: 0.1.0 [2019-04-25]

### New Features

 * Add rcli::install(), rcli::uninstall(), and rcli::validate().
 
 * Add support for --config=<file.dcf>.
 
 * Add --as=bioconductor for testing according to Bioconductor.

 * Add --as=cran, which is equivalent to --as-cran.
 
 * Package now supports `R CMD check --as={style}`. If `{style}` is not a
   built-in style, then the package **rcli.addon.{style}** is loaded if it
   is available, which should then register a corresponding "as" function.
   A built-in example is `R CMD check --as=rcli-test --help`.

 * Add `r_cmd_call()`.

 * Add support for `R CMD build --renviron={name}`, which sets
   `R_BUILD_ENVIRON=~/.R/{name}.build.Renviron`.

 * Add support for `R CMD check --renviron={name}`, which sets
   `R_CHECK_ENVIRON=~/.R/{name}.check.Renviron`.
