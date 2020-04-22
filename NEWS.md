# Package: rcliaddons

## Version: 0.0.0-9000 [2019-04-21]

### New Features

 * Add `r_cmd_call()`.

 * Add support for `R CMD build --renviron=<name>`, which sets
   `R_BUILD_ENVIRON=~/.R/<name>.build.Renviron`.

 * Add support for `R CMD check --renviron=<name>`, which sets
   `R_CHECK_ENVIRON=~/.R/<name>.check.Renviron`.

 * Add support for `R CMD check --flavor=BiocCheck <options> <tarball>`, which
   runs `BiocCheck::BiocCheck(<tarball>, <options>)`, e.g.
   `R CMD check --flavor=BiocCheck --help` and
   `R CMD check --flavor=BiocCheck --no-check-vignettes pkg_1.0.tar.gz`.
