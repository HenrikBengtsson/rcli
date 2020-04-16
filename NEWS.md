# Package: rcliaddons

## Version: 0.0.0-9000 [2019-04-15]

### New Features

 * Add `r_cmd_call()` which provides `R CMD build --renviron=<name>`, which
   sets `R_BUILD_ENVIRON=~/.R/<name>.build.Renviron`. Analoguously,
   `R CMD check --renviron=<name>` sets
   `R_CHECKS_ENVIRON=~/.R/<name>.build.Renviron`.
