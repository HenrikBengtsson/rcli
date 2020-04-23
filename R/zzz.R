.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  check_as_functions("rcli-test" = parse_check_rcli_test)
}
