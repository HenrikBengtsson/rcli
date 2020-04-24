.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  check_as_functions("rcli-test" = check_rcli_test)
  check_as_functions("cran" = check_cran)
}
