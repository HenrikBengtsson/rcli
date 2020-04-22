.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  check_flavors(BiocCheck = parse_check_BiocCheck)
}
