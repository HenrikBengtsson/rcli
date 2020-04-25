.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  check_as_functions("rcli-test" = check_rcli_test)
  
  check_as_functions("cran"          = check_cran_default)
  check_as_functions("cran::default" = check_cran_default)
  check_as_functions("cran::ci"      = check_cran_ci)

  check_as_functions("bioconductor"            = check_bioconductor_default)
  check_as_functions("bioconductor::default"   = check_bioconductor_default)
  check_as_functions("bioconductor::BiocCheck" = check_bioconductor_BiocCheck)
}
