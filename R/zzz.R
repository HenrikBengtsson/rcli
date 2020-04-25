.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  register_as("rcli-test" = check_rcli_test)
  
  register_as("cran"          = check_cran_default)
  register_as("cran::default" = check_cran_default)
  register_as("cran::ci"      = check_cran_ci)

  register_as("bioconductor"            = check_bioconductor_default)
  register_as("bioconductor::default"   = check_bioconductor_default)
  register_as("bioconductor::BiocCheck" = check_bioconductor_BiocCheck)
}
