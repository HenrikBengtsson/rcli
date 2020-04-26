.onLoad <- function(libname, pkgname) {
  ## Register 'R CMD check' flavors
  register_as("rcli" = check_rcli)
  
  register_as("cran"          = check_cran_default)
#  register_as("cran::default" = check_cran_default)
#  register_as("cran::offline" = check_cran_offline)
#  register_as("cran::newbie"  = check_cran_newbie)

  register_as("bioconductor"            = check_bioconductor_default)
  register_as("bioconductor::default"   = check_bioconductor_default)
  register_as("bioconductor::newbie"    = check_bioconductor_newbie)
  register_as("bioconductor::BiocCheck" = check_bioconductor_BiocCheck)
}
