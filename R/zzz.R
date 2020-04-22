.onLoad <- function(libname, pkgname) {
  setHook(packageEvent("BiocCheck", "onLoad"), function(...) {
    ## Register 'R CMD check' flavors
    rcli:::check_flavors(BiocCheck = parse_check_BiocCheck)
  })
}
