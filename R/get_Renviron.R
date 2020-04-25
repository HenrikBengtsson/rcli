#' @importFrom utils file_test
get_Renviron <- function(what = c("build", "check"), set, package = .packageName) {
  what <- match.arg(what)
  error_if_not(is.character(set), length(set) == 1L, !is.na(set))
  
  path <- system.file(package = package, set)
  if (!file_test("-d", path)) return(NULL)
  
  if (what == "build") {
    env <- "R_BUILD_ENVIRON"
  } else if (what == "check") {
    env <- "R_CHECK_ENVIRON"
  }

  value <- Sys.getenv(env, NA_character_)
  if (!is.na(value)) {
    logf("- Already set R_CHECK_ENVIRON=%s", value)
    return(value)
  }

  filename <- sprintf("%s.Renviron", what)

  pathname <- file.path(path, filename)
  logf("- Look for R_CHECK_ENVIRON file: %s", sQuote(pathname))
  if (!file_test("-f", pathname)) return(NULL)

  logf("- R_CHECK_ENVIRON file found: %s", sQuote(pathname))

  pathname
}
