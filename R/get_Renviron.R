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
  if (!is.na(value)) return(value)

  filename <- sprintf("%s.Renviron", what)

  pathname <- file.path(path, filename)
  if (!file_test("-f", pathname)) return(NULL)

  pathname
}
