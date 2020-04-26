#' @importFrom utils file_test
find_Renviron <- function(what = c("build", "check"), set, package = .packageName, must_exist = FALSE) {
  what <- match.arg(what)
  error_if_not(is.character(set), length(set) == 1L, !is.na(set))

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

  root_paths <- c(
    file.path("~", ".R"),
    file.path(system.file(package = package))
  )

  paths <- file.path(root_paths, set)
  keep <- file_test("-d", paths)
  if (!any(keep)) {
    msg <- sprintf("None of the Renviron folders exists: %s",
                   paste(sQuote(paths), collapse = ", "))
    logf("- %s", msg)
    if (must_exist) error(msg)
    return(NULL)
  }
  paths <- paths[keep]
  
  filename <- sprintf("%s.Renviron", what)
  pathnames <- file.path(paths, filename)
  keep <- file_test("-f", pathnames)
  if (!any(keep)) {
    msg <- sprintf("Failed to locate a %s file: %s", sQuote(filename),
                   paste(sQuote(paths), collapse = ", "))
    logf("- %s", msg)
    if (must_exist) error(msg)
    return(NULL)
  }
  pathnames <- pathnames[keep]

  ## In case there is more than one match, use the first
  pathname <- pathnames[1]
  logf("- R_CHECK_ENVIRON file found: %s", sQuote(pathname))

  pathname
}
