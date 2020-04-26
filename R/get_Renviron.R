#' @importFrom utils file_test
find_custom_Renviron <- function(what = c("build", "check"), set, package = .packageName, must_exist = FALSE) {
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


#' @importFrom utils file_test
find_Renviron <- function(what = c("build", "check"), on_not_found = c("drop")) {
  what <- match.arg(what)
  on_not_found <- match.arg(on_not_found)

  if (what == "build") {
    env <- "R_BUILD_ENVIRON"
  } else if (what == "check") {
    env <- "R_CHECK_ENVIRON"
  }

  pathname <- Sys.getenv(env, NA_character_)
  if (!is.na(pathname)) {
    logf("- Already set R_CHECK_ENVIRON=%s", pathname)
    if (!file_test("-f", pathname)) pathname <- NULL
    return(pathname)
  }

  filename <- sprintf("%s.Renviron", what)
  pathname <- file.path("~", ".R", filename)
  if (!file_test("-f", pathname)) pathname <- NULL
  
  pathname
}

temp_Renviron <- function(what = c("build", "check"), envs) {
  stop_if_not(is.character(envs))
  if (length(envs) == 0L) return(NULL)
  names <- names(envs)
  str(names)
  stop_if_not(length(names) > 0L, all(nzchar(names) > 0L))

  lines <- character(0L)

  prior <- find_Renviron(what = what)
  if (!is.null(prior)) {
    lines <- c(lines, sprintf("## Imported from %s:", sQuote(prior)))
    lines <- c(lines, readLines(prior, warn = FALSE))
  } else {
  }
  
  lines <- c(lines, "", "## Temporarily addded by the 'rcli' package:")
  bfr <- sprintf("%s=%s", names, envs)
  lines <- c(lines, bfr)

  pathname <- tempfile(fileext = sprintf(".%s.Renviron", what))
  writeLines(lines, con = pathname)

  pathname
}


use_Renviron <- function(what = c("build", "check"), envs) {
  if (length(envs) == 0L) return()
  pathname <- temp_Renviron("check", envs = envs)
  logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
  Sys.setenv(R_CHECK_ENVIRON = pathname)
}
