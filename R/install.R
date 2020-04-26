#' Install and uninstall 'rcli' to the R startup process
#'
#' Install and uninstall 'rcli' by appending / removing one line of code to
#' the \file{~/.Rprofile} file.
#'
#' @param path The path where to create / update the \file{.Rprofile} file.
#'
#' @param backup If `TRUE`, a timestamped backup copy of the original file is
#' created before modifying / overwriting it, otherwise not.  If the backup
#' fails, then an error is produced and the R startup file is unmodified.
#'
#' @param overwrite If the R startup file already exist, then `FALSE` (default)
#' appends the rcli code to the end of the file. is overwritten.  If `TRUE`,
#' any pre-existing R startup file is overwritten.
#'
#' @param quiet If `FALSE` (default), detailed messages are generated,
#' otherwise not.
#'
#' @param validate If `TRUE` (default), the installation is validated.
#'
#' @return (invisible) The pathname of the R startup file modified.
#'
#' @describeIn install injects
#' ```r
#' if (nzchar(Sys.getenv("R_CMD")) && require("rcli", quietly=TRUE)) rcli::r_cmd_call()
#' ```
#' to the \file{.Rprofile} file (created if missing).
#'
#' @export
install <- function(path = "~", backup = TRUE, overwrite = FALSE,
                    quiet = FALSE, validate = TRUE) {
  if (quiet) notef <- function(...) NULL

  file <- file.path(path, ".Rprofile")
  if (is_installed(file)) {
    msg <- sprintf("rcli::r_cmd_call() already installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }


  file_exists <- file.exists(file)
  if (backup && file_exists) backup(file, quiet = quiet)

  code <- 'if (nzchar(Sys.getenv("R_CMD")) && require("rcli", quietly=TRUE)) rcli::r_cmd_call()\n'

  ## If the .Rprofile file does not have a newline at the end, which is
  ## a mistake, make sure that the appended startup code is on its own line
  if (file_exists && !eof_ok(file)) code <- paste("\n", code, sep = "")
  
  cat(code, file = file, append = !overwrite)
  if (file_exists) {
    notef("%s 'rcli::r_cmd_call()' to already existing R startup file: %s",
          if (overwrite) "Appended" else "Added", sQuote(file))
  } else {
    notef("Created new R startup file with 'rcli::r_cmd_call()': %s",
          sQuote(file))
  }

  if (validate) validate()

  invisible(file)
}


#' @describeIn install Remove calls to `startup::startup()` and similar.
#' @export
uninstall <- function(path = "~", backup = TRUE, quiet = FALSE) {
  if (quiet) notef <- function(...) NULL

  file <- file.path(path, ".Rprofile")
  if (!is_installed(file)) {
    msg <- sprintf("rcli::r_cmd_call() not installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }

  bfr <- readLines(file, warn = FALSE)
  pattern <- "rcli::r_cmd_call[(].*[)]"
  bfr2 <- grep(pattern, bfr, value = TRUE, invert = TRUE)
  ## Nothing to do?
  if (isTRUE(all.equal(bfr2, bfr))) {
    msg <- sprintf("rcli::r_cmd_call() not installed: %s", sQuote(file))
    notef(msg)
    warning(msg)
    return(file)
  }
  if (backup) backup(file, quiet = quiet)
  writeLines(bfr2, con = file)
  notef("R startup file updated: %s", sQuote(file))

  invisible(file)
}


is_installed <- function(file = file.path("~", ".Rprofile")) {
  if (!file.exists(file)) return(FALSE)
  bfr <- readLines(file, warn = FALSE)
  bfr <- gsub("#.*", "", bfr)
  pattern <- "rcli::r_cmd_call[(].*[)]"
  res <- any(grepl(pattern, bfr))
  attr(res, "file") <- file
  res
}

#' @rdname install
#' @importFrom utils file_test
#' @export
validate <- function() {
  R_bin <- file.path(R.home("bin"), "R")
  stop_if_not(file_test("-x", R_bin))
  args <- c("CMD", "check", "--as=rcli-test")
  output <- system2(R_bin, args = args, stdout = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("rcli::r_cmd_call() is not properly installed in .Rprofile. 'R CMD check --as=rcli-test' terminated with a non-zero exit code")
  }
  expect <- "* using --as=rcli-test"
  if (!any(grepl(expect, output, fixed = TRUE))) {
    stop(sprintf("rcli::r_cmd_call() is not properly installed in .Rprofile. 'R CMD check --as=rcli-test' did not output the string %s", sQuote(expect)))
  }
  message("Validated that 'R CMD check --as=<style>' works")
  invisible(TRUE)
}