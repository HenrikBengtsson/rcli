## A good-enough approach to identify the tarball to be checked
#' @importFrom utils file_test
cmd_args_tarball <- function(args) {
  error_if_not(is.list(args))
  
  if (length(args) == 0L) {
    error("No more arguments to parse")
  }

  names <- names(args)
  args <- args[nchar(names) == 0L]
  if (length(args) == 0L) {
    error("Did you forget to specify a package tarball file?")
  }

  is_string <- vapply(args, FUN = is.character, FUN.VALUE = FALSE)
  args <- args[is_string]
  if (length(args) == 0L) {
    error("Did you forget to specify a package tarball file?")
  }

  args <- unlist(args, use.names = FALSE)
  pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
  tarball <- grep(pattern, args, value = TRUE)
  if (length(tarball) == 0L) {
    error("Did you forget to specify a package tarball file?")
  } else if (length(tarball) > 1L) {
    error("Found more than one package tarball file: %s",
          paste(sQuote(tarball), collapse = ", "))
  }
  logf(" - tarball: %s", sQuote(tarball))
  if (!file_test("-f", tarball)) {
    error("Package tarball file does not exist: %s", sQuote(tarball))
  }
  
  tarball
}




parse_command_args <- function(args) {
  ## WORKAROUND/FIXME: Need a dummy argument /HB 2020-04-21
  parsed_args <- command_args(asValues = TRUE, .args = c("", args))
  is_empty <- vapply(parsed_args, FUN = identical, "", FUN.VALUE = FALSE)
  if (is.null(names(parsed_args))) {
    has_name <- rep(FALSE, times = length(parsed_args))
  } else {
    has_name <- nzchar(names(parsed_args))
  }
  parsed_args <- parsed_args[has_name | !is_empty]
  
  ## Find tarball, if it exists
  if (length(parsed_args) > 0L) {
    tarball <- unlist(parsed_args, use.names = FALSE)
    pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
    idxs <- grep(pattern, tarball)
    if (length(idxs) > 0L) {
      tarball <- tarball[idxs]
      logf(" - tarball: %s", paste(sQuote(tarball), collapse = ", "))
      if (length(tarball) > 1L) {
        error("More than one package tarball specific: ", paste(sQuote(tarball), collapse = ", "))
      }
      if (!file_test("-f", tarball)) {
        error("Package tarball file does not exist: %s", sQuote(tarball))
      }
      ## Move tarball argument to the end
      parsed_args[idxs] <- NULL
      parsed_args$tarball <- tarball

      ## Drop from non-parsed CLI args too
      pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
      idxs <- grep(pattern, args)
      args <- c(args[-idxs], tarball)
      
      ## Sanity check
#      logs(list(kk=2, parsed_args =parsed_args, args = args))
#      error_if_not(length(parsed_args) == length(args))
    }
  }

  ## Sanity check
#  logs(list(kk=3, parsed_args =parsed_args, args = args))
#  error_if_not(length(parsed_args) == length(args))

  attr(parsed_args, "command_line_arguments") <- args
  parsed_args
}



consume_arg <- function(parsed_args, name) {
  args <- attr(parsed_args, "command_line_arguments")
  parsed_args[[name]] <- NULL
  args <- grep(sprintf("^--%s(=.*|)$", name), args, invert = TRUE, value = TRUE)
  attr(parsed_args, "command_line_arguments") <- args
  parsed_args
}