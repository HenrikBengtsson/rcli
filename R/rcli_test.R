rcli_test_help <- function() {
  cat("rcli_test:\n")
  cat("\n")
  cat("Usage:\n")
  cat(" R CMD check --as=rcli_test <options>\n")
  cat("\n")
  cat("Options:\n")
  cat(" --help  Display this help\n")
  cat("\n")
  cat("Examples:\n")
  cat(" R CMD check --as=rcli_test --help\n")
  cat("\n")
}

parse_check_rcli_test <- function(args, stdin) {
  if (isTRUE(args$help)) {
    args <- list()
    code <- c("rcli:::rcli_test_help()")
    exit <- getOption("rcli.debug.exit", TRUE)
    code <- c(code, if (exit) {
      "quit(save = 'no', status = 0L)"
    } else {
      "return(invisible(0L))"
    })
  } else {
    code <- character(0L)
  }
  list(args = args, stdin = code)
}

