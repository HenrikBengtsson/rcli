rcli_test_help <- function() {
  cat("rcli-test:\n")
  cat("\n")
  cat("Usage:\n")
  cat(" R CMD check --as=rcli-test <options>\n")
  cat("\n")
  cat("Options:\n")
  cat(" --help  Display this help\n")
  cat("\n")
  cat("Examples:\n")
  cat(" R CMD check --as=rcli-test --help\n")
  cat("\n")
}

check_help <- function(args, stdin) {
  list(
    args   = list(),
    stdin  = c("rcli:::rcli_test_help()", "rcli:::done()"),
    silent = TRUE
  )
}

check_rcli_test <- function(args, stdin) {
  if (isTRUE(args$help)) return(check_help())

  list(args = args, stdin = character(0L))
}
