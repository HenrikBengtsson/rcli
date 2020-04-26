rcli_help <- function() {
  cat("rcli: Demo of the R Command-Line Interface Booster\n")
  cat("\n")
  cat("Usage:\n")
  cat(" R CMD check --as=rcli <options>\n")
  cat("\n")
  cat("Options:\n")
  cat(" --hello    Say hello to the world\n")
  cat(" --help     Display this help\n")
  cat(" --version  Display version of the 'rcli' package\n")
  cat("\n")
  cat("Examples:\n")
  cat(" R CMD check --as=rcli --hello\n")
  cat(" R CMD check --as=rcli --version\n")
  cat("\n")
}

check_rcli_hello <- function(args, stdin) {
  list(
    args   = list(),
    stdin  = c('cat("Hello world!\n")', "rcli:::done()"),
    silent = TRUE
  )
}

check_rcli_help <- function(args, stdin) {
  list(
    args   = list(),
    stdin  = c("rcli:::rcli_help()", "rcli:::done()"),
    silent = TRUE
  )
}

check_rcli_version <- function(args, stdin) {
  list(
    args   = list(),
    stdin  = c('cat(as.character(utils::packageVersion("rcli")), "\n", sep="")', "rcli:::done()"),
    silent = TRUE
  )
}

check_rcli <- function(args, stdin) {
  if (length(args) == 0L || isTRUE(args$help)) return(check_rcli_help())
  if (isTRUE(args$version)) return(check_rcli_version())
  if (isTRUE(args$hello)) return(check_rcli_hello())

  list(args = args, stdin = character(0L))
}
