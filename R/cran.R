check_cran_default <- function(args, stdin) {
  if (isTRUE(args$ci)) {
    Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false")
    args$ci <- NULL
  }
  args <- attr(args, "command_line_arguments")
  args <- c("--as-cran", args)
  list(args = args, stdin = stdin)
}

check_cran_ci <- function(args, stdin) {
  args$ci <- TRUE
  check_cran_default(args, stdin)
}

check_cran_newbie <- function(args, stdin) {
  check_cran_default(args, stdin)
}
