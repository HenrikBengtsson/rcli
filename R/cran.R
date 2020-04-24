check_cran <- function(args, stdin) {
  args <- attr(args, "command_line_arguments")
  args <- c("--as-cran", args)
  list(args = args, stdin = stdin)
}

