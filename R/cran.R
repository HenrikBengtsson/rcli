check_cran_default <- function(args, stdin) {
  if (isTRUE(args$offline)) {
    cat(sprintf("* using option %s\n", sQuote("--offline")))
    envs <- c(
      ## Force to run CRAN checks in offline mode
      "_R_CHECK_CRAN_INCOMING_"                           = "FALSE",
      "_R_CHECK_CRAN_INCOMING_REMOTE_"                    = "FALSE",
      ## Further prevent internet queries
      "_R_CHECK_ORPHANED_"                                = "FALSE",
      "_R_CHECK_XREFS_USE_ALIASES_FROM_CRAN_"             = "FALSE"
    )
    pathname <- temp_Renviron("check", envs = envs)
    logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
    Sys.setenv(R_CHECK_ENVIRON = pathname)
    args <- consume_arg(args, "offline")
  }
  args <- attr(args, "command_line_arguments")
  args <- c("--as-cran", args)
  list(args = args, stdin = stdin)
}

check_cran_offline <- function(args, stdin) {
  args$offline <- TRUE
  check_cran_default(args, stdin)
}

check_cran_newbie <- function(args, stdin) {
  check_cran_default(args, stdin)
}
