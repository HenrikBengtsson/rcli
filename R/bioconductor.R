#' @importFrom utils packageVersion
bioconductor_version <- function() {
  v <- Sys.getenv("R_BIOC_VERSION")
  if (nzchar(v)) return(package_version(v))
  v <- tryCatch(packageVersion("BiocVersion")[,1:2], error = function(e) NULL)
  if (!is.null(v)) return(v)
  NULL
}

set_bioconductor <- function(version = bioconductor_version()) {
  bioc_version <- package_version(version)
  sprintf("bioc-%s", bioc_version)
}

check_bioconductor_help <- function() {
  list(
    args  = list(),
    stdin = c("BiocCheck::usage()", "rcli:::done()"),
    silent = TRUE
  )
}


check_bioconductor_default <- function(args, stdin) {
  if (isTRUE(args$help)) return(check_bioconductor_help())

  pathname <- get_Renviron("check", set = set_bioconductor())
  if (!is.null(pathname)) {
    logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
    Sys.setenv(R_CHECK_ENVIRON = pathname)
  }

  args <- attr(args, "command_line_arguments")
  
  list(args = args, stdin = stdin)
}

check_bioconductor_BiocCheck <- function(args, stdin) {
  if (isTRUE(args$help)) return(check_bioconductor_help())

  pathname <- get_Renviron("check", set = set_bioconductor())
  if (!is.null(pathname)) {
    logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
    Sys.setenv(R_CHECK_ENVIRON = pathname)
  }

  args <- c(list(package = args$tarball), args)
  stdin <- "do.call(BiocCheck::BiocCheck, args=args)"

  list(args = args, stdin = stdin)
}
