#' @importFrom utils packageVersion
bioconductor_version <- function() {
  v <- Sys.getenv("R_BIOC_VERSION")
  if (nzchar(v)) {
    v <- package_version(v)
    attr(v, "source") <- "R_BIOC_VERSION"   
    logf("- R_BIOC_VERSION=%s", v)
  } else {
    v <- tryCatch(packageVersion("BiocVersion")[,1:2], error = function(e) NULL)
    if (!is.null(v)) {
      attr(v, "source") <- "BiocVersion"   
      logf("- BiocVersion v%s.*", v)
    }
  }
  v
}

set_bioconductor <- function(version = NULL) {
  if (is.null(version)) {
    version <- tryCatch(bioconductor_version(), error = function(ex) {
      error("Failed to infer Bioconductor version: %s", conditionMessage(ex))
    })
  }
  bioc_version <- version
  cat(sprintf("* using Bioconductor version: %s (per %s)\n", bioc_version, attr(bioc_version, "source")))
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
  pathname <- get_Renviron("check", set = set_bioconductor())
  if (is.null(pathname)) {
    error("Failed to located Renviron file for Bioconductor version %s: %s", bioconductor_version(), sQuote(pathname))
  }

  logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
  Sys.setenv(R_CHECK_ENVIRON = pathname)

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
