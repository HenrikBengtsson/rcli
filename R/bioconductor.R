#' @importFrom utils packageVersion
bioconductor_version <- function(must_work = TRUE) {
  v <- tryCatch(packageVersion("BiocVersion")[,1:2], error = function(e) NULL)
  if (!is.null(v)) {
    attr(v, "source") <- "BiocVersion"   
    logf("- BiocVersion v%s.*", v)
  } else {
    v <- Sys.getenv("R_BIOC_VERSION")
    if (nzchar(v)) {
      v <- package_version(v)
      attr(v, "source") <- "R_BIOC_VERSION"   
      logf("- R_BIOC_VERSION=%s", v)
    } else {
      v <- NULL
    }
  }
  if (is.null(v) && must_work) {
    error("Bioconductor version is unknown. Make sure 'BiocVersion' is installed or 'R_BIOC_VERSION' is set")
  }
  cat(sprintf("* using Bioconductor version: %s (per %s)\n", v, attr(v, "source")))
  v
}

set_bioconductor <- function(version = NULL) {
  if (is.null(version)) version <- bioconductor_version()
  sprintf("bioc-%s", version)
}

check_bioconductor_help <- function() {
  list(
    args  = list(),
    stdin = c('suppressMessages(requireNamespace("BiocCheck"))', "BiocCheck::usage()", "rcli:::done()"),
    silent = TRUE
  )
}


check_bioconductor_default <- function(args, stdin) {
  version <- bioconductor_version()
  if (version <= "3.10") {
    cat("* using regular 'R CMD check' since Bioconductor (<= 3.10) does not use customized testing\n")
  } else {
    pathname <- find_Renviron("check", set = set_bioconductor(version))
    if (is.null(pathname)) {
      error("Failed to located Renviron file for Bioconductor version %s: %s", version, sQuote(pathname))
    }
  
    logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
    Sys.setenv(R_CHECK_ENVIRON = pathname)
  }

  args <- attr(args, "command_line_arguments")
  
  list(args = args, stdin = stdin)
}

check_bioconductor_BiocCheck <- function(args, stdin) {
  if (isTRUE(args$help)) return(check_bioconductor_help())

  pathname <- find_Renviron("check", set = set_bioconductor())
  if (!is.null(pathname)) {
    logf("- Setting environment variable R_CHECK_ENVIRON=%s", dQuote(pathname))
    Sys.setenv(R_CHECK_ENVIRON = pathname)
  }

  args <- c(list(package = args$tarball), args)
  stdin <- c('suppressMessages(requireNamespace("BiocCheck"))',
             "do.call(BiocCheck::BiocCheck, args=args)")

  list(args = args, stdin = stdin)
}


check_bioconductor_newbie <- function(args, stdin) {
  check_bioconductor_BiocCheck(args, stdin)
}
