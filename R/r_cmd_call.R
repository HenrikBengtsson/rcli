#' _(EXPERIMENTAL)_ Performs a tweaked 'R CMD' call
#'
#' @param extras (character vector) Additional features that should be injected
#' to `R CMD`.
#'
#' @param args (optional) Command-line arguments after the `--args` option
#'
#' @param unload If `TRUE`, then the package is unloaded afterward, otherwise
#' not.
#'
#' @param debug If TRUE, debug information is outputted, otherwise not.
#'
#' @param envir The environment where to evaluate any \R expression used
#' internally by the `R CMD` call.
#'
#' @return Nothing.
#'
#' @section Installation & Usage:
#' This function should called at the end of the \file{Rprofile} startup file.
#' For example, append:
#' ```r
#' if (requireNamespace("rcliaddons", quietly=TRUE)) {
#'   rcliaddons::r_cmd_call()
#' }
#' ```
#' to your \file{~/.Rprofile} file.
#'
#' @section The `renviron` feature:
#' When `extras` include `"renviron"`, this function will add support for the
#' `--renviron=<name>` command-line option to `R CMD build` and `R CMD check`.
#' When specified, `R CMD build --renviron=<name>` will run with
#' `R_BUILD_ENVIRON` set to \file{~/.R/<name>-build.Renviron}.
#' Similarly, `R CMD check --renviron=<name>` will run with
#' `R_CHECK_ENVIRON` set to \file{~/.R/<name>-check.Renviron}.
#'
#' @keywords internal
#' @export
r_cmd_call <- function(extras = c("renviron"), args = commandArgs(trailingOnly=TRUE), unload = TRUE, debug = NA, envir = parent.frame()) {
  R_CMD <- Sys.getenv("R_CMD")

  ## Nothing to do?
  if (!nzchar(R_CMD) || length(args) == 0L || length(extras) == 0L) {
    if (unload) unload()
    return()
  }
  
  extras <- match.arg(extras, several.ok = TRUE)
  debug(debug)
  debug <- debug()

  ## Get R CMD <command> options after --args
  args_org <- args

  ## Parse "nextArg" encoded arguments
  args <- strsplit(args, split = "nextArg", fixed = TRUE)[[1]]
  args <- args[nzchar(args)]

  ## Check for custom R CMD <command> options
  custom <- FALSE
  
  renviron <- NULL
  if ("renviron" %in% extras) {
    pattern <- "^--renviron=(.*)$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      renviron <- gsub(pattern, "\\1", args[pos])
      renviron <- renviron[length(renviron)]
      args <- args[-pos]
      custom <- TRUE
    }
  }

  ## No R CMD extras?
  if (!custom) {
    if (unload) unload(debug = debug)
    return()
  }


  command <- NULL
  stdin <- NULL

  ## Then we need to infer what <command> is in use
  stdin <- readLines(stdin(), warn = FALSE)
  stdin <- stdin[nzchar(stdin)]
  if (stdin[1] == "tools:::.build_packages()") {
    command <- "build"
  } else if (stdin[1] == "tools:::.check_packages()") {
    command <- "check"
  } else if (stdin[1] == "tools:::.install_packages()") {
    command <- "INSTALL"
  } else {
    command <- "<unknown>"
  }
  
  ## Use a custom build/check renviron?
  if (!is.null(renviron)) {
    stopifnot(!is.null(command))
    if (command %in% c("build", "check")) {
      env <- sprintf("R_%s_ENVIRON", toupper(command))
      default <- sprintf("~/.R/%s.Renviron", command)
      
      ## Infer where the Renviron file for this <command> should be
      pathname <- do.call(Sys.getenv, args = list(env, unset = default))
      path <- dirname(pathname)
      filename <- sprintf("%s-%s.Renviron", renviron, command)
      pathname <- file.path(path, filename)
      if (!is_file(pathname)) {
        stop(sprintf("No such %s.Renviron file for R CMD check --renviron=%s: %s",
                     command, renviron, sQuote(pathname)), call. = FALSE)
      }
      args0 <- list(pathname)
      names(args0) <- env
      do.call(Sys.setenv, args = args0)

      pathname <- do.call(Sys.getenv, args = list(env))
      stopifnot(nzchar(pathname))

      if (command == "build") {
        stdin[1] <- "tools:::.build_packages(args)"
      } else if (command == "check") {
        stdin[1] <- "tools:::.check_packages(args)"
      }
    } else {
      stop(sprintf("Unknown R CMD %s option: --renviron=%s",
                   command, sQuote(renviron)))
    }
  }
  
  ## If we intercepted the standard input, then treat it as code and
  ## evaluate it here
  if (!is.null(stdin)) {
    logf("Tweaked R CMD %s call: %s",
         command, sQuote(paste(stdin, collapse = "\n")))
    
    expr <- parse(text = stdin)
    assign("args", args, envir = envir, inherits = FALSE)
    on.exit(rm(list = "args", envir = envir, inherits = FALSE))
    
    if (!is.null(renviron)) {
      cat(sprintf("* using %s=%s (from --renviron=%s)\n",
                  env, sQuote(pathname), renviron))
    }
    eval(expr, envir = envir)
  }

  if (unload) unload(debug = debug)
}
