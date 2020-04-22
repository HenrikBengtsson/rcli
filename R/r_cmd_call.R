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
#'
#' @importFrom utils file_test
#' @importFrom R.utils commandArgs
#' @export
r_cmd_call <- function(extras = c("debug", "flavor", "renviron"), args = commandArgs(trailingOnly=TRUE), unload = TRUE, debug = NA, envir = parent.frame()) {
  R_CMD <- Sys.getenv("R_CMD")

  ## Nothing to do?
  if (!nzchar(R_CMD) || length(args) == 0L || length(extras) == 0L) {
    if (unload) unload()
    return()
  }
  
  extras <- match.arg(extras, several.ok = TRUE)
  debug(debug)
  debug <- debug()
  logf(" - checking for: %s", paste(sQuote(extras), collapse = ", "))
  
  ## Get R CMD <command> options after --args
  args_org <- args

  ## Parse "nextArg" encoded arguments
  args <- strsplit(args, split = "nextArg", fixed = TRUE)[[1]]
  args <- args[nzchar(args)]
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))

  ## Check for custom R CMD <command> options
  custom <- FALSE

  if ("debug" %in% extras) {
    pattern <- "^--debug$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      debug(TRUE)
      debug <- TRUE
      logf("- debug mode: %s", sQuote(debug))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      custom <- TRUE
    }
  }
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))
  
  flavor <- NULL
  if ("flavor" %in% extras) {
    pattern <- "^--flavor=(.*)$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      logf(" - detected --flavor=<value>")
      value <- gsub(pattern, "\\1", args[pos])
      flavor <- value[length(value)]
      logf("- --flavor=%s", sQuote(flavor))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      custom <- TRUE
    }
  }

  renviron <- NULL
  if ("renviron" %in% extras) {
    pattern <- "^--renviron=(.*)$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      logf(" - detected --renviron=<value>")
      value <- gsub(pattern, "\\1", args[pos])
      renviron <- value[length(value)]
      logf("- --renviron=%s", sQuote(renviron))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      custom <- TRUE
    }
  }

  logf(" - custom: %s", custom)

  ## No R CMD extras?
  if (!custom) {
    if (unload) unload(debug = debug)
    return()
  }

  error <- function(fmtstr, ...) {
    msg <- sprintf(fmtstr, ...)
    msg <- sprintf("ERROR: %s", msg)
    message(msg)
    stop(msg)
  }
  
  ## A good-enough approach to identify the tarball to be checked
  cmd_args_tarball <- function(args) {
    pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
    tarball <- grep(pattern, args, value = TRUE)
    if (length(tarball) == 0L) {
      error("Did you forget to specify a tarball to be checked?")
    }
    tarball <- tarball[length(tarball)]
    logf(" - tarball: %s", sQuote(tarball))
    if (!file_test("-f", tarball)) {
      error("No such file: ", sQuote(tarball))
    }
    tarball
  }

  command <- NULL
  stdin <- NULL
  prologue <- list()
  epilogue <- list()
  
  ## Then we need to infer what <command> is in use
  stdin <- readLines(stdin(), warn = FALSE)
  stdin <- stdin[nzchar(stdin)]
  if (stdin[1] == "tools:::.build_packages()") {
    command <- "build"
    stdin[1] <- "tools:::.build_packages(args)"
  } else if (stdin[1] == "tools:::.check_packages()") {
    command <- "check"
    stdin[1] <- "tools:::.check_packages(args)"
  } else if (stdin[1] == "tools:::.install_packages()") {
    command <- "INSTALL"
    stdin[1] <- "tools:::.install_packages(args)"
  } else {
    command <- "<unknown>"
  }
  logf(" - command: %s", sQuote(command))


  ## Use a custom check flavor?
  if (!is.null(flavor)) {
    if (command == "check") {
      if (flavor == "BiocCheck") {
        ## Assert that the 'BiocCheck' package is installed
        pkg <- "BiocCheck"
        res <- requireNamespace(pkg, quietly = TRUE)
        if (!res) error("Failed to load the '%s' package", pkg)
        ## WORKAROUND/FIXME: Need a dummy argument /HB 2020-04-21
        args <- commandArgs(asValues = TRUE, .args = c("", args))
        if (isTRUE(args$help)) {
          code <- "BiocCheck::usage()"
          expr <- parse(text = code)
          eval(expr, envir = envir)
          quit(save = "no", status = 0L)
        }
        tarball <- cmd_args_tarball(args)
        args <- c(list(tarball), args)
        code <- "do.call(BiocCheck::BiocCheck, args=args)"
        ## Assert that code is valid
        parse(text = code)
        stdin[1] <- code
      } else {
        error("Unknown R CMD %s flavor: --flavor=%s", command, sQuote(flavor))
      }
    } else {
      error("Unknown R CMD %s option: --flavor=%s", command, sQuote(flavor))
    }
  }

  logf(" - stdin: %s", paste(sQuote(stdin), collapse = " "))

  ## Use a custom build/check Renviron file?
  if (!is.null(renviron)) {
    if (command %in% c("build", "check")) {
      env <- sprintf("R_%s_ENVIRON", toupper(command))
      default <- sprintf("~/.R/%s.Renviron", command)
      
      ## Infer where the Renviron file for this <command> should be
      pathname <- do.call(Sys.getenv, args = list(env, unset = default))
      path <- dirname(pathname)
      filename <- sprintf("%s-%s.Renviron", renviron, command)
      pathname <- file.path(path, filename)
      if (!is_file(pathname)) {
        error("No such %s.Renviron file for R CMD check --renviron=%s: %s",
              command, renviron, sQuote(pathname))
      }
      args0 <- list(pathname)
      names(args0) <- env
      do.call(Sys.setenv, args = args0)

      pathname <- do.call(Sys.getenv, args = list(env))
      stopifnot(nzchar(pathname))
    } else {
      error("Unknown R CMD %s option: --renviron=%s", command, sQuote(renviron))
    }
  }


  ## If we intercepted the standard input, then treat it as code and
  ## evaluate it here
  if (!is.null(stdin)) {
    logf("Tweaked R CMD %s:", command)
    logf(" - call: %s", sQuote(paste(stdin, collapse = " ")))
    expr <- parse(text = stdin)
    logf(" - args: %s", paste(sQuote(args), collapse = ", "))
    assign("args", args, envir = envir, inherits = FALSE)
    on.exit(rm(list = "args", envir = envir, inherits = FALSE))
    
    if (!is.null(renviron)) {
      cat(sprintf("* using %s=%s (from --renviron=%s)\n",
                  env, sQuote(pathname), renviron))
    }

#    if (!is.null(flavor)) {
#      cat(sprintf("* using --flavor=%s\n", flavor))
#    }
    
    local({
      opwd <- getwd()
      logf("Working directory: %s", sQuote(opwd))
      on.exit(setwd(opwd))
      res <- eval(expr, envir = envir)
      logf("Results: %s", paste(sQuote(res), collapse = ", "))
    })
    
    if (length(epilogue) > 0L) {
      for (name in names(epilogue)) {
        cat(sprintf("* Epilogue '%s'\n", name))
        code <- epilogue[[name]]
        cat(sprintf("  - %s\n", code))
        expr <- parse(text = code)
        eval(expr, envir = envir)
      }
    }
  }

  if (unload) unload(debug = debug)
}
