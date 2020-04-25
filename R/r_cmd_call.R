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
#' @param dryrun If TRUE, nothing is evaluated.
#'
#' @return (logical; invisible) TRUE or FALSE.
#'
#' @section Installation & Usage:
#' This function should called at the end of the \file{Rprofile} startup file.
#' For example, append:
#' ```r
#' if (nzchar(Sys.getenv("R_CMD")) && require("rcli", quietly=TRUE)) {
#'   rcli::r_cmd_call()
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
#' @export
r_cmd_call <- function(extras = c("debug", "as", "config", "renviron"), args = commandArgs(trailingOnly=TRUE), unload = TRUE, debug = NA, envir = parent.frame(), dryrun = FALSE) {
  R_CMD <- Sys.getenv("R_CMD")

  ## Nothing to do?
  if (!nzchar(R_CMD) || length(args) == 0L || length(extras) == 0L) {
    if (unload) unload()
    return(invisible(FALSE))
  }

  extras <- match.arg(extras, several.ok = TRUE)
  debug(debug)
  debug <- debug()
  logf(" - checking for: %s", paste(sQuote(extras), collapse = ", "))
  
  ## Get R CMD <command> options after --args
  args_org <- args

  ## Parse "nextArg" encoded arguments
  args <- unlist(strsplit(args, split = "nextArg", fixed = TRUE), use.names = FALSE)
  args <- args[nzchar(args)]
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Check for custom R CMD <command> options
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  params <- list(debug = NULL, as = NULL)

  ## Flags
  for (name in c("debug")) {
    if (name %in% extras) {
      pattern <- sprintf("^--%s$", name)
      pos <- grep(pattern, args)
      if (length(pos) > 0L) {
        ## Special (enable debug as soon as possible)
        if (name == "debug") {
          debug <- TRUE
          debug(TRUE)
          logf("- debug mode: %s", sQuote(debug))
        }
        args <- args[-pos]
        logf(" - args: %s", paste(sQuote(args), collapse = ", "))
        params[[name]] <- TRUE
      }
    }
  }
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))



  ## Key-value options
  for (name in c("as", "config", "renviron")) {
    if (name %in% extras) {
      pattern <- sprintf("^--%s=(.*)$", name)
      pos <- grep(pattern, args)
      if (length(pos) > 0L) {
        logf(" - detected --%s=<value>", name)
        value <- gsub(pattern, "\\1", args[pos])
        value <- value[length(value)]
        logf("- --%s=%s", name, sQuote(value))
        args <- args[-pos]
        logf(" - args: %s", paste(sQuote(args), collapse = ", "))
        params[[name]] <- value
      }
    }
  }

  custom <- any(!vapply(params, FUN = is.null, FUN.VALUE = TRUE))
  logf(" - custom: %s", custom)

  ## No R CMD extras?
  if (!custom) {
    if (unload) unload(debug = debug)
    return(invisible(FALSE))
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Identify which R CMD <command> was called
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  command <- NULL
  stdin <- NULL
  silent <- FALSE
  
  ## Then we need to infer what <command> is in use
  stdin <- getOption("rcli.debug.stdin", readLines(stdin(), warn = FALSE))
  logf(" - stdin: %s", paste(sQuote(stdin), collapse = "; "))
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
  logf(" - stdin: %s", sQuote(stdin))
  log(" - str:")
  logs(sQuote(params))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Custom --config=<value>
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Use a custom configuration file?
  if (!is.null(params$config)) {
    pathname <- params$config
    logf(" - Reading config file: %s", sQuote(pathname))
    config <- parse_config_dcf(pathname)
    logp(config)
    
    if (length(config$env) > 0L) do.call(Sys.setenv, args = config$env)

    args <- c(args, config$options)
  }

  logf(" - stdin: %s", paste(sQuote(stdin), collapse = " "))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Custom --as=<value>
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Use a custom check as?
  if (!is.null(params$as)) {
    if (command == "check") {
      res <- parse_check_as_option(params$as, args = args, stdin = stdin)
      args <- res$args
      stdin <- res$stdin
      silent <- isTRUE(res$silent)
    } else {
      error("Unknown R CMD %s option: --as=%s", command, sQuote(params$as))
    }
  }

  logf(" - stdin: %s", paste(sQuote(stdin), collapse = " "))


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Custom build/check Renviron file
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.null(params$renviron)) {
    if (command %in% c("build", "check")) {
      env <- sprintf("R_%s_ENVIRON", toupper(command))
      default <- sprintf("~/.R/%s.Renviron", command)
      
      ## Infer where the Renviron file for this <command> should be
      pathname <- do.call(Sys.getenv, args = list(env, unset = default))
      path <- dirname(pathname)
      filename <- sprintf("%s-%s.Renviron", params$renviron, command)
      pathname <- file.path(path, filename)
      if (!is_file(pathname)) {
        error("No such %s.Renviron file for R CMD check --renviron=%s: %s",
              command, params$renviron, sQuote(pathname))
      }
      args0 <- list(pathname)
      names(args0) <- env
      do.call(Sys.setenv, args = args0)

      pathname <- do.call(Sys.getenv, args = list(env))
      error_if_not(nzchar(pathname))
    } else {
      error("Unknown R CMD %s option: --renviron=%s", command, sQuote(params$renviron))
    }
  }


  ## If possible, do an early return and let the active R CMD <command>
  ## take over from here
  if (is.null(stdin)) {
    if (unload) unload(debug = debug)
    return(invisible(TRUE))
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Process custom features
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## If we intercepted the standard input, then treat it as code and
  ## evaluate it here
  logf("Tweaked R CMD %s:", command)
  logf(" - call: %s", sQuote(paste(stdin, collapse = " ")))
  expr <- parse(text = stdin)
  logf(" - args: %s", paste(sQuote(unlist(args)), collapse = ", "))
  assign("args", args, envir = envir, inherits = FALSE)
  on.exit(rm(list = "args", envir = envir, inherits = FALSE), add = TRUE)
  if (!silent && !is.null(params$as)) {
    cat(sprintf("* using --as=%s\n", params$as))
  }
  
  pathname <- Sys.getenv("R_CHECK_ENVIRON", NA_character_)
  if (!silent && !is.na(pathname)) {
    cat(sprintf("* using R_CHECK_ENVIRON=%s\n", dQuote(pathname)))
    if (!file_test("-f", pathname)) {
      error("No such file: %s", sQuote(pathname))
    }
  }
  
  local({
    opwd <- getwd()
    logf("Working directory: %s", sQuote(opwd))
    on.exit(setwd(opwd))
    logf("Expression: %s", paste(deparse(expr), collapse = " "))
    if (!dryrun) {
      tryCatch({
        res <- eval(expr, envir = envir)
      }, error = function(ex) {
        error("INTERNAL ERROR: %s", conditionMessage(ex))
      })
    }
    logf("Results: %s", paste(sQuote(res), collapse = ", "))
  })
  

  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Cleanup
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (unload) unload(debug = debug)

  invisible(TRUE)
}
