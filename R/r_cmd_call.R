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
#' @return Nothing.
#'
#' @section Installation & Usage:
#' This function should called at the end of the \file{Rprofile} startup file.
#' For example, append:
#' ```r
#' if (requireNamespace("rcli", quietly=TRUE)) {
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
r_cmd_call <- function(extras = c("debug", "as", "renviron"), args = commandArgs(trailingOnly=TRUE), unload = TRUE, debug = NA, envir = parent.frame(), dryrun = FALSE) {
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
  args <- unlist(strsplit(args, split = "nextArg", fixed = TRUE), use.names = FALSE)
  args <- args[nzchar(args)]
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))

  ## Check for custom R CMD <command> options
  params <- list(debug = NULL, as = NULL)

  if ("debug" %in% extras) {
    pattern <- "^--debug$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      value <- TRUE
      logf("- debug mode: %s", sQuote(value))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      params$debug <- debug <- value
      debug(debug)
    }
  }
  logf(" - args: %s", paste(sQuote(args), collapse = ", "))
  
  if ("as" %in% extras) {
    pattern <- "^--as=(.*)$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      logf(" - detected --as=<value>")
      value <- gsub(pattern, "\\1", args[pos])
      value <- value[length(value)]
      logf("- --as=%s", sQuote(value))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      params$as <- value
    }
  }

  if ("renviron" %in% extras) {
    pattern <- "^--renviron=(.*)$"
    pos <- grep(pattern, args)
    if (length(pos) > 0L) {
      logf(" - detected --renviron=<value>")
      value <- gsub(pattern, "\\1", args[pos])
      value <- value[length(value)]
      logf("- --renviron=%s", sQuote(value))
      args <- args[-pos]
      logf(" - args: %s", paste(sQuote(args), collapse = ", "))
      params$renviron <- value
    }
  }

  custom <- any(!vapply(params, FUN = is.null, FUN.VALUE = TRUE))
  logf(" - custom: %s", custom)

  ## No R CMD extras?
  if (!custom) {
    if (unload) unload(debug = debug)
    return()
  }

  command <- NULL
  stdin <- NULL
  prologue <- list()
  epilogue <- list()
  
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
  logs(" - str: %s", sQuote(params))

  ## Use a custom check as?
  if (!is.null(params$as)) {
    if (command == "check") {
      res <- parse_check_as_option(params$as, args = args, stdin = stdin)
      args <- res$args
      stdin <- res$stdin
    } else {
      error("Unknown R CMD %s option: --as=%s", command, sQuote(params$as))
    }
  }

  logf(" - stdin: %s", paste(sQuote(stdin), collapse = " "))

  ## Use a custom build/check Renviron file?
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


  ## If we intercepted the standard input, then treat it as code and
  ## evaluate it here
  if (!is.null(stdin)) {
    logf("Tweaked R CMD %s:", command)
    logf(" - call: %s", sQuote(paste(stdin, collapse = " ")))
    expr <- parse(text = stdin)
    logf(" - args: %s", paste(sQuote(unlist(args)), collapse = ", "))
    assign("args", args, envir = envir, inherits = FALSE)
    on.exit(rm(list = "args", envir = envir, inherits = FALSE), add = TRUE)

    if (!is.null(params$as)) {
      cat(sprintf("* using --as=%s\n", params$as))
    }
    
    pathname <- Sys.getenv("R_CHECK_ENVIRON", NA_character_)
    if (!is.na(pathname)) {
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
    
    if (length(epilogue) > 0L) {
      for (name in names(epilogue)) {
        cat(sprintf("* Epilogue '%s'\n", name))
        code <- epilogue[[name]]
        cat(sprintf("  - %s\n", code))
        expr <- parse(text = code)
        if (!dryrun) eval(expr, envir = envir)
      }
    }
  }

  if (unload) unload(debug = debug)
}


## A good-enough approach to identify the tarball to be checked
#' @importFrom utils file_test
cmd_args_tarball <- function(args) {
  error_if_not(is.list(args))
  
  if (length(args) == 0L) {
    error("No more arguments to parse")
  }

  names <- names(args)
  args <- args[nchar(names) == 0L]
  if (length(args) == 0L) {
    error("Did you forget to specify a package tarball file?")
  }

  is_string <- vapply(args, FUN = is.character, FUN.VALUE = FALSE)
  args <- args[is_string]
  if (length(args) == 0L) {
    error("Did you forget to specify a package tarball file?")
  }

  args <- unlist(args, use.names = FALSE)
  pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
  tarball <- grep(pattern, args, value = TRUE)
  if (length(tarball) == 0L) {
    error("Did you forget to specify a package tarball file?")
  } else if (length(tarball) > 1L) {
    error("Found more than one package tarball file: %s",
          paste(sQuote(tarball), collapse = ", "))
  }
  logf(" - tarball: %s", sQuote(tarball))
  if (!file_test("-f", tarball)) {
    error("Package tarball file does not exist: ", sQuote(tarball))
  }
  
  tarball
}


check_as_functions <- local({
  db <- list()
  
  function(...) {
    args <- list(...)

    ## List registered check "as" functions
    if (length(args) == 0L) return(db)

    if (length(args) != 1L) stop("Maximum one argument can be specified")

    arg <- args[[1]]
    name <- names(args)[1]
    if (is.character(arg)) {
      res <- db[[arg]]
      if (is.null(res)) stop("No such 'as' function: ", sQuote(arg))
      return(res)
    } else if (is.function(arg)) {
      if (is.null(name)) stop("The function must be named")
      db[[name]] <<- arg
      return(invisible(db))
    } else {
      stop("Unknown type of argument: ", sQuote(typeof(arg)))
    }
  }
})

import_check_as_functions <- function(names) {
  ## Load packages named 'rcli.addon.{as}' and '{as}', if they exists.
  ## If {as} is of form {head}::{tail} or {head}-{tail}, then
  ## {head} is used instead of {as} to load packages.
  ## A rcli "addon" package should register their rcli addons when loaded.
  for (name in names) {
    name <- gsub("(::|-).*", "", name)
    pkg <- sprintf("rcli.addon.%s", name)
#    message("Trying to load: ", sQuote(pkg))
    requireNamespace(pkg, quietly = TRUE)
    pkg <- name
#    message("Trying to load: ", sQuote(pkg))
    requireNamespace(pkg, quietly = TRUE)
#    message("Next ...")
  }
  
  check_as_functions()
}


#' @importFrom R.utils commandArgs
parse_command_args <- function(args) {
  ## WORKAROUND/FIXME: Need a dummy argument /HB 2020-04-21
  parsed_args <- commandArgs(asValues = TRUE, .args = c("", args))
  is_empty <- vapply(parsed_args, FUN = identical, "", FUN.VALUE = FALSE)
  if (is.null(names(parsed_args))) {
    has_name <- rep(FALSE, times = length(parsed_args))
  } else {
    has_name <- nzchar(names(parsed_args))
  }
  parsed_args <- parsed_args[has_name | !is_empty]
  
  ## Find tarball, if it exists
  if (length(parsed_args) > 0L) {
    tarball <- unlist(parsed_args, use.names = FALSE)
    pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
    idxs <- grep(pattern, tarball)
    if (length(idxs) > 0L) {
      tarball <- tarball[idxs]
      logf(" - tarball: %s", paste(sQuote(tarball), collapse = ", "))
      if (length(tarball) > 1L) {
        error("More than one package tarball specific: ", paste(sQuote(tarball), collapse = ", "))
      }
      if (!file_test("-f", tarball)) {
        error("Package tarball file does not exist: ", sQuote(tarball))
      }
      ## Move tarball argument to the end
      parsed_args[idxs] <- NULL
      parsed_args$tarball <- tarball

      ## Drop from non-parsed CLI args too
      pattern <- "[.](tar[.]gz|tgz|tar[.]bz2|tar[.]xz)$"
      idxs <- grep(pattern, args)
      args <- c(args[-idxs], tarball)
      
      ## Sanity check
      logs(list(kk=2, parsed_args =parsed_args, args = args))
#      error_if_not(length(parsed_args) == length(args))
    }
  }

  ## Sanity check
  logs(list(kk=3, parsed_args =parsed_args, args = args))
#  error_if_not(length(parsed_args) == length(args))

  attr(parsed_args, "command_line_arguments") <- args
  parsed_args
}


parse_check_as_option <- function(name, args = character(0L), ...) {
  logs("parse_check_as_option() ...")
  on.exit(logs("parse_check_as_option() ... done"), add = TRUE)
  
  db <- import_check_as_functions(name)

  logf(" - registered check '--as' methods: %s", sQuote(names(db)))

  fcn <- db[[name]]
  if (is.null(fcn)) {
    msg <- sprintf("Unknown R CMD %s value on --%s=%s. There are %d registered styles", "check", "as", sQuote(name), length(db))
    if (length(db) > 0) {
      msg <- sprintf("%s (%s)", msg, paste(sQuote(names(db)), collapse = ", "))
    }
    error(msg)
  }

  logf(" - identified '--as' methods: %s", sQuote(name))

  parsed_args <- parse_command_args(args)
  logs(list(parsed_args = parsed_args))
  
  logp(list(fcn = fcn))
  tryCatch({
    res <- fcn(args = parsed_args, ...)
  }, error = function(ex) {
    error("INTERNAL ERROR: ", conditionMessage(ex))
  })
  logs(list(res = res))
  error_if_not(is.list(res$args) || is.character(res$args))
  error_if_not(is.character(res$stdin))

  ## Make sure that code is valid R code
  expr <- tryCatch({
    parse(text = res$stdin)
  }, error = function(ex) {
    error("INTERNAL ERROR: Syntax error: ", sQuote(res$stdin))
  })
  
  res
}
