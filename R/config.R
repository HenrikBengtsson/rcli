#' @importFrom utils file_test
parse_config_dcf <- function(pathname) {
  if (!file_test("-f", pathname)) {
    error("No such configuration file: %s", sQuote(pathname))
  }
  config <- suppressWarnings(read.dcf(pathname, all = TRUE))
  config <- as.list(config)
  config <- lapply(config, FUN = function(x) {
    x <- x[!is.na(x)]
    x <- strsplit(x, split = "\n", fixed = TRUE)
    unlist(x)
  })

  ## Parse 'env' entries
  if (length(config$env) > 0L) {
    pattern <- "^([^=]+)=(.*)$"
    invalid <- grep(pattern, config$env, invert = TRUE)
    if (length(invalid) > 0L) {
      error("SYNTAX ERROR: Unknown %s entry in configuration file %s: %s", sQuote("env"), sQuote(pathname), paste(sQuote(invalid), collapse = ", "))
    }
    envs <- list()
    for (env in config$env) {
      name <- gsub(pattern, "\\1", env)
      value <- gsub(pattern, "\\2", env)
      ## Trim surrounding quotes
      value <- gsub('^["](.*)["]$', "\\1", value)
      value <- gsub("^['](.*)[']$", "\\1", value)
      envs[[name]] <- value
    }
    config$env <- envs
  }

  ## Parse 'asserts' entries
  if (length(config$assert) > 0L) {
    logp(config$assert)
    tryCatch(parse(text = config$assert), error = function(ex) {
      error("Syntax error in %s entry of configuration file %s: %s", sQuote("assert"), sQuote(pathname), conditionMessage(ex))
    })
  }

  attr(config, "pathname") <- pathname
  
  config
}

config_assert <- function(config, envir = parent.frame()) {
  ## Make sure assertions are met
  if (length(config$assert) == 0L) return(config)

  pathname <- attr(config, "pathname")

  exprs <- parse(text = config$assert)
  for (kk in seq_along(exprs)) {
    expr <- exprs[[kk]]
    res <- tryCatch(eval(expr, envir = envir), error = identity)
    if (inherits(res, "error")) {
      error("Evaluation of %s expression #%d resulted in an error: %s",
            sQuote("assert"), kk, conditionMessage(res))
    }
    logs(list(expr = expr, res = res))
    if (!is.logical(res) || length(res) != 1L || is.na(res)) {
      error("Expression #%d (%s) of %s did not return TRUE or FALSE: %s",
            kk, paste(deparse(expr), collapse = "; "), sQuote("assert"),
            paste(deparse(res), collapse = "; "))
    }
    if (!res) {
      error("Cannot use configuration file (%s) because of unfullfilled assertion: %s", sQuote(pathname), paste(deparse(expr), collapse = "; "))
    }
  }

  config
}
