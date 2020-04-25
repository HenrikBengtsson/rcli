stop_if_not <- function(...) {
  res <- list(...)
  n <- length(res)
  if (n == 0L) return()

  for (ii in 1L:n) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "...")
        stop(sQuote(call), " is not TRUE", call. = FALSE, domain = NA)
    }
  }
}


error <- function(fmtstr, ..., call. = TRUE) {
  msg <- sprintf(fmtstr, ...)
  msg <- sprintf("ERROR: %s", msg)
  message(msg)
  stop(msg, call. = call.)
}

error_if_not <- function(...) {
  exprs <- substitute(...)
  expr <- bquote({
    tryCatch(stop_if_not(.(exprs)), error = function(ex) {
      error("INTERNAL ERROR: %s", conditionMessage(ex))
    })
  })
  eval.parent(expr)
}


is_dir <- function(f) {
  if (length(f) != 1L) {
    stop(sprintf("INTERNAL ERROR in %s:::is_dir(): only scalar input is supported: [n=%d] %s", .packageName, length(f), paste(sQuote(f), collapse = ", ")))
  }
  nzchar(f) && file.exists(f) && file.info(f)$isdir
}

is_file <- function(f) {
  if (length(f) != 1L) {
    stop(sprintf("INTERNAL ERROR in %s:::is_file(): only scalar input is supported: [n=%d] %s", .packageName, length(f), paste(sQuote(f), collapse = ", ")))
  }
  nzchar(f) && file.exists(f) && !file.info(f)$isdir
}


## base::file.size() was only introduced in R 3.2.0
file_size <- function(...) file.info(..., extra_cols = FALSE)$size

path_info <- function(f, extra = NULL) {
  if (!nzchar(f)) return(sQuote(""))
  fx <- path.expand(f)
  if (!is.null(extra)) {
    extra <- paste("; ", extra, sep = "")
  } else {
    extra <- ""
  }

  if (!is_dir(f)) {
    return(sprintf("%s (non-existing directory%s)", sQuote(f), extra))
  }

  if (fx == f) {
    sprintf("%s (existing folder%s)", sQuote(f), extra)
  } else {
    sprintf("%s => %s (existing folder%s)", sQuote(f), sQuote(fx), extra)
  }
}


parse_renviron <- function(f) {
  bfr <- readLines(f, warn = FALSE)
  bfr <- grep("^[ \t]*#", bfr, value = TRUE, invert = TRUE)
  bfr <- grep("^[ \t]*$", bfr, value = TRUE, invert = TRUE)
  bfr <- grep("=.*$", bfr, value = TRUE)
  pattern <- "^([^=]*)[ \t]*=[ \t]*(.*)$"
  bfr <- grep(pattern, bfr, value = TRUE)
  names <- gsub(pattern, "\\1", bfr)
  values <- gsub(pattern, "\\2", bfr)
  names(values) <- names
  values
}


nlines <- function(f) {
  oopts <- options(encoding = "native.enc")
  on.exit(options(oopts))
  bfr <- readLines(f, warn = FALSE)
  bfr <- grep("^[ \t]*#", bfr, value = TRUE, invert = TRUE)
  bfr <- grep("^[ \t]*$", bfr, value = TRUE, invert = TRUE)
  length(bfr)
}


file_info <- function(f, type = "txt", extra = NULL) {
  if (!nzchar(f)) return(sQuote(""))
  fx <- path.expand(f)
  if (length(extra) > 0L) {
    extra <- paste("; ", extra, sep = "")
  } else {
    extra <- ""
  }
  if (!is_file(f)) {
    return(sprintf("%s (non-existing file%s)", sQuote(f), extra))
  }

  if (fx == f) {
    prefix <- sQuote(f)
  } else {
    prefix <- sprintf("%s => %s", sQuote(f), sQuote(fx))
  }
  if (type == "binary") {
    sprintf("%s (binary file; %d bytes%s)", prefix, file_size(f), extra)
  } else if (type == "env") {
    vars <- names(parse_renviron(f))
    nvars <- length(vars)
    if (nvars > 0) {
      vars <- sprintf(" (%s)", paste(sQuote(vars), collapse = ", "))
    } else {
      vars <- ""
    }
    sprintf("%s (%d lines; %d bytes%s) setting %d environment variables%s",
            prefix, nlines(f), file_size(f), extra, nvars, vars)
  } else if (type == "r") {
    sprintf("%s (%d code lines; %d bytes%s)",
            prefix, nlines(f), file_size(f), extra)
  } else {
    sprintf("%s (%d lines; %d bytes%s)",
            prefix, nlines(f), file_size(f), extra)
  }
}


parse_config_dcf <- function(pathname) {
  error_if_not(file_test("-f", pathname))
  config <- suppressWarnings(read.dcf(pathname, all = TRUE))
  config <- as.list(config)
  config <- lapply(config, FUN = function(x) {
    x <- x[!is.na(x)]
    x <- strsplit(x, split = "\n", fixed = TRUE)
    unlist(x)
  })

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
  
  config
}
