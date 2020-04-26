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
  
  ## empty stdin() to prevent further processsing
  readLines(stdin())
  
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


file_info <- function(f, type = "txt") {
  if (!nzchar(f)) return(sQuote(""))
  fx <- path.expand(f)
  if (!is_file(f)) {
    return(sprintf("non-existing file", sQuote(f)))
  }

  if (type == "binary") {
    sprintf("binary file; %d bytes", file_size(f))
  } else if (type == "env") {
    vars <- names(parse_renviron(f))
    nvars <- length(vars)
    if (nvars > 0) {
      vars <- paste(sQuote(vars), collapse = ", ")
    } else {
      vars <- ""
    }
    sprintf("%d lines; %d bytes setting %d environment variables: %s",
            nlines(f), file_size(f), nvars, vars)
  } else if (type == "r") {
    sprintf("%d code lines; %d bytes", nlines(f), file_size(f))
  } else {
    sprintf("%d lines; %d bytes", nlines(f), file_size(f))
  }
}


eof_ok <- function(file) {
  size <- file.info(file)$size
  ## On Windows, symbolic links give size = 0
  if (.Platform$OS.type == "windows" && size == 0L) size <- 1e9
  bfr <- readBin(file, what = "raw", n = size)
  n <- length(bfr)
  if (n == 0L) return(FALSE)
  is.element(bfr[n], charToRaw("\n\r"))
}
