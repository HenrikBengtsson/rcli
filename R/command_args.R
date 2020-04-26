#' @importFrom methods as
#' @importFrom utils type.convert
command_args <- function(trailingOnly=FALSE, asValues=FALSE, defaults=NULL, always=NULL, adhoc=FALSE, unique=FALSE, excludeReserved=FALSE, excludeEnvVars=FALSE, os=NULL, .args=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  getReserved <- function(os, patterns=FALSE) {
    rVer <- getRversion()

    # General arguments
    if (rVer >= "4.0.0") {
      # According to R v4.0.0:
      reservedArgs <- c("--help", "-h", "--version", "--encoding=(.*)", "--encoding (.*)", "--save", "--no-save", "--no-environ", "--no-site-file", "--no-init-file", "--restore", "--no-restore", "--no-restore-data", "--no-restore-history", "--vanilla", "-f (.*)", "--file=(.*)", "-e (.*)", "--min-vsize=(.*)", "--max-vsize=(.*)", "--min-nsize=(.*)", "--max-nsize=(.*)", "--max-ppsize=(.*)", "--quiet", "--silent", "--no-echo", "-q", "--slave", "--verbose", "--args")
    } else if (rVer >= "2.13.0") {
      # According to R v2.13.1:
      reservedArgs <- c("--help", "-h", "--version", "--encoding=(.*)", "--encoding (.*)", "--save", "--no-save", "--no-environ", "--no-site-file", "--no-init-file", "--restore", "--no-restore", "--no-restore-data", "--no-restore-history", "--vanilla", "-f (.*)", "--file=(.*)", "-e (.*)", "--min-vsize=(.*)", "--max-vsize=(.*)", "--min-nsize=(.*)", "--max-nsize=(.*)", "--max-ppsize=(.*)", "--quiet", "--silent", "-q", "--slave", "--verbose", "--args")
    } else if (rVer >= "2.7.0") {
      # According to R v2.7.1:
      reservedArgs <- c("--help", "-h", "--version", "--encoding=(.*)", "--save", "--no-save", "--no-environ", "--no-site-file", "--no-init-file", "--restore", "--no-restore", "--no-restore-data", "--no-restore-history", "--vanilla", "-f (.*)", "--file=(.*)", "-e (.*)", "--min-vsize=(.*)", "--max-vsize=(.*)", "--min-nsize=(.*)", "--max-nsize=(.*)", "--max-ppsize=(.*)", "--quiet", "--silent", "-q", "--slave", "--interactive", "--verbose", "--args")
    } else {
      # According to R v2.0.1:
      reservedArgs <- c("--help", "-h", "--version", "--save", "--no-save", "--no-environ", "--no-site-file", "--no-init-file", "--restore", "--no-restore", "--no-restore-data", "--no-restore-history", "--vanilla", "--min-vsize=(.*)", "--max-vsize=(.*)", "--min-nsize=(.*)", "--max-nsize=(.*)", "--max-ppsize=(.*)", "--quiet", "--silent", "-q", "--slave", "--verbose", "--args")
    }

    # a) Unix (and macOS?!? /HB 2011-09-14)
    if ("unix" %in% os) {
      reservedArgs <- c(reservedArgs, "--no-readline", "--debugger-args=(.*)", "--debugger=(.*)", "-d", "--gui=(.*)", "-g", "--interactive", "--arch=(.*)")
      if (rVer >= "3.0.0") {
        # Source: R 3.0.0 NEWS (but did not appear in R --help until R 3.2.0)
        reservedArgs <- c(reservedArgs, "--min-nsize=(.*)", "--min-vsize=(.*)")
      }
    }

    # b) Macintosh
    if ("mac" %in% os) {
      # Nothing special here.
    }

    # c) Windows
    if ("windows" %in% os) {
      reservedArgs <- c(reservedArgs, "--no-Rconsole", "--ess", "--max-mem-size=(.*)")
      # Additional command-line options for RGui.exe
      reservedArgs <- c(reservedArgs, "--mdi", "--sdi", "--no-mdi", "--debug")
    }

    # If duplicates where created, remove them
    reservedArgs <- unique(reservedArgs)

    if (patterns) {
      # Create regular expression patterns out of the reserved arguments
      args <- gsub("^(-*)([-a-zA-Z]+)", "\\1(\\2)", reservedArgs)
      args <- sprintf("^%s$", args)

      reservedArgs <- list()
      # Identify the ones that has an equal sign
      idxs <- grep("=(.*)", args, fixed=TRUE)
      reservedArgs$equals <- args[idxs]
      args <- args[-idxs]

      # Identify the ones that has an extra argument
      idxs <- grep(" (.*)", args, fixed=TRUE)
      reservedArgs$pairs <- gsub(" .*", "$", args[idxs])
      args <- args[-idxs]

      # The rest are flags
      reservedArgs$flags <- args
    }

    reservedArgs
  } # getReserved()


  # Parse reserved pairs ('-<key>', '<value>') and ('--<key>', '<value>')
  # arguments into '-<key> <value>' and '--<key> <value>', respectively.
  parseReservedArgs <- function(args, os) {
    nargs <- length(args)

    reservedArgs <- getReserved(os=os, patterns=TRUE)

    # Set user arguments to start after '--args', otherwise
    # all arguments are considered user arguments
    user <- FALSE
    startU <- which(args == "--args")[1L]
    if (is.na(startU)) user <- TRUE

    argsT <- list()
    idx <- 1L
    while (idx <= nargs) {
       # A user argument?
       user <- !user && isTRUE(idx > startU)

       # Argument to be investigates
       arg <- args[idx]

       # A flag argument?
       idxT <- unlist(sapply(reservedArgs$flags, FUN=grep, arg))
       if (length(idxT) == 1L) {
         argsT[[idx]] <- list(arg=arg, user=user, reserved=!user, merged=FALSE, envvar=FALSE)
         idx <- idx + 1L
         next
       }

       # A '--<key> <value>' argument?
       idxT <- unlist(sapply(reservedArgs$pairs, FUN=grep, arg))
       if (length(idxT) == 1L) {
         arg <- c(args[idx], args[idx+1L])
         argsT[[idx]] <- list(arg=arg, user=user, reserved=!user, merged=TRUE, envvar=FALSE)
         idx <- idx + 2L
         next
       }

       # A '--<key>=<value>' argument?
       idxT <- unlist(sapply(reservedArgs$equals, FUN=grep, arg))
       if (length(idxT) == 1L) {
         pattern <- reservedArgs$equals[idxT]
         argsT[[idx]] <- list(arg=arg, user=user, reserved=!user, merged=FALSE, envvar=FALSE)
         idx <- idx + 1L
         next
       }

       # An environment variable?
       envvar <- !user && (regexpr("^([^=-]*)(=)(.*)$", arg) != -1L)
       if (envvar) {
         argsT[[idx]] <- list(arg=arg, user=FALSE, reserved=FALSE, merged=FALSE, envvar=TRUE)
         idx <- idx + 1L
         next
       }

       # Otherwise a non-reserved argument
       argsT[[idx]] <- list(arg=arg, user=user, reserved=FALSE, merged=FALSE, envvar=FALSE)

       idx <- idx + 1L
    } # while (idx <= nargs)

    argsT <- argsT[!sapply(argsT, FUN=is.null)]

    argsT
  } # parseReservedArgs()


  assertNamedList <- function(x, .name=as.character(substitute(x))) {
    # Nothing todo?
    if (length(x) == 0L) return(x)

    keys <- names(x)
    if (is.null(keys)) {
      stop(sprintf("None of the elements in '%s' are named.", .name))
    }

    if (any(nchar(keys) == 0L)) {
      stop(sprintf("Detected one or more non-named arguments in '%s' after parsing.", .name))
    }

    x
  } # assertNamedList()

  coerceAs <- function(args, types) {
    types <- types[types != "NULL"]
    todo <- which(is.element(names(args), names(types)) &
                  !sapply(args, FUN = inherits, "CmdArgExpression"))
    if (length(todo) > 0L) {
      argsT <- args[todo]
      typesT <- types[names(argsT)]
      suppressWarnings({
        for (jj in seq_along(argsT)) {
          argT <- argsT[[jj]]
          value <- as(argT, Class=typesT[jj])
          argsT[[jj]] <- value
          if (length(value) != 1L || !is.na(value)) argsT[[jj]] <- value
        }
      })
      args[todo] <- argsT
    }
    args
  } # coerceAs()



  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'defaults':
  if (asValues) {
    defaults <- as.list(defaults)
    defaults <- assertNamedList(defaults)
  } else {
    if (is.list(defaults)) {
      stop("Argument 'defaults' must not be a list when asValues=FALSE.")
    }
  }

  # Argument 'always':
  if (asValues) {
    always <- as.list(always)
    always <- assertNamedList(always)
  } else {
    if (is.list(always)) {
      stop("Argument 'always' must not be a list when asValues=FALSE.")
    }
  }

  # Argument 'os':
  if (is.null(os) || toupper(os) == "ANY") {
    os <- c("unix", "mac", "windows")
  } else if (tolower(os) == "current") {
    os <- .Platform$OS.type
  }
  os <- tolower(os)
  if (any(is.na(match(os, c("unix", "mac", "windows"))))) {
    stop("Argument 'os' contains unknown values.")
  }

  # Argument '.args':
  if (is.null(.args)) {
    .args <- base::commandArgs(trailingOnly=trailingOnly)
    ## Also support 'littler' (https::cran.r-project.org/package=littler)
    ## command-line options 'argv' character vector.  If it exists, then
    ## append it to the above vector of arguments.
    if (exists("argv", mode='character', envir=globalenv())) {
      argv <- get("argv", mode='character', envir=globalenv())
      .args <- c(.args, argv)
    }
  } else if (!is.character(.args)) {
    stop("Argument '.args' must be a character vector: ", class(.args)[1L])
  }

  args <- .args


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (1) Parse into user, paired, reserved arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  argsT <- parseReservedArgs(args, os=os)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (2) Identify which arguments not to drop
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  keep <- unlist(lapply(argsT, FUN=function(arg) {
     !(excludeReserved && arg$reserved) && !(excludeEnvVars && arg$envvar)
  }))
  argsT <- argsT[keep]


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # (3) Coerce arguments to a named list?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (asValues) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (a) Parse key-value pairs
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # An argument name cannot start with a hypen ('-').
    keyPattern <- "[[:alnum:]_.][[:alnum:]_.-]*"
    nargsT <- length(argsT)
    for (ii in seq_len(nargsT)) {
      argI <- argsT[[ii]]
      arg <- argI$arg
#      cat(sprintf("Argument #%d: '%s' [n=%d]\n", ii, arg, length(arg)))

      if (length(arg) == 2L) {
        argsT[[ii]]$key <- gsub("^[-]*", "", arg[1L])
        argsT[[ii]]$value <- arg[2L]
#        utils::str(list("1", argsT_ii = argsT[[ii]]))
        next
      }

      # Sanity check
      stop_if_not(length(arg) == 1L)

      # --<key>(=|:=)<value>
      pattern <- sprintf("^--(%s)(=|:=)(.*)$", keyPattern)
      if (regexpr(pattern, arg) != -1L) {
        key <- gsub(pattern, "\\1", arg)
        what <- gsub(pattern, "\\2", arg)
        value <- gsub(pattern, "\\3", arg)
        if (what == ":=") class(value) <- c("CmdArgExpression")
        argsT[[ii]]$key <- key
        argsT[[ii]]$value <- value
#        utils::str(list("2", argsT_ii = argsT[[ii]]))
        next
      }

      # --<key>
      pattern <- sprintf("^--(%s)$", keyPattern)
      if (regexpr(pattern, arg) != -1L) {
        key <- gsub(pattern, "\\1", arg)
        argsT[[ii]]$key <- key
#        utils::str(list("3", argsT_ii = argsT[[ii]]))
        next
      }

      # -<key>(=|:=)<value>
      pattern <- sprintf("^-(%s)(=|:=)(.*)$", keyPattern)
      if (regexpr(pattern, arg) != -1L) {
        key <- gsub(pattern, "\\1", arg)
        what <- gsub(pattern, "\\2", arg)
        value <- gsub(pattern, "\\3", arg)
        if (what == ":=") class(value) <- c("CmdArgExpression")
        argsT[[ii]]$key <- key
        argsT[[ii]]$value <- value
#        utils::str(list("4", argsT_ii = argsT[[ii]]))
        next
      }

      # -<key>
      pattern <- sprintf("^-(%s)$", keyPattern)
      if (regexpr(pattern, arg) != -1L) {
        key <- gsub(pattern, "\\1", arg)
        argsT[[ii]]$key <- key
#        utils::str(list("5", argsT_ii = argsT[[ii]]))
        next
      }

      # <key>(=|:=)<value>
      pattern <- sprintf("^(%s)(=|:=)(.*)$", keyPattern)
      if (regexpr(pattern, arg) != -1L) {
        key <- gsub(pattern, "\\1", arg)
        what <- gsub(pattern, "\\2", arg)
        value <- gsub(pattern, "\\3", arg)
        if (what == ":=") class(value) <- c("CmdArgExpression")
        argsT[[ii]]$key <- key
        argsT[[ii]]$value <- value
#        utils::str(list("6", argsT_ii = argsT[[ii]]))
        next
      }

      argsT[[ii]]$value <- arg
    } # for (ii ...)

#    utils::str(list(argsT = argsT))


    # Rescue missing values
    if (nargsT > 1L) {
      for (ii in 1:(nargsT-1L)) {
        if (length(argsT[[ii]]) == 0L)
          next

        key <- argsT[[ii]]$key
        value <- argsT[[ii]]$value

        # No missing value?
        if (!is.null(value)) {
           ## This is what makes "R" into R=NA. Is that what we want? /HB 2014-01-26
           if (is.null(key)) {
              argsT[[ii]]$key <- value
              argsT[[ii]]$value <- NA_character_
#           utils::str(list("6", argsT_ii = argsT[[ii]]))
           }
           next
        }

        # Missing value - can we rescue it?
        if (!is.null(key) && grepl("^--", key)) {
          nextKey <- argsT[[ii+1L]]$key
          nextValue <- argsT[[ii+1L]]$value
          if (is.null(nextKey)) {
             # Definitely!
             argsT[[ii]]$value <- nextValue
             argsT[[ii+1L]] <- list(); # Drop next
#             utils::str(list("7", argsT_ii = argsT[[ii]]))
             next
          }
        }

        # Otherwise, interpret as a flag
        argsT[[ii]]$value <- TRUE
#           utils::str(list("8", argsT_ii = argsT[[ii]]))
      } # for (ii ...)

      # Special case: Rescue missing value in argsT[[<last>]]?
      argT <- argsT[[nargsT]]
      if (length(argT) > 0L && is.null(argT$value)) {
        argsT[[nargsT]]$value <- TRUE
      }

#    utils::str(list("B", argsT = argsT))

      # Drop empty
      keep <- (sapply(argsT, FUN=length) > 0L)
      argsT <- argsT[keep]
      nargsT <- length(argsT)
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (b) Revert list(a="1", key=NA) to list(a="1", "key")
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    for (ii in seq_along(argsT)) {
      if (identical(argsT[[ii]]$value, NA_character_)) {
        argsT[[ii]]$value <- argsT[[ii]]$key
        argsT[[ii]]$key <- ""
      }
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (c) Make sure everything has a key
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    for (ii in seq_along(argsT)) {
      if (is.null(argsT[[ii]]$key)) {
        argsT[[ii]]$key <- ""
      }
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (d) Coerce to key=value list
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    keys <- unlist(lapply(argsT, FUN=function(x) x$key))
    args <- lapply(argsT, FUN=function(x) x$value)
    names(args) <- keys

    argsT <- NULL; # Not needed anymore

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (e) Coerce arguments to known data types?
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(args) > 0L && length(defaults) + length(always) > 0L) {
      # First to the 'always', then remaining to the 'defaults'.
      types <- sapply(c(defaults, always), FUN=storage.mode)
      keep <- !duplicated(names(types), fromLast=TRUE)
      types <- types[keep]
      args <- coerceAs(args, types=types)
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (f) Ad hoc coercion of numerics?
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (adhoc && length(args) > 0L) {
      modes <- sapply(args, FUN=storage.mode)
      idxs <- which(modes == "character")
      if (length(idxs) > 0L) {
        argsT <- args[idxs]
        # Try to coerce / evaluate...
        for (kk in seq_along(argsT)) {
          arg <- argsT[[kk]]
          # (a) Try to evaluate expression using eval(parse(...))
          if (inherits(arg, "CmdArgExpression")) {
            value <- tryCatch({
              expr <- parse(text=arg)
              value <- eval(expr, envir=globalenv())
            }, error=function(ex) {
              value <- arg
              class(value) <- c("FailedCmdArgExpression", class(value))
              value
            })
            argsT[kk] <- list(value); ## Also NULL
            next
          }

          # (b) Don't coerce 'T' and 'F' to logical
          if (is.element(arg, c("T", "F"))) next

          # (c) Try to coerce to "logical, integer, numeric, complex
          # or factor as appropriate." using utils::type.convert()
          tryCatch({
            value <- type.convert(arg, as.is=TRUE)
            argsT[[kk]] <- value
          }, error=function(ex) {})
        }
        args[idxs] <- argsT
      }
    } # if (adhoc)

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (g) Coerce arguments to known data types?
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(args) > 0L && length(defaults) + length(always) > 0L) {
      # First to the 'always', then remaining to the 'defaults'.
      types <- sapply(c(defaults, always), FUN=storage.mode)
      keep <- !duplicated(names(types), fromLast=TRUE)
      types <- types[keep]
      args <- coerceAs(args, types=types)
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (h) Prepend defaults, if not already specified
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(defaults) > 0L) {
      # Any missing?
      idxs <- which(!is.element(names(defaults), names(args)))
      if (length(idxs) > 0L) {
        args <- c(args[1L], defaults[idxs], args[-1L])
      }
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (i) Override by/append 'always' arguments?
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(always) > 0L) {
      args <- c(args, always)
    }

    # Keep only unique arguments?
    if (unique && length(args) > 1L) {
      # Keep only those with unique names
      keep <- !duplicated(names(args), fromLast=TRUE)
      # ...and those without names
      keep <- keep | !nzchar(names(args))
      args <- args[keep]
    }
  } else { # if (asValue)
    args <- unlist(lapply(argsT, FUN=function(x) x$arg))
    argsT <- NULL; # Not needed anymore

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (a) Prepend defaults, if not already specified
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(defaults) > 0L) {
      # Any missing?
      idxs <- which(!is.element(defaults, args))
      if (length(idxs) > 0L) {
        args <- c(args[1L], defaults[idxs], args[-1L])
      }
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # (b) Append 'always' argument, if not already specified
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(always) > 0L) {
      args <- c(args, setdiff(always, args))
    }

    # Keep only unique arguments?
    if (unique && length(args) > 0L) {
      keep <- !duplicated(args, fromLast=TRUE)
      args <- args[keep]
    }
  } # if (asValues)

  args
}
