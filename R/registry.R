registry <- local({
  dbs <- list(
    as = list()
  )
  
  function(what = "as", ...) {
    what <- match.arg(what)
    args <- list(...)

    db <- dbs[[what]]
    
    ## List registered check "as" functions
    if (length(args) == 0L) return(db)

    if (length(args) != 1L) error("Maximum one argument can be specified")

    arg <- args[[1]]
    name <- names(args)[1]
    if (is.character(arg)) {
      res <- db[[arg]]
      if (is.null(res)) error("such %s function: %s", sQuote(what), sQuote(arg))
      return(res)
    } else if (is.function(arg)) {
      if (is.null(name)) error("The function must be named")
      db[[name]] <- arg
      dbs[[what]] <<- db
      return(invisible(db))
    } else {
      error("Unknown type of argument: %s", sQuote(typeof(arg)))
    }
  }
})


register_by_packages <- function(names) {
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
}

register_as <- function(...) {
  registry(what = "as", ...)
}

## Backward compatibility
check_as_functions <- register_as
