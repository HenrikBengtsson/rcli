parse_check_as_option <- function(name, args = character(0L), ...) {
  logs("parse_check_as_option() ...")
  on.exit(logs("parse_check_as_option() ... done"), add = TRUE)
  
  register_by_packages(name)
  db <- registry("as")

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
  logf("- Calling custom 'as' function: %s", sQuote(name))
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
