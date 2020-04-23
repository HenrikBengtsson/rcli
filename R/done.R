done <- function(status = 0L) {
  action <- getOption("rcli.done.action", "exit")
  if (action == "exit") quit(save = "no", status = status)
  if (action == "error") stop("rcli::done()", call. = FALSE)
  invisible(status)
}
