parse_check_BiocCheck <- function(args, stdin) {
  ## Assert that the 'BiocCheck' package is installed
  pkg <- "BiocCheck"
  res <- requireNamespace(pkg, quietly = TRUE)
  if (!res) error("Failed to load the '%s' package", pkg)

  if (isTRUE(args$help)) {
    args <- list()
    code <- c("BiocCheck::usage()", "quit(save = 'no', status = 0L)")
  } else {
    tarball <- cmd_args_tarball(args)
    args <- c(list(package = tarball), args)
    code <- "do.call(BiocCheck::BiocCheck, args=args)"
  }
  stdin <- code

  res <- list(args = args, stdin = stdin)

  res
}
