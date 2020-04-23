library(rcli)

Sys.setenv(R_CMD = TRUE)
options(
  rcli.debug.stdin = "tools:::.check_packages()",
  rcli.done.action  = "return"
)

bfr0 <- utils::capture.output({
  rcli:::rcli_test_help()
})
cat(bfr0, sep = "\n")

args <- c("--as=rcli-test", "--help")
bfr <- utils::capture.output({
  res <- r_cmd_call(args = args, unload = FALSE, debug = TRUE)
})
print(res)
cat(bfr, sep = "\n")
stopifnot(all(bfr == bfr0))

options(
  rcli.debug.stdin = NULL,
  rcli.done.action = NULL
)
