tar_fn <- function(fn) {
  lapply(as.list(body(fn))[-1], eval)
}
