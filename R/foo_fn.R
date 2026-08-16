foo_fn <- function(...) {
  print("...names _________________________________________________")
  print(...names())

  print("enquos _________________________________________________")
  print(rlang::enquos(...))
  print(rlang::enquos(...) |> str())

  print("wait _________________________________________________")
  print(rlang::enquos(...)[[1]] |> rlang::eval_tidy())
  print(rlang::enquos(...)[[1]] |> str())

  print(rlang::quo_get_expr(rlang::enquos(...)[[1]]) |> as.character())
}

# foo_tar
# foo_fn(foo_tar, "sdf")

# https://rlang.r-lib.org/reference/topic-quosure.html
# https://adv-r.hadley.nz/evaluation.html
