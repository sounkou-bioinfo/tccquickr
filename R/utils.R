.tccq_check_character_scalar <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    tccq_abort(
      "schema.invalid_character_scalar",
      sprintf("`%s` must be a single non-empty string.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
}

.tccq_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    tccq_abort(
      "schema.invalid_logical_scalar",
      sprintf("`%s` must be a single TRUE/FALSE value.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
}

.tccq_check_s7 <- function(x, class, label, arg) {
  if (!S7::S7_inherits(x, class)) {
    tccq_abort(
      "schema.invalid_s7_class",
      sprintf("`%s` must be <%s>.", arg, label),
      phase = "schema",
      path = arg,
      data = list(expected = label, actual = class(x))
    )
  }
}

.tccq_check_list_of <- function(x, class, label, arg) {
  if (!is.list(x)) {
    tccq_abort(
      "schema.invalid_list",
      sprintf("`%s` must be a list of <%s> values.", arg, label),
      phase = "schema",
      path = arg,
      data = list(expected = label, actual = typeof(x))
    )
  }
  for (i in seq_along(x)) {
    .tccq_check_s7(x[[i]], class, label, sprintf("%s[[%d]]", arg, i))
  }
}
