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

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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

.tccq_check_list <- function(x, arg) {
  if (!is.list(x)) {
    tccq_abort(
      "schema.invalid_list",
      sprintf("`%s` must be a list.", arg),
      phase = "schema",
      path = arg,
      data = list(actual = typeof(x))
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

.tccq_check_character_or_empty <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    tccq_abort(
      "schema.invalid_character_scalar",
      sprintf("`%s` must be a single string.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
}

.tccq_check_character_set <- function(x, choices, arg, allow_empty = FALSE) {
  if (
    !is.character(x) ||
      (!allow_empty && length(x) == 0L) ||
      anyNA(x) ||
      any(!nzchar(x))
  ) {
    tccq_abort(
      "schema.invalid_character_set",
      sprintf("`%s` must be a character vector of supported values.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
  unsupported <- setdiff(unique(x), choices)
  if (length(unsupported) > 0L) {
    tccq_abort(
      "schema.unsupported_character_set_value",
      sprintf("`%s` contains unsupported values.", arg),
      phase = "schema",
      path = arg,
      data = list(value = unsupported, supported = choices)
    )
  }
}

.tccq_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1L || x != as.integer(x)) {
    tccq_abort(
      "schema.invalid_positive_integer",
      sprintf("`%s` must be a positive integer.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
  as.integer(x)
}

.tccq_check_optional_positive_integer <- function(x, arg) {
  if (is.integer(x) && length(x) == 1L && is.na(x)) {
    return(NA_integer_)
  }
  .tccq_check_positive_integer(x, arg)
}

.tccq_check_optional_nonnegative_integer <- function(x, arg) {
  if (is.integer(x) && length(x) == 1L && is.na(x)) {
    return(NA_integer_)
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0L || x != as.integer(x)) {
    tccq_abort(
      "schema.invalid_nonnegative_integer",
      sprintf("`%s` must be a non-negative integer or NA.", arg),
      phase = "schema",
      path = arg,
      data = list(value = x)
    )
  }
  as.integer(x)
}

.tccq_check_memory_space <- function(memory_space, arg) {
  .tccq_check_character_scalar(memory_space, arg)
  if (!memory_space %in% TCCQ_MEMORY_SPACES) {
    tccq_abort(
      "schema.invalid_memory_space",
      sprintf("`%s` is not a supported memory space.", arg),
      phase = "schema",
      path = arg,
      data = list(memory_space = memory_space, supported = TCCQ_MEMORY_SPACES)
    )
  }
}
