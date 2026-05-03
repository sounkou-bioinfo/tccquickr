# tccq_extlib.R - external C library descriptors for later milestones
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_external_library <- function(
  name,
  headers = character(),
  libraries = character(),
  include_paths = character(),
  library_paths = character(),
  options = character(),
  symbols = list(),
  effects = list()
) {
  structure(
    list(
      name = name,
      headers = headers,
      libraries = libraries,
      include_paths = include_paths,
      library_paths = library_paths,
      options = options,
      symbols = symbols,
      effects = effects
    ),
    class = "tccq_external_library"
  )
}

tccq_merge_contexts <- function(x = list(), y = list()) {
  x <- x %||% list()
  y <- y %||% list()

  tccq_validate_compile_context(list(
    headers = tccq_unique(c(x$headers %||% character(), y$headers %||% character())),
    libraries = tccq_unique(c(x$libraries %||% character(), y$libraries %||% character())),
    include_paths = tccq_unique(c(x$include_paths %||% character(), y$include_paths %||% character())),
    library_paths = tccq_unique(c(x$library_paths %||% character(), y$library_paths %||% character())),
    options = tccq_unique(c(x$options %||% character(), y$options %||% character())),
    external_symbols = utils::modifyList(x$external_symbols %||% list(), y$external_symbols %||% list()),
    external_effects = utils::modifyList(x$external_effects %||% list(), y$external_effects %||% list())
  ))
}

tccq_validate_context_character <- function(x, field) {
  if (is.null(x)) {
    return(character())
  }
  if (!is.character(x)) {
    tccq_abort("compile context field '", field, "' must be character")
  }
  x
}

tccq_validate_context_safe_lines <- function(x, field, allow_shell_chars = FALSE) {
  x <- tccq_validate_context_character(x, field)
  if (!length(x)) {
    return(x)
  }

  bad_line <- grepl("[\r\n]", x)
  if (any(bad_line)) {
    tccq_abort("unsafe compile context field '", field, "': embedded newlines are not allowed")
  }

  if (!isTRUE(allow_shell_chars)) {
    bad_shell <- grepl("[[:space:];&|`$<>]", x)
    if (any(bad_shell)) {
      tccq_abort("unsafe compile context field '", field, "': shell metacharacters or whitespace are not allowed")
    }
  }

  x
}

tccq_validate_compile_context <- function(ctx = list()) {
  ctx <- ctx %||% list()
  ctx$headers <- tccq_validate_context_safe_lines(ctx$headers %||% character(), "headers", allow_shell_chars = TRUE)
  ctx$libraries <- tccq_validate_context_safe_lines(ctx$libraries %||% character(), "libraries")
  ctx$include_paths <- tccq_validate_context_safe_lines(ctx$include_paths %||% character(), "include_paths")
  ctx$library_paths <- tccq_validate_context_safe_lines(ctx$library_paths %||% character(), "library_paths")
  ctx$options <- tccq_validate_context_safe_lines(ctx$options %||% character(), "options")
  ctx$external_symbols <- ctx$external_symbols %||% list()
  ctx$external_effects <- ctx$external_effects %||% list()
  ctx
}

tccq_context_from_extlibs <- function(extlibs = list()) {
  if (!length(extlibs)) {
    return(list(
      headers = character(),
      libraries = character(),
      include_paths = character(),
      library_paths = character(),
      options = character(),
      external_symbols = list(),
      external_effects = list()
    ))
  }

  headers <- character()
  libraries <- character()
  include_paths <- character()
  library_paths <- character()
  options <- character()
  symbols <- list()
  effects <- list()

  for (lib in extlibs) {
    if (!inherits(lib, "tccq_external_library")) {
      tccq_abort("expected tccq_external_library")
    }
    headers <- c(headers, lib$headers)
    libraries <- c(libraries, lib$libraries)
    include_paths <- c(include_paths, lib$include_paths)
    library_paths <- c(library_paths, lib$library_paths)
    options <- c(options, lib$options %||% character())
    symbols <- utils::modifyList(symbols, lib$symbols)
    effects <- utils::modifyList(effects, lib$effects %||% list())
  }

  tccq_validate_compile_context(list(
    headers = tccq_unique(headers),
    libraries = tccq_unique(libraries),
    include_paths = tccq_unique(include_paths),
    library_paths = tccq_unique(library_paths),
    options = tccq_unique(options),
    external_symbols = symbols,
    external_effects = effects
  ))
}
