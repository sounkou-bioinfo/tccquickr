# tccq_extlib.R - external C library descriptors for later milestones
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_external_library <- function(
  name,
  headers = character(),
  libraries = character(),
  include_paths = character(),
  library_paths = character(),
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
      symbols = symbols,
      effects = effects
    ),
    class = "tccq_external_library"
  )
}

tccq_context_from_extlibs <- function(extlibs = list()) {
  if (!length(extlibs)) {
    return(list(headers = character(), libraries = character(), external_symbols = list()))
  }

  headers <- character()
  libraries <- character()
  symbols <- list()

  for (lib in extlibs) {
    if (!inherits(lib, "tccq_external_library")) {
      tccq_abort("expected tccq_external_library")
    }
    headers <- c(headers, lib$headers)
    libraries <- c(libraries, lib$libraries)
    symbols <- modifyList(symbols, lib$symbols)
  }

  list(
    headers = tccq_unique(headers),
    libraries = tccq_unique(libraries),
    external_symbols = symbols
  )
}
