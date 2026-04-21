# tccquickr - experimental lowering on top of Rtinycc
# Copyright (C) 2025-2026 Sounkou Mahamane Toure
# SPDX-License-Identifier: GPL-3.0-or-later

#' tccquickr package
#'
#' Experimental compiler front-end and transformation framework built on top of
#' `Rtinycc`.
#'
#' @import Rtinycc
#' @keywords internal
#' @name tccquickr-package
NULL

.RtinyccCall <- base::.Call

`%||%` <- function(x, y) if (is.null(x)) y else x
