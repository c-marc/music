#' Add roles to an `xtibble` object
#'
#' Add a column with roles to an [xtibble()] object.
#' Its index must be ordered as fifths.
#' If the period
#' @export
#' @param x An [xtibble()] with [index()] valid attributes.
#' @param style A string. The style for roles label (when `period < 31`):
#' - "keep" (default): use 31-TET roles.
#' - "collapse_all": all merged roles are renamed.
#' - "collapse_simpler": only discarded roles are renamed.
#' @param ... Further arguments passed to or from other methods.
#' @returns The [xtibble()] object with an additional (or updated) column `role`.
#' @family add_
#' @examples
#' start_with_mode(degree = 7) |> add_roles()
#' start_with_mode(degree = 7) |> add_roles(style = "collapse_simpler")
add_roles <- function(x, ...){
  UseMethod("add_roles", x)
}


#' @rdname add_roles
#' @export
add_roles.m_xtibble <- function(x, style = "keep", ...){
  # Required attributes
  check_fifths(x)
  rlang::arg_match(style, c("keep", "collapse_all", "collapse_simpler"))

  idx_name <- period <- unit <- NULL
  c(idx_name, period, unit) %<-% ipu_attr(x)

  xtbl_default <- get_roles() |>
    dplyr::mutate(idx = index(.data$idx, period = period, unit = fifth(period)))

  xtbl_roles <- switch(style,
                       "keep" = xtbl_default,
                       "collapse_all" = collapse_roles(period = period, style = "all"),
                       "collapse_simpler" = collapse_roles(period = period, style = "simpler"))

  xtbl_roles <- dplyr::rename(xtbl_roles, "{idx_name}" := "idx")

  x |>
    # Delete if it exists
    dplyr::select(!tidyselect::any_of("role")) |>
    dplyr::left_join(xtbl_roles, by = idx_name)
}
