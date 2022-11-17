#' Collapse all 31 labels
#' @noRd
collapse_all <- function(period) {
  get_roles() |>
    dplyr::mutate(idx = vec_data(.data$idx) %% period,
                  secondary = abs(.data$idx) > (period/2)) |>
    dplyr::group_by(.data$idx) |>
    dplyr::summarise(merge_flag = length(.data$idx) > 1,
                     role = stringr::str_flatten(.data$role[order(.data$secondary)], collapse = "~")) |>
    dplyr::mutate(idx = index(.data$idx, period = period, unit = fifth(period))) |>
    xtibble(idx_name = "idx") # unclassed by summarise ?
}


#' Only relabel the most subtle roles
#' @noRd
collapse_simplify <- function(period) {
  get_roles() |>
    dplyr::mutate(idx = vec_data(.data$idx)) |>
    dplyr::filter(abs(.data$idx) <= (period/2)) |>
    dplyr::mutate(idx = .data$idx %% period) |>
    # deal with b5/A4 c(0:11, 11)
    dplyr::group_by(.data$idx) |>
    dplyr::summarise(merge_flag = length(.data$idx) > 1,
                     role = stringr::str_flatten(.data$role, collapse = "~")) |>
    dplyr::mutate(idx = index(.data$idx, period = period, unit = fifth(period))) |>
    xtibble(idx_name = "idx") # unclassed by summarise ?
}


#' Collapse roles into a lower period
#'
#' @description
#' Construct new labels for 19 or 12-EDO. Two style are proposed:
#' - "all": relabel all merge.
#' - "simpler": only relabel the more subtle roles to emphasize the merge,
#'   but keep the merge implicit elsewhere.
#' @export
#' @param period An integer, the new EDO system. 19 or 12.
#' @param style A string, either "full" or "simpler"
#' @returns An [xtibble()] object with a new period and new labels for the roles.
#' @examples
#' collapse_roles(period = 12, style = "all")
#' collapse_roles(period = 12, style = "simpler")
collapse_roles <- function(period = 12, style = c("all", "simpler")){
  check_period(period)
  if (period == 31) {
    return(get_roles())
  }

  rlang::arg_match(style)

  if (style == "all") {
    xtbl <- collapse_all(period = period)
  } else {
    xtbl <- collapse_simplify(period = period)
  }

  sel <- switch(as.character(period),
                "19" = -9:9,
                "12" = -6:6) #duplicate on purpose

  periodic_slice(xtbl, sel)
}
