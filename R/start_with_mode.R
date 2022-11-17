#' Start with a mode
#'
#' @description
#' Start with one of 12 available modes:
#'   * 7 heptatonic modes;
#'   * 5 pentatonic modes.
#' Currently degrees for pentatonic modes are numbered to match heptatonic ones.
#' The five available degrees are therefore 1, 2, 3, 5 and 6 (and not 1:5).
#' @param mode_size An integer, 7 (default) or 5, the required family of modes.
#' @param degree An integer, the degree from the family of modes.
#' @returns An [xtibble()] object with column `ìdx` (period = 12; unit = 7), locked.
#' @family mode
#' @family start_with_
#' @export
#' @examples
#' start_with_mode(mode_size = 7L, degree = 2L)
start_with_mode <- function(mode_size = 7L, degree = 1L){
  xtbl <- get_mode(mode_size = mode_size)

  degree <- check_scalar_integerish(degree)

  if (!degree %in% xtbl[["degree"]]) {
    cli::cli_abort(c(
      "Invalid degree",
      "!" = "{.arg degree} must be compatible with the mode family.",
      "i" = "{.arg degree} are {.val {xtbl[['degree']]}}.",
      "i" = "See {.code ?start_with_mode} for more."
    ))
  }

  xtbl |>
    set_lock(to = FALSE, silently = TRUE) |>
    set_zero(on = degree, from = "degree") |>
    set_lock(to = TRUE, silently = TRUE) |>
    dplyr::select("idx")
}


#' Show mode
#'
#' @inheritParams start_with_mode
#' @export
#' @family mode
#' @examples
#' show_mode(mode_size = 7L)
show_mode <- function(mode_size = 7L){
  xtbl <- get_mode(mode_size = mode_size)

  xtbl |>
    dplyr::select("degree", "name") |>
    dplyr::arrange(.data$degree)
}
