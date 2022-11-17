#' Set first of the set
#'
#' @description
#' Transform the index so that the `i_th` row (when the index is positive) become the first one (with negative lowest value then).
#' This takes advantage of the periodic feature of the index.
#' More pragmatically, this computes:
#'   * any _inversion_ of a chord (first inversion with `rank = 2` and so on...);
#'   * any mode of a scale.
#' @param x An [xtibble()] with [index()] set to unit 1.
#' @param rank An integer. The rank to set first (to set to lowest index value).
#' @return The object with modified index and re-arranged accordingly.
#' @export
#' @examples
#' c("F", "C", "G") |> start_with_some_notes() |> as_12_TET() |> set_first(rank = 2L)
#' start_with_mode() |> add_notes() |> set_unit_to_1() |> set_first(rank = 2L)
set_first <- function(x, rank){
  UseMethod("set_first", x)
}


#' @rdname set_first
#' @export
set_first.m_xtibble <- function(x, rank = 1L){
  check_attr(x, unit = 1L)
  check_scalar_integerish(rank)
  if (!dplyr::between(rank, 1, nrow(x))) {
    cli::cli_abort(c(
      "Invalid rank",
      "!" = "{.arg rank} must be a valid rank for {.arg x}.",
      "i" = "Valid ranks for {.arg x} are integers between 1 and {nrow(x)}."
    ))
  }

  idx_name <- period <- unit <- NULL
  c(idx_name, period, unit) %<-% ipu_attr(x)

  idx_reset <- sort(vec_data(x[[idx_name]]) %% period)
  sel <- (0:(period - 1)) + idx_reset[rank]

  periodic_slice(x, sel)
}
