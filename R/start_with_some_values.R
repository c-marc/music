#' Start with a set of chosen values (of an index)
#'
#' Create an [xtibble()] object from values of an index.
#' Higher order helper to construct an [xtibble()] object and its index with a shorter call.
#' @param x A vector of strings. The set of roles.
#' @param period An integer, the period of the [index()] (default 31L).
#' @param unit An integer, the unit of the [index()] (default 18L).
#' @param idx_name A string, the column name for the index (default "idx").
#' @return An [xtibble()] with one column.
#' @export
#' @family start_with_
#' @examples
#' # Ionian mode
#' start_with_some_values((0:6) - 1)
start_with_some_values <- function(x, period = 31L, unit = 18L, idx_name = "idx"){
  rlang::check_required(x)
  # Validation is delegated

  tbl <- tibble::tibble("{idx_name}" := index(x, period = period, unit = unit))
  xtibble(tbl, idx_name = idx_name)
}
