#' Set unit of an index to one
#'
#' Transform the index so that its unit equals one. Values are projected between `0` and `period-1`.
#' This wraps [set_unit()].
#' @param x An object.
#' @return The object with a modified index.
#' @export
#' @examples
#' index(-1:1, period = 12L, unit = 7L) |> set_unit_to_1()
#' start_with_some_notes(c("C","E","G")) |> set_unit_to_1()
set_unit_to_1 <- function(x){
  UseMethod("set_unit_to_1", x)
}


#' @rdname set_unit_to_1
#' @export
set_unit_to_1.m_index <- function(x){
  set_unit(x, to = 1L)
}


#' @rdname set_unit_to_1
#' @export
set_unit_to_1.m_xtibble <- function(x){
  idx_name <- idx_name_attr(x)
  x |>
    set_lock(to = FALSE, silently = TRUE) |>
    set_unit(to = 1L) |>
    set_lock(to = TRUE, silently = TRUE) |>
    dplyr::arrange(.data[[idx_name]])
}
