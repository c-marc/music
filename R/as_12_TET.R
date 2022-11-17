#' Set the attributes of an index so that it matches 12-TET
#'
#' Set the period to 12 and the unit to 1.
#' @export
#' @inheritParams set_period
#' @returns An object with modified index.
#' @examples
#' index(-1:1, period = 31L, unit = 18L) |> as_12_TET()
#' start_with_some_roles(c("P1", "d5", "A6")) |> as_12_TET()
as_12_TET <- function(x, ...){
  UseMethod("as_12_TET", x)
}


#' @rdname as_12_TET
#' @export
as_12_TET.m_index <- function(x, ...){
  x |>
    set_period(to = 12L, merge = FALSE) |>
    set_unit_to_1()
}


#' @rdname as_12_TET
#' @export
as_12_TET.m_xtibble <- function(x, ...){
  x |>
    # Never merge here because set_unit() will skip the opportunity to add_notes/roles()
    set_period(to = 12L, merge = FALSE) |>
    set_unit_to_1()
}
