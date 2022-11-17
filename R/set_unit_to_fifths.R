#' Set unit of an index so that it gets ordered by fifths
#'
#' Transform the index so that its unit equals the one that corresponds to a fifth interval.
#' Values are projected between `-period %/% 2` and `period %/% 2` with the exception of 12-TET where values are projected between -5 and 6.
#' This wraps [set_unit()].
#' @param x An object.
#' @return The object with a modified index.
#' @export
#' @examples
#' index(0:11, period = 12L, unit = 1L) |> set_unit_to_fifths()
#' start_with_mode() |> as_12_TET() |> set_unit_to_fifths()
set_unit_to_fifths <- function(x){
  UseMethod("set_unit_to_fifths", x)
}


#' @rdname set_unit_to_fifths
#' @export
set_unit_to_fifths.m_index <- function(x){
  period <- period_attr(x)
  set_unit(x, to = fifth(period))
}


#' @rdname set_unit_to_fifths
#' @export
set_unit_to_fifths.m_xtibble <- function(x){
  idx_name <- period <- unit <- NULL
  c(idx_name, period, unit) %<-% ipu_attr(x)

  x <- x |>
    set_lock(to = FALSE, silently = TRUE) |>
    set_unit(to = fifth(period)) |>
    set_lock(to = TRUE, silently = TRUE)

  sel <- switch(as.character(period),
                "31" = -15:15,
                "19" = -9:9,
                "12" = -5:6,
                "7" = -3:3)

  periodic_slice(x, sel)
}
