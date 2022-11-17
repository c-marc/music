#' Test if the index is circular
#'
#' Test if the range of values is not superior to the `period` attribute.
#' This means the index does not extend over multiple periods and modulo can be applied without colliding values.
#' @param x A `m_index` object.
#' @return A logical.
#' @noRd
#' @examples
#' index(0:11, period = 6) |> is_circular() # FALSE
is_circular <- function(x){
  check_index(x)
  as.logical( diff(range(vec_data(x), na.rm = TRUE)) < period_attr(x) )
}


#' Check if circular and abort nicely otherwise
#' @return The object invisibly
#' @noRd
check_circular <- function(x) {
  if (is_circular(x)) {
    return(invisible(x))
  }

  cli::cli_abort(c(
    "Invalid operation",
    "!" = "Values of the {.cls m_index} object must be circular for this operation.",
    "i" = "The values of {.arg x} extend over the {.field period} attribute."
  ))
}
