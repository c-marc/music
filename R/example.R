#' Construct tiny examples of objects of class `m_index` or `m_xtibble`
#'
#' @description
#' Construct tiny examples of [index()] and [xtibble()] objects.
#' - For concise clearer examples in documented function.
#' - For the user to get familiar with some fonctions.
#' @param class A string. Either "index" or "xtibble".
#' @param period An integer. Default is `4L`.
#' @param unit An integer. Default is `1L`.
#' @param lock A logical. Default is `FALSE`.
#' @return An object of the required class
#' @export
#' @examples
#' example("index")
#' example("xtibble", period = 7L)
example <- function(class = NULL, period = 4L, unit = 1L, lock = FALSE){
  choices <- c("index", "xtibble")
  if (is.null(class)) {
    cli::cli_inform(c(
            "Set argument {.arg class}",
      "i" = "Choose between available choices}"
    ))
    return(choices)
  }

  rlang::arg_match(class, values = choices)

  idx <- index(0:(period - 1), period = period, unit = unit)

  if (class == "index") {
    return(idx)
  }

  xtibble(tibble::tibble(idx = idx, name = paste0("x", as.integer(idx))),
                 idx_name = "idx",
                 lock = lock)
}
