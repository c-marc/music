new_xtibble <- function(x = tibble::tibble(),
                        idx_name = character(),
                        lock = logical()) {
  stopifnot(tibble::is_tibble(x))
  stopifnot(rlang::is_scalar_character(idx_name))
  stopifnot(rlang::is_scalar_logical(lock))

  # Subclass tibble
  tibble::new_tibble(x,
                     class = "m_xtibble",
                     idx_name = idx_name,
                     lock = lock
  )
}


#' Validate `xtibble`
#' @noRd
#' @param x An object of class `m_xtibble`
validate_xtibble <- function(x){
  idx_name <- attr(x, "idx_name")

  check_column(x, name = idx_name)
  check_index(x[[idx_name]])

  x
}


#' Create an instance of class `m_xtibble`
#'
#' This creates a specialized tibble (subclass of a tibble), with [index()] related features.
#' @param x
#' - For `xtibble()`: A tibble with a [index()] column.
#' - For `is_xtibble()`: An object to test.
#' @param idx_name A string, the name of a column with a [index()].
#' @param lock A logical, to lock the object and prevent unexpected transformations.
#' @return An object of class `m_xtibble`.
#' @export
#' @examples
#' tbl <- tibble::tibble(idx = index(0:2, period = 7, unit = 1), x = LETTERS[1:3])
#' xtibble(tbl, idx_name = "idx")
xtibble <- function(x,
                    idx_name,
                    lock = TRUE){
  rlang::check_required(x)
  rlang::check_required(idx_name)

  validate_xtibble(new_xtibble(x = x,
                               idx_name = idx_name,
                               lock = lock))
}


#' @export
#' @rdname xtibble
is_xtibble <- function(x) {
  inherits(x, "m_xtibble")
}


idx_name_attr <- function(x) attr(x, "idx_name")


#' Get these 3 attributes (ipu) at once to unpack with zeallot
#' @noRd
ipu_attr <- function(x){
  idx_name <- idx_name_attr(x)
  period <- period_attr(x[[idx_name]])
  unit <- unit_attr(x[[idx_name]])
  list(idx_name = idx_name, period = period, unit = unit)
}
