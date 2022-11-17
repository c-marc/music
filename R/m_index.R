new_index <- function(x = integer(), period = 1L, unit = 1L) {
  vec_assert(x, integer())
  vec_assert(period, integer(), size = 1L)
  vec_assert(unit, integer(), size = 1L)

  new_vctr(x, class = "m_index", period = period, unit = unit)
}


period_attr <- function(x) attr(x, "period")
unit_attr <- function(x) attr(x, "unit")


validate_index <- function(x){
  x_raw <- vec_data(x)
  x_raw <- x_raw[!is.na(x_raw)]

  period <- period_attr(x)

  dup <- anyDuplicated(x_raw) # unexpected logical without anyDuplicated(x)
  if (dup) {
    cli::cli_abort(c(
            "Cannot have duplicated values",
      "i" = "{.arg x} has {dup} duplicate{?s}."
    ))
  }

  if (!period > 0) {
    cli::cli_abort(c(
      "Invalid period",
      "!" = "{.arg period} must be > 0."
    ))
  }

  x
}


#' `index` vector
#'
#' Create an integer vector that represents an index with _boosted_ features.
#' @details
#' - It is periodic. In the context of the package, it typically has period 31, 19 or 12 (for the 12-TET).
#'   It can also have period 7 or 5 if we want to use these methods for 7-notes or 5-notes sets.
#' - It has a transformable `unit`. This means the index can represent steps of 1 or any amount of steps.
#'   It can walk by 2 (e.g 0 is 0/12, 1 is 2/12, ...), 3, 4 and so on....
#'   By fifths is the mostly used feature.
#' @param x An integer vector
#'  * For `index()`: A integer vector
#'  * For `is_index()`: An object to test.
#' @param period An integer scalar. The period of the index.
#' @param unit An integer scalar. The unit of the index.
#' @return An S3 vector of class `m_index`.
#' @export
#' @examples
#' index(0:11, period = 12, unit = 7)
index <- function(x = integer(), period = 1L, unit = 1L) {
  x <- vec_cast(x, integer())
  period <- vec_recycle(vec_cast(period, integer()), 1L)
  unit <- vec_recycle(vec_cast(unit, integer()), 1L)

  validate_index(new_index(x, period = period, unit = unit))
}


#' @rdname index
#' @export
is_index <- function(x) {
  inherits(x, "m_index")
}


#' @noRd
#' @export
format.m_index <- function(x, ...) {
  out <- vec_data(x)
  out[is.na(x)] <- NA

  sel <- !is.na(out) & (out %% period_attr(x) == 0)
  out <- paste0(ifelse(sel, "~ ", "  "), out)

  out
}


#' @export
vec_ptype_abbr.m_index <- function(x, ...) {
  paste0("i<", unit_attr(x), "/", period_attr(x), ">")
}

#' @export
vec_ptype_full.m_index <- function(x, ...) {
  paste0("index<", unit_attr(x), "/", period_attr(x), ">")
}


# vec_ptype2.source.target

#' @export
# vec_ptype2.m_index.m_index <- function(x, y, ...) integer()
# Allow for easier joining
# But it escapes validation... especially the unique requirement...
vec_ptype2.m_index.m_index <- function(x, y, ...) {
  ptest <- period_attr(x) != period_attr(y)
  utest <- unit_attr(x) != unit_attr(y)
  if (ptest || utest) {
    cli::cli_abort(c(
      "Incompatible attributes",
      "!" = "Objects of class {.cls m_index} must share the same attributes."
    ))
  }
  index(period = period_attr(x), unit = unit_attr(x))
}

#' @export
vec_ptype2.integer.m_index <- function(x, y, ...) integer()
#' @export
vec_ptype2.m_index.integer <- function(x, y, ...) integer()


# vec_cast.target.source

#' @export
vec_cast.m_index.m_index <- function(x, to, ...) {
  index(vec_data(x), period = period_attr(x), unit = unit_attr(x))
}
#' @export
vec_cast.m_index.integer <- function(x, to, ...) index(x)
#' @export
vec_cast.integer.m_index <- function(x, to, ...) vec_data(x)
