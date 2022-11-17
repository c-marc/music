#' Slice an `xtibble` using index positions
#'
#' @description
#' Slice an [xtibble()] object, with a list of indices. Importantly:
#' - This uses the `period` attribute to match indices based on `modulo`results.
#' - This uses an _inner join_: so this allows pseudo-duplication (for example to select -6:6 of an index with period 12);
#'   It silently ignore missing indices (contrary to a _left join_) so that it allows re-ordering operation by specifying an arbitrary covering range (see example).
#' @export
#' @param x An [xtibble()] object.
#' @param idx A vector of integer.
#' @param ... Further arguments passed to or from other methods.
#' @returns An [xtibble()] object.
#' @examples
#' tbl <- tibble::tibble(idx = index(0:6, 7L, 1L), name = LETTERS[1:7])
#' xtibble(tbl, idx_name = "idx") |> periodic_slice(-9:9)
#'
#' # Re-ordering with a covering range
#' start_with_mode() |> add_roles() |> periodic_slice(0:11)
periodic_slice <- function(x, ...){
  UseMethod("periodic_slice", x)
}


#' @rdname periodic_slice
#' @export
periodic_slice.m_xtibble <- function(x, idx, ...){
  rlang::check_required(x)
  rlang::check_required(idx)

  idx_name <- period <- unit <- NULL
  c(idx_name, period, unit) %<-% ipu_attr(x)

  x <- x |>
    dplyr::rename(key = tidyselect::all_of(idx_name)) |>
    dplyr::mutate(key = vec_data(.data$key) %% period)

  tibble::tibble("{idx_name}" := index(idx, period = period, unit = unit)) |>
    xtibble(idx_name = idx_name) |>
    dplyr::mutate(key = vec_data(.data[[idx_name]]) %% period) |>
    dplyr::inner_join(x, by = "key") |>
    dplyr::select(-"key")
}
