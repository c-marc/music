#' Add notes to an `xtibble` object
#'
#' Add a column with notes to an [xtibble()] object.
#' The [xtibble()] must be ordered as fifths.
#' A _root note_ is required (the note that corresponds to zero in the index).
#' @export
#' @param x An [xtibble()] with [index()] valid attributes.
#' @param root_note A string, the root note (default is "C").
#' @returns The [xtibble()] object with an additional (or updated) column `note`.
#' @family add_
#' @examples
#' start_with_mode(degree = 2) |> add_notes(root_note = "G")
add_notes <- function(x, root_note){
  UseMethod("add_notes", x)
}


#' @rdname add_notes
#' @export
add_notes.m_xtibble <- function(x, root_note = "C"){
  # Required attributes
  check_fifths(x)

  idx_name <- period <- unit <- NULL
  c(idx_name, period, unit) %<-% ipu_attr(x)

  xtbl_notes <- get_notes(root_note = root_note) |>
    dplyr::mutate("{idx_name}" := index(.data$idx, period = period, unit = unit))

  x |>
    # Delete if it exists
    dplyr::select(!tidyselect::any_of("note")) |>
    dplyr::left_join(xtbl_notes, by = idx_name)
}
