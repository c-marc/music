#' Start with a set of chosen notes
#'
#' Create an object of class `m_xtibble` from a set of notes.
#' @param notes A vector of strings. The set of notes.
#' Expected format is an uppercase letter between A and G, with the optional suffix "b", "#", "bb" or "x" (e.g. "C", "Eb", "F#"...).
#' @param root_note A string. The root note. Not necessary in `notes`.
#' @param idx_name A string, the column name for the index (default "idx").
#' @return An object `m_xtibble` with columns `idx` (period 31, unit 18) and `notes`.
#' @export
#' @family start_with_
#' @examples
#' start_with_some_notes(c("F","A","Eb"), root_note = "Gb")
start_with_some_notes <- function(notes, root_note = "C", idx_name = "idx"){
  tbl_notes <- get_notes(root_note = root_note)

  purrr::walk(notes, ~rlang::arg_match(.x, tbl_notes[["note"]]))

  tbl_notes |>
    dplyr::filter(.data$note %in% notes) |>
    dplyr::rename("{idx_name}" := "idx")
}
