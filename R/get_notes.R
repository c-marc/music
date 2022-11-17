#' Get notes with zero of the index set to a root note
#'
#' Get all 31 notes ordered as fifths, with zero of the index set to a specified root note.
#' Available root note are any note between "Gb" and "A#".
#' @export
#' @param root_note A string, the root note.
#' @returns An [xtibble()] object 31 x 2 with `idx` and `note`.
#' @examples
#' get_notes(root_note = "G")
get_notes <- function(root_note = "C"){
  xtbl <- create_notes() |>
    xtibble(idx_name = "idx")

  rlang::arg_match(root_note, xtbl[["allowed_root"]])

  i <- purrr::detect_index(xtbl[["allowed_root"]], ~!is.na(.x) && .x == root_note)

  xtbl |>
    dplyr::mutate(idx = .data$idx - as.integer(.data$idx[i])) |>
    dplyr::select("idx", "note")
}
