bs_options <- function() {
  list(b = c("b", "bemol", "bemols"),
       s = c("s", "sharp", "sharps", "#")
  )
}


#' Create 31 notes
#'
#' @description
#' Create 31 notes (31-EDO) with "bb", "b", none, "#" and "x" suffix.
#' "x" is meant as _doubled sharp_.
#' "Fbb", "Cbb", "Ex", "Bx" are excluded.
#'
#' A secondary column `allowed_root` limit the range of allowed root note from "Gb" to "A#".
#' This allows to construct -9:9 available notes (19-EDO) for each root note.
#' @returns An [xtibble()] object with:
#' - `idx` (period 31, unit 18),
#' - `note`,
#' - `n_suffix`, number of bemols (negative count) or sharps (positive).
#' @examples
#' music:::create_notes()
create_notes <- function(){
  notes <- LETTERS[1:7]
  notes <- notes[match(0:6, (0:6 - 5) %% 7)] # FGABCDE
  notes <- notes[match(0:6, (0:6 * 2) %% 7)] # FCGDAEB

  tbl <- tibble::tibble(suffix = c("bb","b","","#","x"), n_suffix = -2:2) |>
    dplyr::rowwise() |>
    dplyr::mutate(note = list(paste0(notes, .data$suffix))) |>
    tidyr::unnest("note") |>
    dplyr::select(-"suffix") |>
    dplyr::filter(!.data$note %in% c("Fbb", "Cbb", "Ex", "Bx"))

  xinf <- purrr::detect_index(tbl[["note"]], ~.x == "Gb")
  xsup <- purrr::detect_index(tbl[["note"]], ~.x == "A#")

  tbl |>
    dplyr::mutate(idx = dplyr::row_number(),
                  c1 = .data$idx >= xinf,
                  c2 = .data$idx <= xsup,
                  allowed_root = ifelse(.data$c1 & .data$c2, .data$note, NA)) |>
    dplyr::select(!tidyselect::all_of(c("c1", "c2"))) |>
    dplyr::mutate(idx = index(.data$idx, period = 31L, unit = 18L))
}
