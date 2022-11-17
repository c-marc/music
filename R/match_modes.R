#' Match 5 and 7 modes
#'
#' Provide a matching table between 7 and 5 modes.
#' @export
#' @examples
#' match_modes()
match_modes <- function(){
  xtbl5 <- get_data("modes_5") |> rlang::set_names(\(x)paste0(x,"_5"))
  xtbl7 <- get_data("modes_7") |> rlang::set_names(\(x)paste0(x,"_7"))

  tidyr::expand_grid(idx_5 = 0:4, delta = -1:1) |>
    dplyr::mutate(idx_7 = .data$idx_5 + .data$delta) |>
    dplyr::left_join(xtbl5, by = "idx_5") |>
    dplyr::left_join(xtbl7, by = "idx_7") |>
    dplyr::select(-"idx_5", "idx_7")
}
