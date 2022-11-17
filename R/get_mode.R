#' Get modes of seven or five notes
#'
#' Get all heptatonic or pentatonic modes.
#' @param mode_size An integer. The family of modes. Either 7 or 5.
#' @return An [xtibble()] object with 3 columns `idx`, `degree` and `name`.
#' @export
#' @family mode
#' @examples
#' # All 7-notes modes and initiate index on dorian
#' get_mode(mode_size = 7)
get_mode <- function(mode_size = 7){
  check_scalar_integerish(mode_size)
  if (!mode_size %in% c(7, 5)) {
    cli::cli_abort(c(
      "Invalid `mode_size`",
      "!" = "{.arg mode_size} must be either 7 or 5"
    ))
  }

  if (mode_size == 7) {
    xtbl <-  get_data("modes_7")
  } else {
    xtbl <- get_data("modes_5")
  }

  xtbl
}
