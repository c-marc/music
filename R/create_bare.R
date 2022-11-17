#' Create the aeolian A scale
#'
#' Create the aeolian A scale.
#' It only builds a regular [tibble::tibble()].
#' It purposefully makes no use of anything from this package.
#' @return A [tibble::tibble()] 7x2 with columns `idx_r` (**regular half-tones scale**) and `note`.
#' @export
#' @family create_bare
create_aeolian_A <- function(){
  tibble::tibble(
    idx_r = cumsum(c(0, 2, 1, 2, 2, 1, 2)),
    note = LETTERS[1:7]
  )
}


#' Create chromatic scales
#'
#' Create a chromatic scale, either with sharps or bemols.
#' Because it builds up on [create_aeolian_A()], the chromatic scale will starts on "A".
#' @param flavor A string. Either "b" (default) or "#"/"s". Whether to label the five supplementary notes as sharps or bemols.
#' @return A [tibble::tibble()] 12 x 2 with columns `ìdx_r` (**regular half-tones scale**) and `note`.
#' @export
#' @family create_bare
#' @examples
#' create_chromatic(flavor = "#")
create_chromatic <- function(flavor = "b"){
  rlang::check_required(flavor)
  opt_b <- opt_s <- NULL
  c(opt_b, opt_s) %<-% bs_options()
  rlang::arg_match(flavor, values = c(opt_b, opt_s))

  if (flavor %in% opt_s) {
    step <- 1
    suffix <- "#"
  } else {
    step <- -1
    suffix <- "b"
  }

  aeolian_A <- create_aeolian_A()

  tbl <- aeolian_A %>%
    dplyr::mutate(idx_r = .data$idx_r + step, note = paste0(.data$note, suffix)) %>%
    dplyr::anti_join( dplyr::select(aeolian_A, "idx_r"), by = "idx_r" )

  tbl %>%
    dplyr::bind_rows(aeolian_A) %>%
    dplyr::arrange(.data$idx_r)
}
