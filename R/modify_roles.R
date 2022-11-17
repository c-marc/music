#' Augment or diminish roles
#'
#' Augment or diminish roles.
#' This will drop any other column. Index will be reset with the richest attributes.
#' @param x An [xtibble()] object with a column 'role'.
#' @param roles A string, scalar or vector. The roles to augment or diminish.
#' @param delta An integer. The shift. -7L (diminish) or 7L (augment).
#' @returns A new [xtibble()] with 2 columns:
#' - `idx` the modified index with attributes set to period 31 and unit 18,
#' - `role` the modified roles.
shift_role <- function(x, roles, delta){
  check_fifths(x)
  check_column(x, "role")
  purrr::walk(roles, ~rlang::arg_match(.x, x[["role"]]))
  rlang::check_required(delta)
  stopifnot(delta %in% c(-7L, 7L))

  xtbl_roles <- get_roles()

  idx_name <- idx_name_attr(x)

  idx_new <- purrr::map2_dbl(x[[idx_name]], x[["role"]], function(i, r){
    if (!r %in% roles) {
      return(i)
    }
    i2 <- i + delta
    if (!i2 %in% xtbl_roles[["idx"]]) {
      verb <- ifelse(delta > 0, "augmented", "diminished")
      cli::cli_par()
      cli::cli_inform(c(
        "Skip change",
        "i" = "{.val {r}} cannot be {verb} further and was left unchanged."
      ))
      cli::cli_end()
      return(i)
    }
    return(i2)
  })

  if (period_attr(x[[idx_name]]) != 31L) {
    cli::cli_par()
    cli::cli_inform(c(
      "Change of attributes",
      "!" = "Attributes of the index were augmented to account for possible richer context.",
      "i" = "{.field period} is now {.val {31L}} and {.field unit} {.val {18L}}."
    ))
    cli::cli_end()
  }

  xtibble(tibble::tibble("{idx_name}" := index(sort(idx_new), period = 31L, unit = 18L)),
          idx_name = idx_name) |>
    add_roles()
}


#' @rdname shift_role
#' @export
augment <- function(x, roles){
  UseMethod("augment", x)
}


#' @rdname shift_role
#' @export
#' @examples
#' start_with_mode(degree = 6) |> add_roles() |> augment("m3")
augment.m_xtibble <- function(x, roles) shift_role(x, roles = roles, delta = 7L)


#' @rdname shift_role
#' @export
diminish <- function(x, roles){
  UseMethod("diminish", x)
}


#' @rdname shift_role
#' @export
#' @examples
#' start_with_mode(degree = 1) |> add_roles() |> diminish("M7")
diminish.m_xtibble <- function(x, roles) shift_role(x, roles = roles, delta = -7L)
