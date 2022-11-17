#' Compute the greatest common divisor of two values
#'
#' Compute the greatest common divisor of two values. This implements Euler's algorithm.
#' @param x,y The integers to compute the gcd of.
#' @return An integer, the gcd.
#' @noRd
#' @examples
#' 1:11 |> purrr::map_int(~gcd(.x, 12))
#' 1:11 |> purrr::keep(~gcd(.x, 12) == 1)
gcd <- function(x, y){
  rlang::check_required(x)
  rlang::check_required(y)
  stopifnot(rlang::is_scalar_integerish(x))
  stopifnot(rlang::is_scalar_integerish(y))

  r <- x %% y
  ifelse(r, gcd(y, r), as.integer(y))
}


#' Compute the least common multiple of two values
#'
#' @param x,y The integers to compute the `lcm` of.
#' @return An integer, the `lcm`.
#' @noRd
lcm <- function(x, y){
  as.integer(x * y / gcd(x, y))
}


#' Verify if x is a prime
#'
#' Check if a value is a prime.
#' @param x, An integer to test
#' @return A logical
#' @noRd
#' @examples
#' 1:11 |> purrr::keep(~is_prime(.x))
is_prime <- function(x){
  stopifnot(rlang::is_scalar_integerish(x))
  stopifnot(x > 0)

  if (x <= 2) {
    return(TRUE)
  }

  # Brute force
  xinf <- rlang::seq2(from = 2, to = (x - 1))

  purrr::every(xinf, ~x %% .x != 0)
}


#' Constructor for `primes`
#' @noRd
new_primes <- function(x = integer(), to = integer()){
  stopifnot(rlang::is_integer(x))
  stopifnot(rlang::is_scalar_integer(to))
  stopifnot(to > 0)

  structure(x,
            class = "primes",
            to = to)
}


#' Find all primes up to a value and create an object of class `primes`
#'
#' Find all primes up to a certain value and create an object of class `primes` to use later or amend.
#' The upper limit of validity (up to which the list is exhaustive) is stored as attribute `to`.
#' @param to An integer. Primes are listed from 1 to this upper bound.
#' @return An object of class `primes` with primes as a vector of integers.
#' @noRd
#' @examples
#' primes(to = 20)
primes <- function(to = 2L){
  stopifnot(rlang::is_scalar_integerish(to))
  stopifnot(to > 0)

  ps <- rlang::seq2(from = 1, to = to)
  ps <- purrr::keep(ps, ~is_prime(.x))

  new_primes(x = as.integer(ps),
             to = as.integer(to))
}


#' Extend an object of class `primes` to an upper bound
#'
#' Find new primes only in the supplementary range defined by the upper bound of coverage of `x` and the `to` argument.
#' @inheritParams primes
#' @param x A `primes` object
#' @noRd
add_primes <- function(x, to = 2L){
  stopifnot(rlang::inherits_only(x, "primes"))
  stopifnot(rlang::is_scalar_integerish(to))

  if (to <= attr(x, "to")) {
    return(x)
  }

  seq <- rlang::seq2(from = attr(x, "to") + 1, to = to)
  seq <- purrr::keep(seq, ~is_prime(.x))

  new_primes(c(x, seq), to = as.integer(to))
}


#' Factorize `a` by its divisor `b`
#'
#' Factorize `a` into `(b, q)` where `a = b * q`.
#' @noRd
factorize1 <- function(a, b){
  stopifnot(a %% b == 0)
  c(b, a %/% b) # c(b, q) append / go forward
}


#' Recursively factorize a value using a list of dividers
#'
#' Use a list of dividers to try to factorize a value with.
#' The algorithm requires
#' @param xs A vector of integers.
#'   The multiples composing the value being refactored.
#'   The last value is the one being refactored over and over.
#'   Values before are multiples already factorized.
#' @param ps A vector of integers. The dividers to try.
#'   These are used from last to first; the next one is only tried once it's not possible to factorize by the current value.
#' @noRd
#' @examples
#' factorize_rec(12, c(2, 6)) # 6 2
#' factorize_rec(12, c(6, 2)) # 2 2 3
factorize_rec <- function(xs, ps){
  a <- xs[length(xs)] # q from previous step

  # If ps wasn't supplied with 1 as its first element
  if (length(ps) == 0) {
    return(xs)
  }
  if (a == 1) {
    return(xs[-length(xs)]) # Get rid of 1
  }
  p <- ps[length(ps)] # Consume ps backward
  if (a %% p == 0) {
    xs <- c(xs[-length(xs)], factorize1(a, p))
    # Try again with p
    return(factorize_rec(xs, ps))
  }

  # Else try next smaller p
  return( factorize_rec(xs, ps[-length(ps)]) )
}


#' Factorize by primes
#'
#' @param x An integer. The value to refactor.
#' @param ps An object of class `primes`.
#'   A vector of integers with precomputed primes over a range sufficiently large to cover `x`.
#' @return A vector of integer. The decomposition of `x` as multiples of prime numbers.
#' @noRd
#' @examples
#' ps <- primes(to = 100)
#' factorize_by_primes(42, ps = ps)
#' factorize_by_primes(78, ps = ps) %>% prod()
factorize_by_primes <- function(x, ps){
  rlang::check_required(x)
  rlang::check_required(ps)
  stopifnot(rlang::inherits_only(ps, "primes"))

  if ( x > attr(ps, "to") ) {
    cli::cli_abort(c(
            "Primes were not computed up to that value",
      "i" = "First compute more primes with {.code ps <- add_primes(ps, to)}"
    ))
  }
  factorize_rec(x, ps)
}
