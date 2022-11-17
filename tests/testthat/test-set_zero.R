# set_zero.m_index

test_that("set_zero.m_index() works", {
  expect_equal(index(1:3) |> set_zero(on = 1), index(0:2))
})


test_that("set_zero.m_index() is strict", {
  # Type
  expect_error(index(1:3) |> set_zero(on = "1"))
})


# set_zero.m_xtibble

test_that("set_zero.m_xtibble() works", {
  test <- xtibble(tibble::tibble(idx = index(0:3)),
                  idx_name = "idx",
                  lock = FALSE)
  expect_equal(test |> set_zero(on = 1L) |> dplyr::pull(idx), index(-1:2))
  expect_equal(test |> set_zero(on = 1) |> dplyr::pull(idx), index(-1:2))
})


test_that("set_zero.m_xtibble() works with from", {
  test <- xtibble(tibble::tibble(idx = index(0:3), name = LETTERS[1:4]),
                  idx_name = "idx",
                  lock = FALSE)
  expect_equal(test |> set_zero(on = "B", from = "name") |> dplyr::pull(idx), index(-1:2))
})


test_that("set_zero.m_xtibble() is strict", {
  test <- xtibble(tibble::tibble(idx = index(0:3), name = LETTERS[1:4]),
                  idx_name = "idx",
                  lock = FALSE)

  # Name
  expect_error(test |> set_zero(on = "B", from = "wrong"))

  # Type
  expect_error(test |> set_zero(on = TRUE, from = "name"))

  # Not in
  expect_error(test |> set_zero(on = "Z", from = "name"))
})
