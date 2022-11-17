test_that("xtibble() works", {
  idx_args <- list(period = 12, unit = 1)
  test <- xtibble(tibble::tibble(idx = index(1:2, !!!idx_args)),
                  idx_name = "idx",
                  lock = FALSE)

  expect_s3_class(test, "m_xtibble")
  expect_equal(idx_name_attr(test), "idx")
  expect_equal(is_locked(test), FALSE)
})


test_that("xtibble() is strict", {
  idx_args <- list(period = 7, unit = 1)
  data <- tibble::tibble(idx = index(1:2, !!!idx_args))

  expect_error(xtibble(tibble::tibble(idx = 1:2), idx_name = "idx"))

  expect_error(xtibble(data, idx_name = "wrong_name"))

  expect_error(xtibble(data, idx_name = "idx", lock = "wrong_lock"))
})
