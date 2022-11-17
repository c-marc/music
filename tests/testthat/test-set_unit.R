test_that("set_unit.default", {
  # 7
  expect_equal(set_unit(c(0, 4), 7, 1, 4), c(0, 1))
  # 12
  expect_equal(set_unit(c(0, 7), 12, 1, 7), c(0, 1))
})


test_that("set_unit.index", {
  # 7
  x <- index(c(0, 4), period = 7L, unit = 1L)
  y <- index(c(0, 1), period = 7L, unit = 4L)
  expect_equal(set_unit(x, 4), y)
  # 12
  x <- index(c(0, 7), period = 12L, unit = 1L)
  y <- index(c(0, 1), period = 12L, unit = 7L)
  expect_equal(set_unit(x, 7), y)
})
