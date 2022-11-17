test_that("index() works", {
  test <- index(0:11, period = 12, unit = 7)

  expect_s3_class(test, "m_index")
  expect_type(test, "integer")
  expect_equal(vec_data(test), 0:11)
  expect_equal(ignore_attr = TRUE, unclass(test), 0:11)
  expect_equal(period_attr(test), 12)
  expect_equal(unit_attr(test), 7)
})


test_that("index() is strict", {
  expect_error(index("nope", period = 12, unit = 7))
  expect_error(index(0:11, period = -12, unit = 7))
  expect_error(index(0:11, period = 12, unit = "7"))
})


test_that("index() casting keep the class", {
  # This is required to avoid joining nightmare...
  res <- index(0:1) |> {\(x)vec_c(x, x)}()
  expect_s3_class(res, "m_index")
})


test_that("index() casting strip the class", {
  res <- index(0:1) |> {\(x)vec_c(1L, x)}()
  expect_s3_class(res, NA)

  res <- index(0:1) |> {\(x)vec_c(x, 1L)}()
  expect_s3_class(res, NA)
})
