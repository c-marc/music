test_that("set_lock() works", {
  test <- example("xtibble")

  expect_equal(test |> set_lock(to = FALSE) |> is_locked(), FALSE)
  expect_equal(test |> set_lock(to = TRUE) |> is_locked(), TRUE)
})
