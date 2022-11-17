test_that("add_roles works", {
  # With period 31
  xtbl <- xtibble(tibble::tibble(i = index(c(0, 6), period = 31L, unit = 18L)),
                  idx_name = "i")
  test <- xtbl |> add_roles()

  expect_equal(dplyr::pull(test, "role"), c("P1", "A4"))
  expect_equal(ipu_attr(xtbl), ipu_attr(test))

  # With period 12
  xtbl <- xtibble(tibble::tibble(i = index(c(0, 6), period = 12L, unit = 7L)),
                  idx_name = "i")
  test <- xtbl |> add_roles()

  expect_equal(dplyr::pull(test, "role"), c("P1", "A4"))
  expect_equal(ipu_attr(xtbl), ipu_attr(test))
})


test_that("add_roles use different label", {
  # With period 12
  xtbl <- xtibble(tibble::tibble(i = index(c(0, 6), period = 12L, unit = 7L)),
                  idx_name = "i")
  test <- xtbl |> add_roles(style = "collapse_simpler")

  expect_equal(dplyr::pull(test, "role"), c("P1", "d5~A4"))
  expect_equal(ipu_attr(xtbl), ipu_attr(test))
})


test_that("add_roles is strict", {
  # Not allowed if not as fifths
  xtbl <- xtibble(tibble::tibble(i = index(0:1, period = 31L, unit = 10L)),
                  idx_name = "i")
  expect_error(xtbl |> add_roles())
})
