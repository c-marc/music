test_that("add_notes works", {
  # With period 31
  xtbl <- xtibble(tibble::tibble(i = index(0:1, period = 31L, unit = 18L)),
                  idx_name = "i")
  test <- xtbl |> add_notes(root_note = "D")

  expect_equal(dplyr::pull(test, "note"), c("D", "A"))
  expect_equal(ipu_attr(xtbl), ipu_attr(test))

  # With period 12
  xtbl <- xtibble(tibble::tibble(i = index(0:1, period = 12L, unit = 7L)),
                  idx_name = "i")
  test <- xtbl |> add_notes(root_note = "G")

  expect_equal(dplyr::pull(test, "note"), c("G", "D"))
  expect_equal(ipu_attr(xtbl), ipu_attr(test))
})


test_that("add_notes is strict", {
  # Not allowed if not as fifths
  xtbl <- xtibble(tibble::tibble(i = index(0:1, period = 31L, unit = 10L)),
                  idx_name = "i")
  expect_error(xtbl |> add_notes(root_note = "D"))

  # Not allowed with extreme root
  xtbl <- xtibble(tibble::tibble(i = index(0:1, period = 31L, unit = 18L)),
                  idx_name = "i")
  expect_error(xtbl |> add_notes(root_note = "Cb"))
})
