test_that("augment works", {
  r1 <- c("m2","M2", "P4")
  r2 <- start_with_some_roles(r1) |> augment(r1) |> dplyr::pull("role")
  expect_equal(r2, c("M2", "A4", "A2"))

  # Do not augment
  r2 <- start_with_some_roles(c("P1", "A5")) |> augment("A5") |> dplyr::pull("role")
  expect_equal(r2, c("P1", "A5"))
})


test_that("diminish works", {
  r1 <- c("m2","M2", "P4")
  r2 <- start_with_some_roles(r1) |> diminish(r1) |> dplyr::pull("role")
  expect_equal(r2, c("d2", "d4", "m2"))

  # Do not diminish
  r2 <- start_with_some_roles(c("P1", "d5")) |> diminish("d5") |> dplyr::pull("role")
  expect_equal(r2, c("d5", "P1"))
})
