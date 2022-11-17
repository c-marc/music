test_that("get_data() lists available tables", {
  mtbl_internal <- c(
    "roles",
    "modes_7",
    "modes_5",
    "melodic_minor",
    "harmonic_minor"
  )
  expect_equal(sort(get_data()), sort(mtbl_internal))
})


test_that("get_data() aborts for unknown data", {
  expect_error(get_data("unknown"))
})


test_that("All data are reachable and mtbl objects", {
  all_mtbl <- get_data()
  purrr::walk(all_mtbl, function(name){
    mtbl <- get_data(name)
    expect_s3_class(mtbl, "m_xtibble")
  })
})
