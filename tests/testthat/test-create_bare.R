test_that("create_aeolian_A() works", {
  aa <- create_aeolian_A()
  expect_equal(dim(aa), c(7, 2))
  expect_equal(aa[["note"]], LETTERS[1:7])
})


test_that("create_chromatic('s') works", {
  chr_s <- create_chromatic("s")
  expect_equal(dim(chr_s), c(12, 2))
  expect_true(all(c("G#", "A", "A#") %in% chr_s[["note"]]))
  expect_true(!any(c("B#", "E#") %in% chr_s[["note"]]))
})

test_that("create_chromatic('b') works", {
  chr_b <- create_chromatic("b")
  expect_equal(dim(chr_b), c(12, 2))
  expect_true(all(c("Ab", "A", "Bb") %in% chr_b[["note"]]))
  expect_true(!any(c("Cb", "Fb") %in% chr_b[["note"]]))
})
