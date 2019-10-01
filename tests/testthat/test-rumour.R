context("test-rumour")

test_that("whodunnit default version works", {
  expect_equal(length(whodunnit()), 1)
})

test_that("whodunnit non-default version works", {
  expect_equal(length(whodunnit(4)), 4)
})

test_that("whodunnit throws an error if you enter a string", {
  expect_error(whodunnit("4"), "whodunnit needs an integer input")
})

