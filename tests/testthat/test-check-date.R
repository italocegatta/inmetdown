context("Check dates")

test_that("check_date parse", {
  x1 <- Sys.Date()
  x2 <- format(x1, "%d/%m/%Y")
  x3 <- format(x1, "%d.%m.%Y")
  x4 <- format(x1, "%Y-%m-%d")
  x5 <- format(x1, "%Y.%m.%d")

  expect_equal(check_date(x1), as.Date(x1))
  expect_equal(check_date(x2), as.Date(x2, "%d/%m/%Y"))
  expect_equal(check_date(x3), as.Date(x3, "%d.%m.%Y"))
  expect_equal(check_date(x4), as.Date(x4, "%Y-%m-%d"))
  expect_equal(check_date(x5), as.Date(x5, "%Y.%m.%d"))
})

# test_that("check_date 90 days", {
#   x1 <- Sys.Date()
#   expect_error(check_date(x1 - 91))
# })
