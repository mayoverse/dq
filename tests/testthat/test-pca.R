test_that("Edge cases for PCA (#1)", {
  dat <- data.frame(x = c(NA, NA), y = c(NA, NA))
  expect_equal(length(dq_pca(dat)), 0)

  dat <- data.frame(x = 1, y = c(2, 2))
  expect_equal(length(dq_pca(dat)), 0)
})
