test_that("Edge cases for PCA (#1)", {
  dat <- data.frame(x = c(NA, NA), y = c(NA, NA))
  expect_equal(length(dq_pca(dat)), 0)

  dat <- data.frame(x = 1, y = c(2, 2))
  expect_equal(length(dq_pca(dat)), 0)
})

test_that("Character/factors with one level are taken care of (#4)", {
  dat <- data.frame(x = rep("A", 10), y = 1)
  expect_equal(length(dq_pca(dat)), 0)

  dat$y <- 1:10
  expect_equal(length(dq_pca(dat)), 1)

  dat$z <- rep(c("B", "C"), each = 5)
  expect_equal(length(dq_pca(dat)), 3)
})
