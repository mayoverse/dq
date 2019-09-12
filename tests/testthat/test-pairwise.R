test_that("No missings in pairwise (#3)", {

  dat <- data.frame(x = 1:10, y = rep(c("A", "B"), each = 5), z = c(1, 0:8), stringsAsFactors = FALSE)
  expect_equal(format(dq_pairwise(dat))$correlation.nas, c("There are not 2 or more variables with missings.", "", ""))
})
