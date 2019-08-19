

get_eigen <- function(dat)
{
  dat2 <- model.matrix(~ . - 1, data = dat)
  eigen(cor(dat2, use = "complete.obs"), only.values = TRUE)$values
}


