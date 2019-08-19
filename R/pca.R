

dq_pca <- function(dat)
{
  dat2 <- model.matrix(~ . - 1, data = dat)
  structure(
    eigen(cor(dat2, use = "complete.obs"), only.values = TRUE)$values,
    class = "dq_pca"
  )
}


