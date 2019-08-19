
#' Get the eigenvalues of PCA
#'
#' @param dat The input data set.
#' @export
dq_pca <- function(dat)
{
  dat2 <- stats::model.matrix(~ . - 1, data = dat)
  structure(
    eigen(stats::cor(dat2, use = "complete.obs"), only.values = TRUE)$values,
    class = "dq_pca"
  )
}


