
#' Get the eigenvalues of PCA
#'
#' @param dat The input data set.
#' @value An object of class "dq_pca": a numeric vector containing eigenvalues.
#' @export
dq_pca <- function(dat)
{
  dat2 <- stats::model.matrix(~ . - 1, data = dat)
  sds <- apply(dat2, 2, sd, na.rm = TRUE)
  keep.cols <- !is.na(sds) & sds > 0
  dat2 <- dat2[, keep.cols, drop = FALSE]
  if(ncol(dat2) == 0) return(structure(numeric(0), class = "dq_pca"))
  structure(
    eigen(stats::cor(dat2, use = "complete.obs"), only.values = TRUE)$values,
    class = "dq_pca"
  )
}


