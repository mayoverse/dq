
#' Get the eigenvalues of PCA
#'
#' @param dat The input data set.
#' @param object An R object
#' @param cutoffs Cutoffs for percent variance explained.
#' @param ... Other arguments.
#' @return An object of class "dq_pca": a numeric vector containing eigenvalues.
#' @name dq_pca
NULL
#> NULL

#' @rdname dq_pca
#' @export
dq_pca <- function(dat)
{
  dat2 <- stats::model.matrix(~ . - 1, data = dat)
  sds <- apply(dat2, 2, stats::sd, na.rm = TRUE)
  keep.cols <- !is.na(sds) & sds > 0
  dat2 <- dat2[, keep.cols, drop = FALSE]
  if(ncol(dat2) == 0) return(structure(numeric(0), class = "dq_pca"))
  structure(
    eigen(stats::cor(dat2, use = "complete.obs"), only.values = TRUE)$values,
    class = "dq_pca"
  )
}


#' @rdname dq_pca
#' @export
summary.dq_pca <- function(object, cutoffs = c(0.95, 0.975, 0.99), ...)
{
  eig <- cumsum(object)/sum(object)
  out <- lapply(cutoffs, function(cutoff) {
    min(which(cutoff <= eig))
  })
  stats::setNames(as.data.frame(out), paste0(cutoffs*100, "%"))
}
