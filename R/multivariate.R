
detect.mv.outliers.par <- function(X, mc.cores){
  X <- fix.dates(as.data.frame(X))
  p <- ncol(X)
  n <- nrow(X)
  X.NA <- is.na(X)

  ind.cont <- vapply(X, is.numericish, NA)

  pval <- parallel::mclapply(1:n, function(i) {
    ind.nna.i <- !X.NA[i,]
    ind.cont.i <-  ind.nna.i & ind.cont
    ind.cat.i <-  ind.nna.i & !ind.cont
    if(!any(ind.cont.i)) return(NA_real_)

    X.cont.i <- X[ind.cont.i]
    X.cat.i <- X[ind.cat.i]

    ind.obs.i <- rowSums(X.NA[, ind.nna.i]) == 0
    X.cat.i <- X.cat.i[ind.obs.i,,drop=FALSE]
    X.cont.i <- X.cont.i[ind.obs.i,,drop=FALSE]
    x.ij <- if(ncol(X.cat.i) == 0) matrix(1, nrow(X.cont.i), 1) else stats::model.matrix(~ . , data=X.cat.i)

    eps.i <- X.cont.i
    eps.i[] <- lapply(X.cont.i, function(col) {
      stats::lm.fit(x.ij, col)$residuals
    })

    SX <- stats::cov(eps.i, use="complete.obs")
    if(anyNA(SX)) SX <- stats::cov(eps.i, use="pairwise.complete.obs")

    foo <- try(chol(SX), silent = TRUE)
    while(is.character(foo[1])){
      dSX <- diag(SX)
      SX <- .95*SX
      diag(SX) <- dSX
      foo <- try(chol(SX), silent = TRUE)
    }
    Sinv <- chol2inv(foo)
    ii <- which(i==which(ind.obs.i))
    res.i <- as.numeric(eps.i[ii,])
    v2.i <- t(res.i)%*%Sinv%*%res.i
    stats::pchisq(v2.i, ncol(X.cont.i), lower.tail=FALSE)
  }, mc.cores = mc.cores)
  return(unlist(pval, recursive = FALSE, use.names = FALSE))
}

#' Compute multivariate quality metrics
#'
#' @param dat The input data set
#' @param mc.cores See \code{\link[parallel]{mclapply}}
#' @param digits,digits.pval How many digits to print
#' @param x,object An R object
#' @param n Number of rows to print
#' @param ... Other arguments. For \code{summary()}, these are passed to \code{format()}.
#' @return An object of class "dq_multivariate".
#' @name dq_multivariate
NULL
#> NULL

#' @rdname dq_multivariate
#' @export
dq_multivariate <- function(dat, mc.cores = getOption("mc.cores", 8L))
{
  out <- data.frame(
    Observation = 1:nrow(dat),
    p.value = detect.mv.outliers.par(dat, mc.cores = mc.cores)
  )
  out <- out[order(out$p.value), ]
  row.names(out) <- NULL
  structure(out, class = c("dq_multivariate", "data.frame"))
}

#' @rdname dq_multivariate
#' @export
format.dq_multivariate <- function(x, digits.pval = 4, ...)
{
  data.frame(
    Observation = x$Observation,
    p.value = formatC(x$p.value, digits = digits.pval, format = "f"),
    stringsAsFactors = FALSE
  )
}

#' @rdname dq_multivariate
#' @export
summary.dq_multivariate <- function(object, n = 10, ...)
{
  out <- utils::head(format(object, ...), n)
  colnames(out) <- c("Observation", "P value")
  out
}


