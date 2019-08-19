# library("carData")

library("doMC")
registerDoMC()
options(cores=8)

detect.mv.outliers.par <- function(X){
  X <- fix.dates(as.data.frame(X))
  p <- ncol(X)
  n <- nrow(X)
  X.NA <- is.na(X)

  ind.cont <- vapply(X, is.numericish, NA)

  pval <- foreach(i=1:n, .combine=c)%dopar%{
    ind.nna.i <- !X.NA[i,]
    ind.cont.i <-  ind.nna.i & ind.cont
    ind.cat.i <-  ind.nna.i & !ind.cont
    if(!any(ind.cont.i))
    {
      NA
    } else
    {
      X.cont.i <- X[ind.cont.i]
      X.cat.i <- X[ind.cat.i]

      ind.obs.i <- rowSums(X.NA[, ind.nna.i]) == 0
      X.cat.i <- X.cat.i[ind.obs.i,,drop=FALSE]
      X.cont.i <- X.cont.i[ind.obs.i,,drop=FALSE]
      x.ij <- if(ncol(X.cat.i) == 0) matrix(1, nrow(X.cont.i), 1) else model.matrix(~ . , data=X.cat.i)

      eps.i <- X.cont.i
      eps.i[] <- lapply(X.cont.i, function(col) {
        lm.fit(x.ij, col)$residuals
      })

      SX <- cov(eps.i, use="complete.obs")
      if(anyNA(SX)) SX <- cov(eps.i, use="pairwise.complete.obs")

      foo <- try(chol(SX), silent = TRUE)
      while(is.character(foo[1])){
        ##print("blah")
        dSX <- diag(SX)
        SX <- .95*SX
        diag(SX) <- dSX
        foo <- try(chol(SX), silent = TRUE)
      }
      Sinv <- chol2inv(foo)
      ii <- which(i==which(ind.obs.i))
      res.i <- as.numeric(eps.i[ii,])
      v2.i <- t(res.i)%*%Sinv%*%res.i
      pchisq(v2.i, ncol(X.cont.i), lower.tail=FALSE)
    }
  }
  return(pval)
}

multivariate <- function(dat)
{
  out <- data.frame(
    Observation = 1:nrow(dat),
    p.value = detect.mv.outliers.par(dat)
  )
  out <- out[order(out$p.value), ]
  row.names(out) <- NULL
  class(out) <- c("multivariate", "data.frame")
  out
}


format.multivariate <- function(x, digits = 4, ...)
{
  class(x) <- "data.frame"
  x$p.value <- formatC(x$p.value, digits = digits, format = "f")
  x
}

