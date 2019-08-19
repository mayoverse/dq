
trend.break <- function(x, buffer.pct, buffer, max.ind){
  Fmax <- 0
  ind.max <- NA_integer_

  whch.NA <- which(x.nNA <- !is.na(x))
  x <- x[x.nNA]
  n <- length(x)
  buffer <- max(buffer, round(buffer.pct * n))
  if(n < 2*buffer) return(list(Fmax = Fmax, ind.max = ind.max))

  b.ind <- if(n-2*buffer < max.ind) seq(from=buffer+1, to=n-buffer, by=1) else round(seq(from=buffer+1, to=n-buffer, length.out=max.ind))
  n.ind <- length(b.ind)

  if(!is.numericish(x)){
    x <- as.character(x)
    uniq <- unique(x)
    if(length(uniq) == 1) return(list(Fmax = Fmax, ind.max = ind.max))
    for(j in 1:n.ind){
      tab <- countit(x = x, cutoff = b.ind[j], lvls = uniq)
      before <- tab[, 1]
      after  <- tab[, 2]

      sr <- (before + after)/n
      E1 <- sum(before)*sr
      E2 <- sum(after)*sr
      Fj <- sum((after - E2)^2 / E2) + sum((before - E1)^2 / E1)

      if(Fj > Fmax){
        Fmax <- Fj
        ind.max <- whch.NA[b.ind[j]]+1
      }
    }
  } else
  {
    SSE0 <- sum((x-mean(x))^2)
    df0 <- n-1
    for(j in 1:n.ind){
      x1 <- x[1:b.ind[j]]
      x2 <- x[(b.ind[j]+1):n]
      foo1 <- .lm.fit(cbind(1,1:b.ind[j]), x1)
      foo2 <- .lm.fit(cbind(1,(b.ind[j]+1):n), x2)
      xres <- c(foo1$residuals, foo2$residuals)
      SSE1 <- sum(xres^2)
      df1 <- n-4
      Fj <- ((SSE0-SSE1)/(df0-df1))/(SSE1/df1)
      if(Fj > Fmax){
        Fmax <- Fj
        ind.max <- whch.NA[b.ind[j]]+1
      }
    }
  }
  list(Fmax=Fmax, ind.max=ind.max)
}



trend.perm.test.par <- function(x, nperm, buffer.pct, buffer, max.ind, mc.cores = mc.cores){
  ans <- trend.break(x, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind)
  out <- list(Fmax = ans$Fmax, ind.max = ans$ind.max)
  if(!is.na(ans$ind.max))
  {
    F.vec <- mclapply(1:nperm, function(k) {
      x.k <- sample(x)
      trend.break(x.k, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind)$Fmax
    }, mc.cores = mc.cores)
    F.vec <- unlist(F.vec, recursive = FALSE, use.names = TRUE)
    out$pval <- sum(F.vec > ans$Fmax)/nperm
  } else out$pval <- NA_real_
  out
}


trend.test <- function(dat, nperm = 100, buffer.pct = 0.05, buffer = 5, max.ind = 100, mc.cores = getOption("mc.cores", 8L))
{
  dat <- fix.dates(dat)
  unname(lapply(dat, trend.perm.test.par, nperm = nperm, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind, mc.cores = mc.cores))
}
