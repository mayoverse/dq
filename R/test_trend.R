# library("carData")
library("doMC")

registerDoMC()
options(cores=8)

trend.break <- function(x, buffer.pct = 0.05, buffer = 5, max.ind=100){
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



trend.perm.test.par <- function(x, nperm=1000, buffer.pct = 0.05, buffer=5, max.ind=100){
  ans <- trend.break(x, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind)
  out <- list(Fmax = ans$Fmax, ind.max = ans$ind.max)
  if(!is.na(ans$ind.max))
  {
    F.vec <- rep(0,nperm)
    F.vec <- foreach(k=1:nperm, .combine=c)%dopar%{
      x.k <- sample(x)
      trend.break(x.k, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind)$Fmax
    }
    out$pval <- sum(F.vec > ans$Fmax)/nperm
  } else out$pval <- NA_real_
  out
}


trend.test <- function(dat, nperm = 100, buffer.pct = 0.05, buffer = 5, max.ind = 100)
{
  dat <- fix.dates(dat)
  tmp <- purrr::map(dat, trend.perm.test.par, nperm = nperm, buffer.pct = buffer.pct, buffer = buffer, max.ind = max.ind)
  pvals <- purrr::map_dbl(tmp, "pval")
  tmp <- tmp[order(pvals)]
  arsenal::set_attr(paste0(
    names(tmp), " (Observation=",
    purrr::map_dbl(tmp, "ind.max"), ", p-value=",
    formatC(purrr::map_dbl(tmp, "pval"), digits = 2, format = "f"), ")"
  ), "results", tmp)
}


# set.seed(22)
# head(Hartnagel)
#
#
#
# x1 <- Hartnagel$fconvict
# x2 <- cut(x1, breaks=seq(40,160,by=10))
#
# system.time(trend.perm.test(x1, nperm=1000, buffer=5, max.ind=100))
# system.time(ans1 <- trend.perm.test.par(x1, nperm=1000, buffer=5, max.ind=100))
# ans1
#
# ans2 <- trend.perm.test.par(x2, nperm=1000, buffer=5, max.ind=100)
# ans2
#

trend_plot <- function(var, dat, results)
{
  validate(
    need(var != " ", "Please select a variable.")
  )
  stopifnot(var %in% names(dat))
  stopifnot(var %in% names(results))

  brk <- results[[var]]$ind.max
  dat2 <- data.frame(y = dat[[var]], obs = seq_len(nrow(dat)), gp = c(rep("Before break", brk-1), rep("After break", nrow(dat) - brk + 1)))
  if(is.numericish(dat2$y))
  {
    p <- ggplot(dat2, aes(x = obs, y = y, group = gp)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      xlab("Observation Number") + ylab(var)
  } else
  {
    dat2 <- as.data.frame(table(dat2[c("gp", "y")]))
    dat2$gp <- factor(dat2$gp, levels = c("Before break", "After break"))
    p <- ggplot(dat2, aes(x = gp, y = Freq, fill = y)) +
      geom_bar(position = "fill", stat = "identity") +
      xlab("") + ylab("Proportion") +
      scale_fill_discrete(name = var)
  }
  p + theme(text = element_text(size = 15, face = "bold"))
}




