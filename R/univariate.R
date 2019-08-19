
calc_outlier <- function(x, cutoff = 0.01)
{
  if(is.numericish(x))
  {
    x <- as.numeric(x)
    mu <- mean(x, na.rm = TRUE)
    std <- sd(x, na.rm = TRUE)
    tmp <- pnorm(x, mean = mu, sd = std)
    sum(tmp < cutoff/2 | tmp > 1-cutoff/2, na.rm = TRUE)
  } else
  {
    sum(table(x)[table(x)/length(x) < cutoff])
  }
}

univariate <- function(dat, cutoff)
{
  ## Calculate missings (counts and percents)
  nmiss <- colSums(is.na(dat))
  pct.miss <- 100*colMeans(is.na(dat))

  ## Kurtosis
  kurt <- vapply(dat, function(x) if(is.numericish(x)) e1071::kurtosis(as.numeric(x), na.rm = TRUE) - 3 else NA_real_, NA_real_)
  skew <- vapply(dat, function(x) if(is.numericish(x)) e1071::skewness(as.numeric(x), na.rm = TRUE) else NA_real_, NA_real_)

  ## outliers
  outliers <- vapply(dat, calc_outlier, NA_real_, cutoff = cutoff)
  pct_outliers <- 100*outliers/nrow(dat)

  structure(list(
    variable = names(dat),
    missings = nmiss,
    pct.miss = pct.miss,
    skewness = skew,
    excess.kurt = kurt,
    outliers = outliers,
    pct.outliers = pct_outliers,
    trend.test = trend.test(dat)
  ), class = c("dq_univariate", "data.frame"))
}

format.dq_univariate <- function(x, digits = 3, digits.pct = 1, ...)
{
  sn <- function(y) stats::setNames(y, x$variable)
  ## Calculate missings (counts and percents)
  nmiss <- sort(sn(x$missings), decreasing = TRUE)
  pct.miss <- sort(sn(x$pct.miss), decreasing = TRUE)

  ## Kurtosis
  kurt <- sn(x$excess.kurt)[order(kurt, decreasing = TRUE, na.last = TRUE)]
  skew <- sn(x$skewness)[order(abs(skew), decreasing = TRUE, na.last = TRUE)]

  ## outliers
  o <- order(x$outliers, decreasing = TRUE)
  outliers <- sn(x$outliers)[o]
  pct_outliers <-sn(x$pct.outliers)[o]

  trend.p <- vapply(x$trend.test, function(y) y$pval, NA_real_)
  trend <- sn(x$trend.test[order(trend.p)])

  data.frame(
    missings = paste0(names(nmiss), " (", nmiss, ", ", formatC(pct.miss, digits = digits.pct, format = "f"), "%)"),
    skewness = paste0(names(skew), " (", formatC(skew, digits = digits, format = "f"), ")"),
    excess.kurt = paste0(names(kurt), " (", formatC(kurt, digits = digits, format = "f"), ")"),
    outliers = paste0(names(outliers), " (", outliers, ", ", formatC(pct_outliers, digits = digits.pct, format = "f"), "%)"),
    trend.test = paste0(
      names(trend), " (Observation=",
      vapply(trend, function(y) y$ind.max, NA_real_), ", p-value=",
      formatC(vapply(trend, function(y) y$pval, NA_real_), digits = digits, format = "f"), ")"
    ),
    stringsAsFactors = FALSE
  )
}
