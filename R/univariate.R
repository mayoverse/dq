
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
  nmiss <- sort(colSums(is.na(dat)), decreasing = TRUE)
  pct.miss <- sort(colMeans(is.na(dat)), decreasing = TRUE)

  ## Kurtosis
  kurt <- purrr::map_dbl(dat, function(x) if(is.numericish(x)) e1071::kurtosis(as.numeric(x), na.rm = TRUE) - 3 else NA)
  kurt <- kurt[order(kurt, decreasing = TRUE, na.last = TRUE)]
  skew <- purrr::map_dbl(dat, function(x) if(is.numericish(x)) e1071::skewness(as.numeric(x), na.rm = TRUE) else NA_real_)
  skew <- skew[order(abs(skew), decreasing = TRUE, na.last = TRUE)]

  ## outliers
  outliers <- sort(purrr::map_int(dat, calc_outlier, cutoff = cutoff), decreasing = TRUE)
  pct_outliers <- outliers/nrow(dat)

  data.frame(
    missings = paste0(names(nmiss), " (", nmiss, ", ", formatC(100*pct.miss, digits = 1, format = "f"), "%)"),
    skewness = paste0(names(skew), " (", formatC(skew, digits = 3, format = "f"), ")"),
    excess.kurt = paste0(names(kurt), " (", formatC(kurt, digits = 3, format = "f"), ")"),
    outliers = paste0(names(outliers), " (", outliers, ", ", formatC(100*pct_outliers, digits = 1, format = "f"), "%)"),
    trend.test = trend.test(dat),
    stringsAsFactors = FALSE
  )
}

