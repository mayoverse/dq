


plot.dq_univariate <- function(x, variable, data, ...)
{
  stopifnot(var %in% names(data))
  stopifnot(var %in% names(x$variable))

  brk <- stats::setNames(x$trend.test, x$variable)[[variable]]$ind.max
  dat2 <- data.frame(y = data[[var]], obs = seq_len(nrow(data)), gp = c(rep("Before break", brk-1), rep("After break", nrow(dat) - brk + 1)))

  if(is.numericish(dat2$y))
  {
    p <- ggplot2::ggplot(dat2, ggplot2::aes(x = obs, y = y, group = gp)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
      ggplot2::xlab("Observation Number") + ggplot2::ylab(var)
  } else
  {
    dat2 <- as.data.frame(table(dat2[c("gp", "y")]))
    dat2$gp <- factor(dat2$gp, levels = c("Before break", "After break"))

    p <- ggplot2::ggplot(dat2, ggplot2::aes(x = gp, y = Freq, fill = y)) +
      ggplot2::geom_bar(position = "fill", stat = "identity") +
      ggplot2::xlab("") + ggplot2::ylab("Proportion") +
      ggplot2::scale_fill_discrete(name = var)
  }
  p + ggplot2::theme(text = ggplot2::element_text(size = 15, face = "bold"))
}

plot.dq_multivariate <- function(x, cutoff = 0.05, ...)
{
  q <- qnorm(p <- x$p.value)
  expected <- qnorm(ppoints(nrow(x)))
  ggplot2::ggplot(data.frame(x = expected, y = q, color = factor(p < cutoff, levels = c(FALSE, TRUE))),
                  ggplot2::aes(x = x, y = y, color = color)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("black", "red")) +
    ggplot2::xlab("Theoretical Normal Quantiles") +  ggplot2::ylab("Normal Quantiles of P-values") +
    ggplot2::ggtitle("QQ plot of Multivariate Outliers") +
    ggplot2::theme(text = element_text(size = 15, face = "bold"))
}

plot.dq_pca <- function(x, ...)
{
  dat <- data.frame(x = seq_along(x), y = x)
  ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::ggtitle("Scree Plot of PCAs") +
    ggplot2::xlab("PCA") + ggplot2::ylab("Variance Explained") +
    ggplot2::theme(text = element_text(size = 15, face = "bold"))
}
