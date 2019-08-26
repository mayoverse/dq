
#' Plot \code{dq} objects
#'
#' @param x An object
#' @param variable Character string: which variable to plot?
#' @param data The original data.frame used in \code{\link{dq_univariate}}.
#' @param cutoff The cutoff for outliers.
#' @param ... Other arguments.
#' @import ggplot2
#' @name plot_dq
NULL
#> NULL

#' @rdname plot_dq
#' @export
plot.dq_univariate <- function(x, variable, data, ...)
{
  stopifnot(variable %in% names(data))
  stopifnot(variable %in% x$variable)

  brk <- stats::setNames(x$trend.test, x$variable)[[variable]]$ind.max
  dat2 <- data.frame(y = data[[variable]], obs = seq_len(nrow(data)), gp = c(rep("Before break", brk-1), rep("After break", nrow(data) - brk + 1)))

  if(is.numericish(dat2$y))
  {
    p <- ggplot(dat2, aes_string(x = "obs", y = "y", group = "gp")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      xlab("Observation Number") + ylab(variable)
  } else
  {
    dat2 <- as.data.frame(table(dat2[c("gp", "y")]))
    dat2$gp <- factor(dat2$gp, levels = c("Before break", "After break"))

    p <- ggplot(dat2, aes_string(x = "gp", y = "Freq", fill = "y")) +
      geom_bar(position = "fill", stat = "identity") +
      xlab("") + ylab("Proportion") +
      scale_fill_discrete(name = variable)
  }
  p + theme(text = element_text(size = 15, face = "bold"))
}

#' @rdname plot_dq
#' @export
plot.dq_multivariate <- function(x, cutoff = 0.05, ...)
{
  q <- stats::qnorm(p <- x$p.value)
  expected <- stats::qnorm(stats::ppoints(nrow(x)))
  ggplot(data.frame(x = expected, y = q, color = factor(p < cutoff, levels = c(FALSE, TRUE))),
                  aes_string(x = "x", y = "y", color = "color")) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_point(show.legend = FALSE) +
    scale_color_manual(values = c("black", "red")) +
    xlab("Theoretical Normal Quantiles") +  ylab("Normal Quantiles of P-values") +
    ggtitle("QQ plot of Multivariate Outliers") +
    theme(text = element_text(size = 15, face = "bold"))
}

#' @rdname plot_dq
#' @export
plot.dq_pca <- function(x, ...)
{
  eig <- cumsum(x)/sum(x)
  dat <- data.frame(x = seq_along(x), y = eig)
  ggplot(dat, aes_string(x = "x", y = "y")) +
    geom_line() +
    geom_point() +
    ggtitle("Scree Plot of PCAs") +
    xlab("PCA") + ylab("Variance Explained") +
    theme(text = element_text(size = 15, face = "bold"))
}
