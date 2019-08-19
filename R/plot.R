


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




