
is.numericish <- function(x)
  (is.numeric(x) || inherits(x, "difftime") || is.Date(x) || inherits(x, "POSIXt")) && length(unique(x)) >= getOption("dq.min.unique", 10)

fix.dates <- function(dat)
{
  idx <- vapply(dat, function(x) is.Date(x) || inherits(x, "POSIXt") || inherits(x, "difftime"), NA)
  dat[idx] <- lapply(dat[idx], as.numeric)
  dat
}
