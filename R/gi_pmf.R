#' The probability mass function of a discrete generation interval distribution
#'
#' @title Generation Interval Density Function
#' @param days the difference in days between pairs of infections at which to
#'   evaluate the density fo the distribution
#' @param meanlog the meanlog parameter of the underlying continuous lognormal
#'   distribution
#' @param sdlog the sdlog parameter of the underlying continuous lognormal
#'   distribution
#' @return a vector of probabilities of the same length as 'days'
#' @author Nick Golding
#' @noRd
gi_pmf <- function (days, meanlog = 1.375738, sdlog = 0.5665299) {
  lower <- plnorm(days, meanlog = meanlog, sdlog = sdlog)
  upper <- plnorm(days + 1, meanlog = meanlog, sdlog = sdlog)
  upper - lower
}
