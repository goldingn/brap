#' @title Generation Interval Density Function
#' @description The probability mass function of a discrete generation interval
#'   distribution
#' @param days the difference in days between pairs of infections at which to
#'   evaluate the density of the distribution
#' @param meanlog the meanlog parameter of the underlying continuous lognormal
#'   distribution
#' @param sdlog the sdlog parameter of the underlying continuous lognormal
#'   distribution
#' @return a vector of probabilities of the same length as 'days'
#' @author Nick Golding
#' @noRd
gi_pmf <- function (days, meanlog = 1.375738, sdlog = 0.5665299) {
  lower <- stats::plnorm(days, meanlog = meanlog, sdlog = sdlog)
  upper <- stats::plnorm(days + 1, meanlog = meanlog, sdlog = sdlog)
  upper - lower
}
