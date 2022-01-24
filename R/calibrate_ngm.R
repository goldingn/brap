#' Calibrate a next generation matrix (NGM) to have a specified reproduction
#' number.
#'
#' @title Calibrate a Next Generation Matrix
#' @param ngm_unscaled an unscaled next generation matrix
#' @param R the target reproduction number of the next generation smatrix
#' @return a calibrate next generation matrix
#' @author Nick Golding
#' @export
calibrate_ngm <- function(ngm_unscaled, R = 1) {

  R_raw <- Re(eigen(ngm_unscaled)$values[1])
  scaling <- R / R_raw
  ngm_unscaled * scaling

}
