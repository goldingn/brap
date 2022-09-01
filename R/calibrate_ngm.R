#' @title Calibrate a Next Generation Matrix
#' @description Calibrate a next generation matrix (NGM) to have a specified
#'   reproduction number.
#' @param ngm_unscaled an unscaled next generation matrix
#' @param R the target reproduction number of the next generation matrix
#' @return a calibrate next generation matrix
#' @author Nick Golding
#' @examples
#' set.seed(2022-8-30)
#' n_groups <- 10
#' contact_matrix <- matrix(
#'   runif(n_groups ^ 2, 5, 8),
#'   n_groups,
#'   n_groups
#'   )
#' contact_matrix
#'
#' ngm <- calibrate_ngm(ngm_unscaled = contact_matrix, R = 2.5)
#'
#' ngm
#'
#' @export
calibrate_ngm <- function(ngm_unscaled, R = 1) {

  R_raw <- Re(eigen(ngm_unscaled)$values[1])
  scaling <- R / R_raw
  ngm_unscaled * scaling

}
