#' @title Calculate FI Over Groups
#' @description Calculate a vector of the force of infection into each group,
#'   from a vector (of length n_groups) giving the number of new infections on
#'   a given day.
#' @param infections a vector of infections (of length n_groups)
#' @param ngm a next generation matrix with dimension n_groups
#' @return a vector of length n_groups giving the force of infection into each
#'   group
#' @author Nick Golding
#' @noRd
foi_group <- function(infections, ngm) {
  (ngm %*% infections)[, 1]
}
