#' @title Simulate Future Infections
#' @description Simulate daily new infections in each group forward in time
#'   with the structured branching process model.
#' @param ngm a next generation matrix giving the average rate of new infection
#'   between different population groups
#' @param infections_matrix a day-by-group matrix of the number of initial new
#'   infections per day to simulate form
#' @param n_days_project the number of days to simulate beyond the end of
#'   infections_matrix
#' @return a matrix of new infection sper day in each group under a single
#'   simulation
#' @author Nick Golding
#' @export
project <- function(ngm, infections_matrix, n_days_project = 10) {

  n_groups <- ncol(infections_matrix)

  # set up new infections matrix with empty spaces
  new_days <- matrix(0, nrow = n_days_project, ncol = n_groups)
  infections_matrix <- rbind(infections_matrix, new_days)
  n_days <- nrow(infections_matrix)

  # iterate dynamics
  for (day in seq_len(n_days)) {
    infections_matrix <- sim_infections(infections_matrix, ngm = ngm, day = day)
  }

  infections_matrix

}
