#' @title Simulate one timestep of new infections
#'
#' @description Simulate new infections in days and groups, emanating from the
#'   new infections on people infected on a specific day.
#'
#' @param infections_matrix a date-by-group matrix of new infections collated so
#'   far
#' @param day the day (an integer index to the rows in 'infections_matrix') from
#'   which to simulate new infections
#' @param k the dispersion parameter of a negative binomial distribution over
#'   new infections
#' @param ngm a next generation matrix with dimension n_groups
#' @return matrix with the number of columns matching the number n_groups
#' @author Nick Golding
#' @noRd
sim_infections <- function(infections_matrix, day, ngm, k = 0.1) {

  # Use fact that sum of negative binomials in k, mu, parameterisation is
  # sum(k), sum(mu). Mean new infections into each group is FOI into each group
  # (sum_j of cases_i times R_ij), and dispersion is sum(cases) * k

  # get mu parameter of NB representing sum of NBs with k, mu parameterisation &
  # get k parameter (previous cases is total number of IIDs going into each
  # group)

  infections_today <- infections_matrix[day, ]
  n_days <- nrow(infections_matrix)
  n_infections_today <- sum(infections_today)

  # get the FOI into each group, on each day, from infections on this day
  foi <- infections_today %>%
    foi_group(ngm) %>%
    foi_time(day, n_days)

  new_infections_matrix <- foi
  new_infections_matrix[] <- stats::rnbinom(
    n = length(new_infections_matrix),
    size = k * n_infections_today,
    mu = foi
  )

  new_infections_matrix[is.na(new_infections_matrix)] <- 0
  infections_matrix + new_infections_matrix

}

