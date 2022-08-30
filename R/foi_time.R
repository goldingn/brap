#' @title Calculate FOI Over Time
#' @description Calculate a matrix the force of infection (FOI) over time into
#'   each group, from a vector of the FOI into each group emanating from the
#'   new infections on a single day. Used internally inside [sim_infections()]
#'   after [foi_group()].
#' @param foi_groups a vector of the FOI into each group from a
#'   single day
#' @param day an index to n_Days giving the day from which the FOI emanates
#' @param n_days the total number of days to consider
#' @return matrix with the number of rows being the number of `n_days`, and the
#'   number of columns being the same as the number of `foi_groups`.
#' @author Nick Golding
#' @keywords internal
#' @noRd
foi_time <- function (foi_groups, day, n_days) {
  days_mat <- matrix(NA, nrow = n_days, ncol = length(foi_groups))
  day_diff <- row(days_mat) - day
  prob <- gi_pmf(day_diff)
  sweep(prob, 2, foi_groups, FUN = "*")
}
