#' Simulate a fake linelist of cases
#'
#' The case linelist must have columns: 'case_id' (an identifier, in any
#' format); 'group', an integer giving the columns/row of the next generation
#' matrix to which the case belongs; and 'infection_date' a Date object
#' giving the date of infection of the case.
#'
#' @title Simulate Fake Linelist Data
#' @param n_cases the number of cases to simulate
#' @param n_groups the number of population groups to which cases can belong
#' @param max_days_ago the maximum number of days in the past to simulate cases
#' @return a dataframe in the format described in details.
#' @author Nick Golding
#' @export
# simulate fake initial cases data
sim_cases <- function(n_cases, n_groups, max_days_ago = 14) {
  data.frame(
    case_id = seq_len(
      n_cases
    ),
    group = sample.int(
      n_groups,
      n_cases,
      replace = TRUE
    ),
    infection_date = Sys.Date() - sample.int(
      max_days_ago,
      n_cases,
      replace = TRUE
    )
  )
}
