#' @title Convert a Case Linelist Dataframe to a Matrix
#' @description Convert a linelist of cases (in the same format as produced by
#'   [sim_cases()] into a date-by-group matrix of the number of new infections
#'   on that day in that group. The case linelist must have columns: 'case_id'
#'   (an identifier, in any format); 'group', an integer giving the columns/row
#'   of the next generation matrix to which the case belongs; and
#'   'infection_date' a Date object giving the date of infection of the case.
#' @param cases a case linelist dataframe with three columns as described below
#' @param n_groups the total number of groups in the next generation matrix
#' @return a matrix with rows giving dates (all dates between the minimum and
#'   maximum date in 'cases'), and columns giving groups (n_groups, in numerical
#'   order)
#' @author Nick Golding
#' @export
#' @examples
#' set.seed(2022-8-30)
#' n_groups <- 10
#' cases <- sim_cases(n_cases = 7, n_groups = n_groups)
#' cases
#' cases_matrix <- cases_to_matrix(cases = cases,
#'                                 n_groups = n_groups)
#' cases_matrix
cases_to_matrix <- function(cases, n_groups = max(cases$group)) {

  cases <- cases %>%
    dplyr::select(
      -case_id
    ) %>%
    dplyr::group_by(
      group, infection_date
    ) %>%
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      group = seq_len(n_groups),
      infection_date = seq(
        min(infection_date),
        max(infection_date),
        by = 1
      ),
      fill = list(count = 0)
    ) %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = count,
      names_prefix = "group_"
    ) %>%
    tibble::column_to_rownames(
      "infection_date"
    ) %>%
    as.matrix()
}
