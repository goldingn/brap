set.seed(2022-8-30)
n_groups <- 10
contact_matrix <- matrix(
  runif(n_groups ^ 2, 5, 8),
  n_groups,
  n_groups
)

contact_matrix
cases <- sim_cases(n_cases = 7, n_groups = n_groups)

cases_matrix <- cases_to_matrix(cases = cases,
                                n_groups = n_groups)

ngm <- calibrate_ngm(ngm_unscaled = contact_matrix, R = 2.5)

case_counts <- project(ngm = ngm,
                       infections_matrix = cases_matrix,
                       n_days_project = 10)

case_counts

test_that("cases_to_matrix returns appropriate type and dimension", {
  expect_type(cases_matrix, "integer")
  expect_equal(nrow(cases_matrix), 8)
  expect_equal(ncol(cases_matrix), 10)
})
