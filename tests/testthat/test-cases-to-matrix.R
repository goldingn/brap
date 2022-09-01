set.seed(2022-8-30)
n_groups <- 10
cases <- sim_cases(n_cases = 7, n_groups = n_groups)
cases_matrix <- cases_to_matrix(cases = cases,
                                n_groups = n_groups)
cases_matrix

test_that("cases_to_matrix returns appropriate type and dimension", {
  expect_type(cases_matrix, "integer")
  expect_equal(nrow(cases_matrix), 13)
  expect_equal(ncol(cases_matrix), 10)
})
