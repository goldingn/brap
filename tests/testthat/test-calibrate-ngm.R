set.seed(2022-8-30)
n_groups <- 10
contact_matrix <- matrix(
  runif(n_groups ^ 2, 5, 8),
  n_groups,
  n_groups
)

contact_matrix

ngm <- calibrate_ngm(ngm_unscaled = contact_matrix, R = 2.5)

test_that("calibrate_ngm has appropriate type and dimensions", {
  expect_type(ngm, "double")
  expect_equal(nrow(ngm), 10)
  expect_equal(ncol(ngm), 10)
})
