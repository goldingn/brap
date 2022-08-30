n_groups <- 10
cases <- sim_cases(n_cases = 7, n_groups = n_groups)
test_that("sim_cases returns appropriate type and dimension", {
  expect_type(cases, "list")
  expect_equal(nrow(cases), 7)
  expect_equal(ncol(cases), 3)
})

test_that("classes and names of sim_cases is correct",{
  expect_snapshot_value(
    lapply(cases, class)
  )
})
