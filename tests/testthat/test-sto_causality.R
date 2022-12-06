
test_that("Causal estimate is performed", {
  obj_causal_temp_co2 <- est_irf(temp_co2_data, "temp", "co2", J = 20, lambda = 10)

  expect_s3_class(obj_causal_temp_co2, "list")
  expect_s3_class(obj_causal_temp_co2, "est_causal")
})

test_that("a causal estimate object can be (gg)plottted", {
  obj_causal_temp_co2 <- est_irf(temp_co2_data, "temp", "co2", J = 20, lambda = 10)
  p <- autoplot(obj_causal_temp_co2)

  expect_s3_class(p, "ggplot")
})

test_that("the explained variance ratio can be computed ", {
  obj_causal_co2_temp <- est_irf(temp_co2_data, "co2", "temp", J = 20, lambda = 10)
  res <- evr(obj_causal_co2_temp)

  expect_equal(as.numeric(res), 0.00156774334)
})
