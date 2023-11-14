test_that("ETo_FPM calculates the crop evapotranspiration correctly", {
  expect_equal(ETo_FPM(u_2 = 2, e_a = 2.85, T_min = 25.6, T_max = 34.8, phi_deg = 13.73, elev = 2, date = '2002-04-15', n = 8.5, N = 12.31), 5.755692, tolerance = 1e-3)
})
#> Test passed

test_that("ETo_Hrg calculates the crop evapotranspiration correctly", {
  expect_equal(ETo_Hrg(T_min = 19, T_max = 25, R_a = 32), 7.175241, tolerance = 1e-3)
})
#> Test passed