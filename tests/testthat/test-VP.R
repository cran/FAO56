test_that("AtmPres calculates the atmospheric pressure (P) correctly", {
  expect_equal(AtmPres(z = 1800), 81.7558, tolerance = 1e-3)
})
#> Test passed

test_that("PsyCon calculates the psychrometric constant (gamma) correctly", {
  expect_equal(PsyCon(P = 81.8), 0.05437588, tolerance = 1e-3)
})
#> Test passed

test_that("SatVP calculates the saturation vapour pressure at the air temperature T (e0T) correctly", {
  expect_equal(SatVP(T = 15), 1.705346, tolerance = 1e-3)
})
#> Test passed

test_that("RelHum calculates the relative humidity (R_H) correctly", {
  expect_equal(RelHum(e_a = 1.91, e0T = 2.39), 79.91632, tolerance = 1e-1)
})
#> Test passed

test_that("MSVP calculates the mean saturation vapour pressure (e_s)", {
  expect_equal(MSVP(T_max = 25, T_min = 18), 2.615883, tolerance = 1e-2)
})
#> Test passed

test_that("SlpSVPC calculates the slope of saturation vapour pressure curve (Delta) correctly", {
  expect_equal(SlpSVPC(T = 20), 0.14474, tolerance = 1e-4)
})
#> Test passed

test_that("WndSp2m calculates the wind speed at the height 2m above the ground surface (u_2) correctly", {
  expect_equal(WndSp2m(u_z = 3.2, z = 10), 2.4, tolerance = 1e-2)
})
#> Test passed