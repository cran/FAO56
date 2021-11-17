test_that("ExRad calculates the extraterrestrial radiation for daily periods (R_a) correctly", {
  expect_equal(ExRad(d_r = 0.985, omega_s = 1.527, phi = -0.35, delta = 0.12, G_sc = 0.0820), 32.1738, tolerance = 1e-2)
})
#> Test passed

test_that("JulDate calculates the Julian date correctly", {
  expect_equal(JulDate(date = '2002-05-22'), 141)
})
#> Test passed

test_that("EarSunDis calculates the inverse earth-sun distance (d_r) correctly", {
  expect_equal(EarSunDis(date = '2005/09/03'), 0.9843266, tolerance = 1e-3)
})
#> Test passed

test_that("SolDec calculates the solar declination (delta) correctly", {
  expect_equal(SolDec(date = '2002-09-03'), 0.1263696, tolerance = 1e-3)
})
#> Test passed

test_that("SunHA calculates the sunset hour angel (omega_s)", {
  expect_equal(SunHA(phi = -0.35, delta = 0.12), 1.526767, tolerance = 1e-3)
})
#> Test passed

test_that("DD2Rad converts degree to radian correctly", {
  expect_equal(DD2Rad(phi_deg = 60), pi/3, tolerance = 1e-4)
})
#> Test passed

test_that("DH calculates the daylight hours (N) correctly", {
  expect_equal(DH(omega_s = 1.527), 11.66542, tolerance = 1e-2)
})
#> Test passed

test_that("SolRad calculates the solar radiation (R_s) correctly", {
  expect_equal(SolRad(n = 7.1, N = 10.9, R_a = 25.1), 14.44977, tolerance = 1e-2)
})
#> Test passed

test_that("CSSRad calculates the clear-sky solar radiation (R_so) correctly", {
  expect_equal(CSSRad(a_s = 0.25, b_s = 0.50, R_a = 25.1), 18.8, tolerance = 1e-1)
})
#> Test passed

test_that("NSRad calculates the net shortwave radiation (R_ns) correctly", {
  expect_equal(NSRad(R_s = 14.5), 11.165, tolerance = 1e-2)
})
#> Test passed

test_that("NLRad calculates the net longwave radiation (R_nl) correctly", {
  expect_equal(NLRad(T_max = 25.1, T_min = 19.1, e_a = 2.1, R_s = 14.5, R_so = 18.8), 3.534035, tolerance = 1e-3)
})
#> Test passed

test_that("NLRad calculates the net radiation (R_n) correctly", {
  expect_equal(NRad(R_ns = 11.1, R_nl = 3.5), 7.6, tolerance = 1e-1)
})
#> Test passed