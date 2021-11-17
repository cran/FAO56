#' Extraterrestrial Radiation for Daily Periods (R_a)
#'
#' \code{ExRad} returns the value of extraterrestrial radiation [\eqn{MJ/(m^2\times day)}].
#'
#' This is a function to calculate the extraterrestrial radiation.
#'
#' @param d_r A numeric scalar that denotes the inverse relative earth-sun distance.
#' @param omega_s A numeric scalar that denotes the sunset hour angle [rad].
#' @param phi A numeric scalar that denotes the latitude [rad].
#' @param delta A numeric scalar that denotes the solar declination [rad].
#' @param G_sc A numeric scalar that denotes the solar constant = 0.0820 [\eqn{MJ/(m^2\times min)}].
#'
#' @return The function returns the value of extraterrestrial radiation as a numeric scalar.
#'
#' @examples
#' ExRad(d_r = 0.985, omega_s = 1.527, phi = -0.35, delta = 0.12)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{EarSunDis}, \link{SunHA}, \link{SolDec}, \link{SolRad}, \link{CSSRad}}.
#'
#' @export
ExRad <- function(d_r, omega_s, phi, delta, G_sc = 0.0820) {

  (24 * 60 / pi) * G_sc * d_r * (omega_s * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(omega_s))
}

##############################################################################
#' Julian Date
#'
#' \code{JulDate} returns Julian Date.
#'
#' This is a function to calculate Julian Date.
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns Julian Date as a numeric scalar.
#'
#' @examples
#' JulDate(date = "2020-06-25")
#' JulDate(date = "2020/06/25")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{DH}, \link{EarSunDis}, \link{SolDec}}.
#'
#' @export
JulDate <- function(date) {
  J <- as.POSIXlt(date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
  as.numeric(J$yday)
}

##############################################################################
#' Inverse Earth-Sun Distance (d_r)
#'
#' \code{EarSunDis} returns the inverse earth-sun distance.
#'
#' This is a function to calculate the inverse earth-sun distance.
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of inverse relative earth-sun distance as a numeric scalar.
#'
#' @examples
#' EarSunDis("2020/08/25")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{JulDate}}.
#'
#' @export
EarSunDis <- function(date) {
  J <- JulDate(date)
  1 + 0.033 * cos(2 * pi * J / 365)
}

##############################################################################
#' Solar Declination (delta)
#'
#' \code{SolDec} returns the solar declination.
#'
#' This is a function to calculate the value of solar declination.
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of solar declination as a numeric scalar.
#'
#' @examples
#' SolDec("2020/08/25")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{JulDate}, \link{SunHA}}.
#'
#' @export
SolDec <- function(date) {
  J <- JulDate(date)
  0.409 * sin((2 * pi * J / 365) - 1.39)
}

##############################################################################
#' Sunset Hour Angel (omega_s)
#'
#' \code{SunHA} returns the value of sunset hour angel [rad].
#'
#' This is a function to calculate the sunset hour angel.
#'
#' @inheritParams ExRad
#'
#' @return The function returns the value of sunset hour angel as a numeric scalar.
#'
#' @examples
#' SunHA(phi = -0.35, delta = 0.12)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SolDec}}.
#'
#' @export
SunHA <- function(phi, delta) {
  acos(-tan(phi) * tan(delta))
}

##############################################################################
#' Degree to Radian Converter
#'
#' \code{DD2Rad} converts the value of an angel in the unit degree to the unit radian.
#'
#' This is a function to convert the degree unit to radian.
#'
#' @inheritParams ETo_FPM
#'
#' @return The function convert the value of an angel in the unit degree to the unit radian as a numeric scalar.
#'
#' @examples
#' DD2Rad(phi_deg = 60.73)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SunHA}}.
#'
#' @export
DD2Rad <- function(phi_deg) {
  pi * (phi_deg / 180)
}

##############################################################################
#' Daylight Hours (N)
#'
#' \code{DH} returns the value of daylight hours.
#'
#' This is a function to calculate the daylight hours.
#'
#' @inheritParams ExRad
#'
#' @return The function returns the value of daylight hours as a numeric scalar.
#'
#' @examples
#' DH(omega_s = 1.527)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SunHA}}.
#'
#' @export
DH <- function(omega_s) {
  24 * omega_s / pi
}

##############################################################################
#' Solar Radiation (R_s)
#'
#' \code{SolRad} returns the value of solar radiation.
#'
#' This is a function to calculate the solar radiation based on the land type. If one of the arguments \code{n} or \code{N}
#'    is missing, the function needs to use the values of the arguments \code{T_min}, \code{T_max}, and \code{region}.
#'    If calibrated values of \code{a_s} and \code{b_s} are available, they can replace the default values.
#'
#' @param R_a A numeric scalar that denotes extraterrestrial radiation [\eqn{MJ/(m^2\times day)}]
#' @param region A character string that introduce the type of region and can be assigned "inter" for interior locations
#'    and "coast" for coastal locations for Hargreaves radiation formula (alternative)
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of solar radiation based as a numeric scalar.
#'
#' @examples
#' SolRad(n = 7.1, N = 10.9, R_a = 25.1)
#' SolRad(R_a = 25.1, T_max = 30, T_min = 20, region = "inter")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ExRad}}.
#'
#' @export
SolRad <- function(n = NULL, N =NULL, a_s = 0.25, b_s = 0.5, R_a, T_max = NULL, T_min = NULL, region = NULL) {

 if((is.null(n) | is.null(N)) & (!is.null(T_max) & !is.null(T_min) & !is.null(region))){
  if(region == 'inter'){k_Rs <- 0.16}
  if(region == 'coast'){k_Rs <- 0.19}

  k_Rs * sqrt(T_max - T_min) * R_a
 }

  (a_s + b_s * (n / N)) * R_a
}

##############################################################################
#' Clear-Sky Solar Radiation (R_so)
#'
#' \code{CSSRad} returns the value of clear-sky solar radiation.
#'
#' This is a function to calculate the clear-sky solar radiation. The argument \code{elev} is needed when the calibrated values
#'    of \code{a_s} and \code{b_s} are not available.
#'
#' @inheritParams ETo_FPM
#' @inheritParams SolRad
#' @inheritParams AtmPres
#'
#' @return The function returns the value of clear-sky solar radiation as a numeric scalar.
#'
#' @examples
#' CSSRad(a_s = 0.27, b_s = 0.48, R_a = 25.1)
#' CSSRad(elev = 100, R_a = 25.1)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ExRad}}.
#'
#' @export
CSSRad <- function(a_s = 0.25, b_s = 0.5, elev = NULL, R_a) {

 if(!is.null(elev)){
  (0.75 + 2 * 10^(-5) * elev) * R_a
 }

  (a_s + b_s) * R_a
}

##############################################################################
#' Net Shortwave Radiation (R_ns)
#'
#' \code{NSRad} returns the value of net shortwave radiation.
#'
#' This is a function to calculate the net shortwave radiation.
#'
#' @param R_s A numeric scalar that denotes the incoming solar radiation [\eqn{MJ/(m^2\times day)}].
#'
#' @return The function returns the value of net shortwave radiation as a numeric scalar.
#'
#' @examples
#' NSRad(R_s = 14.5)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SolRad}, \link{NLRad}, \link{NRad}}.
#'
#' @export
NSRad <- function(R_s) {
  alpha <- 0.23
  (1 - alpha) * R_s
}
##############################################################################
# alpha: albedo or canopy reflection coefficient, which is 0.23 for the hypothetical grass reference crop [dimensionless]

##############################################################################
#' Net Longwave Radiation (R_nl)
#'
#' \code{NLRad} returns the value of net longwave radiation.
#'
#' This is a function to calculate the net longwave radiation [\eqn{MJ/(m^2\times day)}].
#'
#' @param R_so A numeric scalar that denotes clear-sky radiation [\eqn{MJ/(m^2\times day)}].
#' @inheritParams ETo_FPM
#' @inheritParams ETo_Hrg
#' @inheritParams NSRad
#'
#' @return The function returns the value of net solar or net shortwave radiation as a numeric scalar.
#'
#' @examples
#' NLRad(T_max = 25.1, T_min = 19.1, e_a = 2.1, R_s = 14.5, R_so = 18.8)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{CSSRad}, \link{NLRad}, \link{NRad}}.
#'
#' @export
NLRad <- function(T_max, T_min, e_a, R_s, R_so) {
  T_maxK <- T_max + 273.16
  T_minK <- T_min + 273.16
  sig <- 4.903 * 10^(-9)
  sig * (T_maxK^4 + T_minK^4) / 2 * (0.34 - 0.14 * sqrt(e_a)) * (1.35 * R_s / R_so - 0.35)
}

##############################################################################
#' Net Radiation (R_n)
#'
#' \code{NRad} returns the value of net radiation.
#'
#' This is a function to calculate the net radiation [\eqn{MJ/(m^2\times day)}].
#'
#' @param R_ns A numeric scalar that denotes net shortwave radiation [\eqn{MJ/(m^2\times day)}].
#' @param R_nl A numeric scalar that denotes net longwave radiation [\eqn{MJ/(m^2\times day)}].
#'
#' @return The function returns the value of net solar radiation as a numeric scalar.
#'
#' @examples
#' NRad(R_ns = 11.1, R_nl = 3.5)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{NLRad}, \link{NSRad}}.
#'
#' @export
## Net Radiation (R_n)
#' @export
NRad <- function(R_ns, R_nl) {
  R_ns - R_nl
}
