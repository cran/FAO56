#' Atmospheric Pressure (P)
#'
#' \code{AtmPres} returns the value of atmospheric pressure.
#'
#' This is a function to calculate the atmospheric pressure [kPa] based on the elevation above the sea level.
#'
#' @param z A numeric scalar that denotes elevation above sea level [m].
#'
#' @return The function returns the value of the atmospheric pressure as a numeric scalar.
#'
#' @examples
#' AtmPres(z = 1800)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{PsyCon}}.
#' @export
AtmPres <- function(z) {
  101.3 * ((293 - 0.0065 * z) / 293)^5.26
}

##############################################################################
#' Psychrometric Constant (gamma)
#'
#' \code{PsyCon} returns the value of psychrometric constant.
#'
#' This is a function to calculate the psychrometric constant [kPa/C].
#'
#' @param P A numeric scalar that denotes the atmospheric pressure [kPa].
#' @param lambda A numeric scalar that denotes the latent heat of vaporization, 2.45 [MJ/kg].
#' @param c_p A numeric scalar that denotes the specific heat at constant pressure, 1.013*10^(-3) [MJ/(kg*C).
#' @param eps A numeric scalar that denotes the ratio molecular weight of water vapour/dry air = 0.622.
#'
#' @return The function returns the value of the psychrometric constant [kPa/C] as a numeric scalar.
#'
#' @examples
#' PsyCon(P = 81.8)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{AtmPres}}.
#' @export
PsyCon <- function(P, lambda = 2.45, c_p = 1.013 * 10^(-3), eps = 0.622) {

  (c_p * P) / (eps * lambda)
}

##############################################################################
#' Mean Daily Air Temperature (T_mean)
#'
#' \code{MeanTemp} returns the value of mean daily air temperature [C].
#'
#' This is a function to calculate the mean daily air temperature [C].
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of the mean daily air temperature [C] as a numeric scalar.
#'
#' @examples
#' MeanTemp(T_min = 5, T_max = 35)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{AtmPres}}.
#' @export
MeanTemp <- function(T_min, T_max) {
  (T_min + T_max) / 2
}

##############################################################################
#' Saturation Vapour Pressure at a specific Air Temperature (e0T)
#'
#' \code{SatVP} returns the value of saturation vapour pressure at the air temperature \code{Temp} [kPa].
#'
#' This is a function to calculate the saturation vapour pressure at the air temperature \code{Temp} [kPa].
#'
#' @param Temp A numeric scalar that denotes the air temperature [C].
#'
#' @return The function returns the value of the saturation vapour pressure at the air temperature \code{Temp} [kPa] as a numeric scalar.
#'
#' @examples
#' SatVP(Temp = 25)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{MSVP}}.
#' @export
SatVP <- function(Temp) {
  0.6108 * exp(17.27 * Temp / (Temp + 237.3))
}

##############################################################################
#' Relative Humidity (RH)
#'
#' \code{RelHum} returns the value of relative humidity.
#'
#' This is a function to calculate the relative humidity.
#'
#' @inheritParams ETo_FPM
#' @param e0T A numeric scalar that denotes the saturation vapour pressure at a specific air temperature [kPa].
#'
#' @return The function returns the value of the relative humidity as a numeric scalar.
#'
#' @examples
#' RelHum(e_a = 0.7, e0T = 0.9)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SatVP}}.
#' @export
RelHum <- function(e_a, e0T) {
  100 * e_a / e0T
}

##############################################################################
#' Mean Saturation Vapour Pressure (e_s)
#'
#' \code{MSVP} returns the value of mean saturation vapour pressure.
#'
#' This is a function to calculate the mean saturation vapour pressure [kPa].
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of the mean saturation vapour pressure [kPa] as a numeric scalar.
#'
#' @examples
#' MSVP(T_max = 35, T_min = 1)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SatVP}}.
#' @export
MSVP <- function(T_max, T_min) {
  (SatVP(T_max) + SatVP(T_min)) / 2
}

##############################################################################
#' Slope of Saturation Vapour Pressure Curve (Delta)
#'
#' \code{SlpSVPC} returns the value of slope of saturation vapour pressure curve at a specific air temperature.
#'
#' This is a function to calculate the slope of saturation vapour pressure curve at the air temperature \code{Temp} [kPa/C].
#'
#' @inheritParams SatVP
#'
#' @return The function returns the value of the slope of saturation vapour pressure curve at air temperature \code{Temp} as a numeric scalar.
#'
#' @examples
#' SlpSVPC(Temp = 25)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SatVP}}.
#' @export
SlpSVPC <- function(Temp) {
  4098 * (0.6108 * exp(17.27 * Temp / (Temp + 237.3))) / (Temp + 237.3)^2
}
