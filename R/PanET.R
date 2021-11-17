#' Evapotranspiration based on Pan Evaporation Method
#'
#' \code{ETo_Pan} returns the value of reference evapotranspiration based on the pan evaporation method.
#'
#' This is a function to calculate the reference evapotranspiration [mm/day] based on the pan evaporation method.
#'
#' @param K_p A numeric scalar that denotes the pan coefficient.
#' @param E_pan A numeric scalar that denotes the pan evaporation [mm/day].
#'
#' @return The function returns the value of the reference evapotranspiration as a numeric scalar.
#'
#' @examples
#' ETo_Pan(K_p = 0.6, E_pan = 5)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ETo_FPM}, \link{ETo_Hrg}}.
#' @export
ETo_Pan <- function(K_p, E_pan) {
  K_p * E_pan
}

##############################################################################
#' Pan Coefficient (K_p)
#'
#' \code{PanCoef} returns the value of Pan Coefficient (K_p).
#'
#' This is a function to calculate the pan coefficient used in the pan evaporation method to calculate the reference evapotranspiration.
#'
#' @param RH_mean A numeric scalar that denotes the mean relative humidity. (\eqn{30\% <= RH_mean <= 84\%})
#' @param FET A numeric scalar that denotes the fetch, or distance of the identified surface type [m] (\eqn{1m <= FET <= 1000m})
#'    (grass or short green agricultural crop for case A, dry crop or bare soil for case B upwind of the evaporation pan)
#' @param type A character string that denotes the type of pan and can take the options "Class A"
#'    for Class A pan and "Colorado" for Colorado sunken pan.
#' @param fetch A character string that denotes the fetch state and can take the options "dry" and "green".
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of the pan coefficient.
#'
#' @examples
#' PanCoef(u_2 = 2, RH_mean = 50, FET = 3, type = "Class A", fetch = "dry")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ETo_Pan}, \link{MeanRH}}.
#' @export
PanCoef <- function(u_2, RH_mean, FET, type, fetch) {

  if((type == 'Class A') & (fetch == 'green')) {
    0.108 - 0.0286 * u_2 + 0.0422 * log(FET) + 0.1434 * log(RH_mean) - 0.000631 * (log(FET))^2 * log(RH_mean )
  }

  if((type == 'Class A') & (fetch == 'dry')) {
    0.61 + 0.00341 * RH_mean - 0.000162 * u_2 * RH_mean - 0.00000959 * u_2 * FET + 0.00327 * u_2 * log(FET) -
    0.00289 * u_2 * log(86.4 * u_2) - 0.0106 * log(86.4 * u_2) * log(FET) + 0.00063 * (log(FET ))^2 *  log(86.4 * u_2)
  }

  if((type == 'Colorado') & (fetch == 'green')) {
    0.87 + 0.119 * log(FET) - 0.0157 * (log(86.4 * u_2))^2 - 0.0019 * (log(FET))^2 *  log(86.4 * u_2) +
    0.013 * log(86.4 * u_2) * log(RH_mean) - 0.000053 * log(86.4 * u_2) * log(FET) * RH_mean
  }

  if((type == 'Colorado') & (fetch == 'dry')) {
   1.145 - 0.080 * u_2 +  0.000903 * (u_2)^2 * log(RH_mean) - 0.0964 * log(FET) + 0.0031 * u_2 * log(FET) +
   0.0015 * (log(FET))^2 *  log(RH_mean)
  }
}

##############################################################################
#' Mean Relative Humidity (RH_mean)
#'
#' \code{MeanRH} returns the value of mean relative humidity.
#'
#' This is a function to calculate the mean relative humidity.
#'
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of the mean relative humidity as a numeric scalar.
#'
#' @examples
#' MeanRH(T_min = 19, T_max = 26)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{SatVP}}.
#' @export
MeanRH <- function(T_min, T_max) {
  50 * SatVP(T_min) / SatVP(T_max) + 50
}