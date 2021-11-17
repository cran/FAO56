#' Hargreaves Reference Evapotranspiration (ETo) Equation
#'
#' \code{ETo_Hrg} returns the value of the evapotranspiration rate from the reference surface.
#'
#' This is a function to calculate the evapotranspiration rate from the reference surface (ETo)
#' by using Hargreaves equation.
#'
#' @param R_a A numeric scalar denotes the extraterrestrial radiation [\eqn{MJ/(m^2\times day)}].
#' @inheritParams ETo_FPM
#'
#' @return The function returns the value of evapotranspiration rate from the reference surface calculated by
#' Hargreaves equation [mm/day] as a numeric scalar.
#'
#' @examples
#' ETo_Hrg(T_min = 19, T_max = 25, R_a = 32)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ETo_FPM}} for FAO Penman-Monteith Equation.
#' @export
ETo_Hrg <- function(T_min, T_max, R_a) {

  T_mean <- (T_min + T_max)/2

  0.0023 * (T_mean + 17.8) * sqrt(T_max - T_min) * R_a
}

