#' FAO Penman-Monteith Reference Evapotranspiration (ETo) Equation
#'
#' \code{ETo_FPM} returns the value of evapotranspiration rate from the reference surface.
#'
#' This is a function to calculate the evapotranspiration rate from the reference surface (ETo)
#'    by using FAO Penman-Monteith equation which is one of the most-widely used equations
#'    for this purpose. If \code{Delta} is missing, the function uses the argumnet \code{T_mean}
#'    to compute its value. If \code{T_mean} is missing, the function needs \code{T_min} and \code{T_max}
#'    to compute \code{T_mean}. If \code{R_n} is missing, the arguments \code{phi_deg}, \code{date},
#'    \code{n}, \code{N}, \code{elev}, \code{T_min}, \code{T_max}, and \code{e_a} must be present.
#'    If \code{gamma} is missing, the function needs \code{elev} to compute \code{gamma}.
#'    If \code{e_s} is missing, the arguments \code{T_min} and \code{T_max} must be present for
#'    computation of \code{e_s}. If \code{e_a} is missing, one of the arguments \code{T_dew} or 
#'    \code{T_min} must be present in order to compute \code{e_a}. If \code{T_dew} is missing and \code{T_min}
#'    is present, then \code{T_dew} is computed based on the \code{T_min} value. If \code{u_2} is missing,
#'    the function needs the values of the arguments \code{u_z} and \code{z} to compute \code{u_2}.
#'
#' @param T_mean Optional. A numeric scalar that denotes the average temperature [C].
#' @param T_min Optional. A numeric scalar that denotes the daily minimum temperature [C].
#' @param T_max Optional. A numeric scalar that denotes the daily maximum temperature [C].
#' @param R_n Optional. A numeric scalar that denotes the net radiation at the crop surface [\eqn{MJ/(m^2\times day)}].
#' @param gamma Optional. A numeric scalar that denotes the psychrometric constant [kPa/C].
#' @param e_s Optional. A numeric scalar that denotes the saturation vapour pressure [kPa].
#' @param e_a Optional. A numeric scalar that denotes the actual vapour pressure [kPa].
#' @param T_dew Optional. A numeric scalar that denotes the dew point temperature [C].
#' @param phi_deg Optional. A numeric scalar that denotes the latitude in terms of degree [degree].
#' @param elev Optional. A numeric scalar that denotes the elevation above the sea level [m].
#' @param date Optional. A character string that denotes the date in the format "Year-Month-Day" or "Year/Month/Day".
#' @param Delta Optional. A numeric scalar that denotes the slope vapour pressure curve [kPa/C].
#' @param u_2 A numeric scalar that denotes the wind speed at the height 2m above the ground surface [m/s].
#' @param u_z A numeric scalar that denotes the wind speed at the height \code{z} above the ground surface [m/s].
#' @param z A numeric scalar that denotes the height above the ground surface where the wind speed has been measured [m].
#' @param G Optional. A numeric scalar that denotes the soil heat flux density [\eqn{MJ/(m^2\times day)}]. The default is \code{G=0}.
#' @param n Optional. A numeric scalar that denotes actual duration of sunshine [hour]
#' @param N Optional. A numeric scalar that denotes maximum possible duration of sunshine or daylight hours [hour]
#' @param a_s Optional. A numeric scalar that denotes regression constant, expressing the fraction ofextraterrestrial radiation
#'    reaching the earth on overcast days (n = 0). The default is \code{a_s = 0.25}.
#' @param b_s Optional. A numeric scalar that denotes fraction of extraterrestrial radiation reaching the earth on clear days
#'    (\eqn{n = N}). The default is \code{b_s = 0.5}
#'
#' @return The function returns the value of evapotranspiration rate from the reference surface as a numeric scalar.
#'
#' @examples
#' ETo_FPM(u_2 = 2, e_a = 2.85, T_min = 25.6, T_max = 34.8, phi_deg = 13.73,
#'              elev = 2, date = '2002-04-15', n = 8.5, N = 12.31)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ETo_Hrg}} for Hargreaves Equation.
#' @export
ETo_FPM <- function(Delta = SlpSVPC(T_mean), T_mean = (T_min + T_max)/2, R_n = NULL, G = 0,
                             gamma = PsyCon(AtmPres(elev)), u_2 = NULL, u_z = NULL, z = NULL,
                             e_s = MSVP(T_max, T_min), T_dew = NULL, e_a = NULL,
                             T_min = NULL, T_max = NULL, phi_deg = NULL, elev = NULL,
                             date = NULL, n = NULL, N = NULL, a_s = 0.25, b_s = 0.5) {

  if(is.null(T_dew) & !is.null(T_min)){
    T_dew <- T_min
  }

  if(is.null(e_a)){
    e_a <- SatVP(T_dew)}

  if(is.null(R_n)) {
    phi <- DD2Rad(phi_deg = phi_deg)
    delta <- SolDec(date = date)
    omega_s <- SunHA(phi = phi, delta = delta)
    d_r <- EarSunDis(date = date)
    R_a <- ExRad(d_r = d_r, omega_s = omega_s, phi = phi, delta = delta, G_sc = 0.0820)
    R_s <- SolRad(n = n, N = N, R_a = R_a)
    R_so <- CSSRad(a_s, b_s, elev = elev, R_a)
    R_ns <- NSRad(R_s)
    R_nl <- NLRad(T_max, T_min, e_a, R_s, R_so)
    R_n <- NRad(R_ns, R_nl)
  }

  if(is.null(u_2)){
    u_2 = WndSp2m(u_z, z)
  }

  (0.408 * Delta * (R_n - G) + gamma * (900 / (T_mean + 273)) * u_2 * (e_s - e_a)) / (Delta + gamma * (1 + 0.34 * u_2))
}
