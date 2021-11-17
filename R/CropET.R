#' Crop Evapotranspiration (ET_c)
#'
#' \code{ET_c} returns the value of crop evapotranspiration.
#'
#' This is a function to calculate the crop evapotranspiration.
#'
#' @param Kc A numeric scalar that denotes the crop coefficient (Kc).
#' @param ETo A numeric scalar that denotes the evapotranspiration rate from the reference surface [mm].
#'
#' @return The function returns the value of crop evapotranspiration as a numeric scalar.
#'
#' @examples
#' # First example
#' ET_c(Kc = 0.6, ETo = 0.9)
#' # Second example
#' # Computing ET_c of the crop millet planted in Sahiwal, Pakistan
#' # for a specific day in the initial growth stage
#' ## Loading the relevant Kc dataset
#' data(Kc_Cereals)
#' ## Latitude in decimal degree
#' latdeg = 31.685
#' ## Date (2020 June 7)
#' pdate = "2020-06-07"
#' ## Maximum and minimum temperatures in celsius
#' temp_max = 38
#' temp_min = 28
#' ## Actual duration of sunshine and maximum possible duration of sunshine or daylight in hours
#' actsunshine = 13
#' maxdaylight = 14
#' ## Elevation above sea level in meter
#' h = 170
#' ## Wind speed in the height 2m above the ground surface in m/s
#' ws = 2
#' ##  Evapotranspiration rate from the reference surface (ETo) in mm/day
#' ET_ref = ETo_FPM(u_2 = ws, e_a = 2.85, T_min = temp_min, T_max = temp_max,
#'                        phi_deg = latdeg, elev = h, date = pdate, n = actsunshine, N = maxdaylight)
#' ## Crop ET     
#' CrET = ET_c(Kc = Kc_Cereals$Kc_ini[12], ETo = ET_ref)
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @seealso \code{\link{ETo_FPM}, \link{ETo_Hrg}, \link{ETo_Pan}}.
#'
#'
#' @export
ET_c <- function(Kc, ETo) {

  Kc * ETo
}
