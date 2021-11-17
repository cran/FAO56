##############################################################################
#' Wind Speed at the height 2 m Above Ground Surface
#'
#' \code{WndSp} returns the value of wind speed at the height 2 m above the ground surface.
#'
#' This is a function to calculate the wind speed [m/s]. If \code{u_z} is missing, the function estimate the
#'    wind speed based on wind general or empirical classes.
#'
#' @param u_z Optional. A numeric scalar that denotes the measured wind speed at z m above ground surface [m/s].
#' @param z A numeric scalar that denotes the height of measurement above ground surface [m].
#' @param speed Optional. A character string that denotes the wind speed general class and can be assigned
#'    \code{"str"} for strong winds, \code{"mod2str"} for moderate to strong winds,
#'    \code{"lig2mod"} for light to moderate winds, and \code{"lig"} for light winds.
#'
#' @return The function returns the value of the wind speed [m/s] as a numeric scalar.
#'
#' @examples
#' WndSp2m(u_z = 3.2, z = 10)
#' WndSp2m(speed = "mod2str")
#'
#' @section Reference:
#' \href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#'    \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
#'    Fao, Rome, 300(9), D05109.}
#'
#' @export
WndSp2m <- function(u_z, z, speed = NULL) {
 if(missing(u_z) & !is.null(speed)){
  if (speed == "str") {
    5
  }
  if (speed == "mod2str") {
    4
  }
  if (speed == "lig2mod") {
    2
  }
  if (speed == "lig") {
    0.5
  }
 }else{
 (u_z * 4.87) / log(67.8 * z - 5.42)
 }
}
