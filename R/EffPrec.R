#' Effective Monthly Precipitation (P_eff)
#'
#' \code{EffPrec} returns the value of effective precipitation.
#'
#' This is a function to calculate the effective precipitation [mm]. The function formula has been developed
#'    for Iran where the mean annual precipitation is about 250 mm. It may be used for similar semi-arid areas,
#'    but it is not recommended for the areas with different climate.
#'
#' @param P_tot A numeric scalar that denotes the total monthly precipitation [mm].
#'
#' @return The function returns the value of effective monthly precipitation [mm].
#'
#' @examples
#' EffPrec(P_tot = 450)
#'
#' @export
EffPrec <- function(P_tot) {
  if (P_tot <= 250) {
    P_tot * (125 - 0.2 * P_tot) / 125
  } else {
    125 + 0.1 * P_tot
  }
}
