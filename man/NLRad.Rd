% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Radiation.R
\name{NLRad}
\alias{NLRad}
\title{Net Longwave Radiation (R_nl)}
\usage{
NLRad(T_max, T_min, e_a, R_s, R_so)
}
\arguments{
\item{T_max}{Optional. A numeric scalar that denotes the daily maximum temperature [C].}

\item{T_min}{Optional. A numeric scalar that denotes the daily minimum temperature [C].}

\item{e_a}{Optional. A numeric scalar that denotes the actual vapour pressure [kPa].}

\item{R_s}{A numeric scalar that denotes the incoming solar radiation [\eqn{MJ/(m^2\times day)}].}

\item{R_so}{A numeric scalar that denotes clear-sky radiation [\eqn{MJ/(m^2\times day)}].}
}
\value{
The function returns the value of net solar or net shortwave radiation as a numeric scalar.
}
\description{
\code{NLRad} returns the value of net longwave radiation.
}
\details{
This is a function to calculate the net longwave radiation [\eqn{MJ/(m^2\times day)}].
}
\section{Reference}{

\href{https://www.fao.org/3/x0490E/x0490e00.htm}{Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
   \emph{Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56}.
   Fao, Rome, 300(9), D05109.}
}

\examples{
NLRad(T_max = 25.1, T_min = 19.1, e_a = 2.1, R_s = 14.5, R_so = 18.8)

}
\seealso{
\code{\link{CSSRad}, \link{NLRad}, \link{NRad}}.
}
