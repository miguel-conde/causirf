% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_sto_causal.R
\encoding{UTF-8}
\name{evr}
\alias{evr}
\title{Compute the Explained Variance Ratio (EVR)}
\usage{
evr(obj)
}
\arguments{
\item{obj}{an \code{est_causal} object generated by \code{\link[causirf:est_irf]{causirf::est_irf}}}
}
\value{
an estimate for the EVR of \code{obj}
}
\description{
Compute the Explained Variance Ratio (EVR)
}
\examples{
\dontrun{
library(causirf)
library(ggplot2)

data(temp_co2_data)

obj_causal_temp_co2 <- est_irf(temp_co2_data, "temp", "co2", J = 20, lambda = 10)

autoplot(obj_causal_temp_co2)

evr(obj_causal_temp_co2)
}
}
\seealso{
\code{\link[causirf:autoplot.est_causal]{causirf::autoplot}},
\code{\link[causirf:est_irf]{causirf::est_irf}}
}
