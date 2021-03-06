% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{interp_data}
\alias{interp_data}
\title{Interpolate concentrations when given unknowns and models}
\usage{
interp_data(df, std)
}
\arguments{
\item{df}{A tibble containing the raw data}

\item{std}{A tibble containing the model fits}
}
\value{
A tibble of interpolated values
}
\description{
Interpolate concentrations when given unknowns and models
}
