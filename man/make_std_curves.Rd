% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolate.R
\name{make_std_curves}
\alias{make_std_curves}
\title{Generate standard curves}
\usage{
make_std_curves(df)
}
\arguments{
\item{df}{A tibble containing columns \code{conc} and \code{value} for generating a
standard curve.}
}
\value{
A tibble with the original \code{data}, linear models, summary statistics,
and plots of the standard curves.
}
\description{
This function takes a tibble, groups by all variables except for concentration
and value, fits a linear model, extracts summary statistics, and generates
a plot. These linear models can then be used to interpolate values. Note that
one must have a \code{future::plan} in place.
}
