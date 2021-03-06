% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_outliers.R
\name{clean_tech_reps}
\alias{clean_tech_reps}
\title{Pivot and remove outlying technical replicates from columns a:c}
\usage{
clean_tech_reps(df)
}
\arguments{
\item{df}{A tibble containing technical replicates in columns labeled a:c.}
}
\value{
A tibble containing summarized data for the technical replicates.
}
\description{
This function will combine columns labeled a:c, group by the remaining
variables in the tibble, and remove outliers based on the median absolute
deviation.
}
