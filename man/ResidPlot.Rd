% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResidPlot.R
\name{ResidPlot}
\alias{ResidPlot}
\title{Generate a residuals vs. fitted values plot.}
\usage{
ResidPlot(model = NULL)
}
\arguments{
\item{model}{The model you'd like to analyze. Defaults to NULL.}
}
\description{
This function reimplements the first result from plot.lm() using ggplot2.
}
\examples{
ResidPlot(model = my_lm)
}
