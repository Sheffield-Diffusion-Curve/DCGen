% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elc_input.R
\name{input_gamma}
\alias{input_gamma}
\title{Input elicitation data as a gamma distribution}
\usage{
input_gamma(pars)
}
\arguments{
\item{pars}{a vector of [shape, rate]}
}
\value{
An elc.input object
}
\description{
Input elicitation data as a gamma distribution
}
\examples{
elc <- input_gamma(c(0.1, 0.1))
elc
}
