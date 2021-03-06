% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elc_agg.R
\name{aggregate_elicitations}
\alias{aggregate_elicitations}
\alias{print.elc.agg}
\title{Pool elicitations data}
\usage{
aggregate_elicitations(elcs)

\method{print}{elc.agg}(elcs, ...)
}
\arguments{
\item{elcs}{a list of elicitation inputs}

\item{...}{additional arguments passed to the integrator or to the methods}
}
\value{
An aggregated elicitation object
}
\description{
Pool elicitations data
}
\examples{
expert1 <- input_norm(c(10, 2))
expert2 <- input_gamma(c(0.1, 0.01))

# Aggregated expert
experts <- aggregate_elicitations(list(expert1, expert2))
experts
}
