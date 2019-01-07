#' Input elicitation data as a gamma distribution
#'
#' @param pars a vector of [shape, rate]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_gamma(c(0.1, 0.1))
#' elc
input_gamma <- function(pars) {
  pars <- as.numeric(pars)[1:2]

  names(pars) <- c("Shape", "Rate")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- qgamma(pr, pars[1], pars[2])
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "Gamma",
    "Values" = qgamma(c(0.25, 0.5, 0.75), pars[1], pars[2]),
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {rgamma(n, pars[1], pars[2])},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data as a normal distribution
#'
#' @param pars a vector of [mean, std]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_norm(c(0.1, 0.1))
#' elc
input_norm <- function(pars) {
  pars <- as.numeric(pars)[1:2]

  names(pars) <- c("Mean", "SD")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- qnorm(pr, pars[1], pars[2])
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "Normal",
    "Values" = qnorm(c(0.25, 0.5, 0.75), pars[1], pars[2]),
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {rnorm(n, pars[1], pars[2])},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data as a normal distribution
#'
#' @param pars a vector of [log(mean), log(std)]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_lnorm(c(2.5, 1))
#' elc
input_lnorm <- function(pars) {
  pars <- as.numeric(pars)[1:2]

  names(pars) <- c("Log(Mean)", "Log(SD)")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- qlnorm(pr, pars[1], pars[2])
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "Log Normal",
    "Values" = qlnorm(c(0.25, 0.5, 0.75), pars[1], pars[2]),
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {rlnorm(n, pars[1], pars[2])},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data as a student t distribution
#'
#' @param pars a vector of [mean, std, df]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_t(c(10, 1, 20))
#' elc
input_t <- function(pars) {
  pars <- as.numeric(pars)[1:3]

  names(pars) <- c("Mean", "SD", "d.f.")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- (qt(pr, pars[3]) + pars[1]) * pars[2]
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "StudentT",
    "Values" = (qt(c(0.25, 0.5, 0.75), pars[3]) + pars[1]) * pars[2],
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {(rt(n, pars[3]) + pars[1]) * pars[2]},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data as a log student t distribution
#'
#' @param pars a vector of [log(mean), log(std), df]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_lt(c(2.5, 1, 20))
#' elc
input_lt <- function(pars) {
  pars <- as.numeric(pars)[1:3]

  names(pars) <- c("Log(Mean)", "Log(SD)", "d.f.")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- exp((qt(pr, pars[3]) + pars[1]) * pars[2])
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "LogStudentT",
    "Values" = exp((qt(c(0.25, 0.5, 0.75), pars[3]) + pars[1]) * pars[2]),
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {exp((rt(n, pars[3]) + pars[1]) * pars[2])},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data as a triangle distribution
#'
#' @param pars a vector of [peak, min, max]
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_triangle(c(10, 8, 12))
#' elc
input_triangle <- function(pars) {
  pars <- as.numeric(pars)[1:3]
  names(pars) <- c("Peak", "Min", "Max")

  pr <- seq(0.005, 0.995, 0.005)
  inv.cdf <- triangle::qtriangle(pr, pars[2], pars[3], pars[1])
  names(inv.cdf) <- pr
  res <- list(
    "Distribution" = "Triangle",
    "Values" = triangle::qtriangle(c(0.25, 0.5, 0.75), pars[2], pars[3], pars[1]),
    "Prob" = c(0.25, 0.5, 0.75),
    "rand" = function(n) {triangle::rtriangle(n, pars[2], pars[3], pars[1])},
    "Parameters" = pars,
    "InvCDF" = inv.cdf,
    "approx" = approxfun(pr[is.finite(inv.cdf)], inv.cdf[is.finite(inv.cdf)], rule=2)
  )
  class(res) <- "elc.input"
  res
}


#' Input elicitation data
#'
#' @param dist distribution of the input among ["Gamma", "Normal", "LogNormal", "StudentT", "Triangle"]
#' @param pars a vector of parameters
#' @param elc an elicitation object to be print
#' @param ... additional arguments passed to the integrator or to the methods
#'
#' @return An elc.input object
#' @export
#'
#' @examples
#' elc <- input_elicitation("Gamma", c(0.1, 0.01))
#' elc
input_elicitation <- function(dist, pars) {
  inp <- switch (dist,
    Gamma = input_gamma,
    Normal = input_norm,
    LogNormal = input_lnorm,
    StudentT = input_t,
    LogStudentT = input_lt,
    Triangle = input_triangle
  )
  inp(pars)
}


#' @rdname input_elicitation
#' @export
print.elc.input <- function(elc, ...) {
  cat("Inputed elicitation distribution\n")
  cat("\nDistribution:", elc$Distribution, "\n")
  cat("\nParameters:\n")
  for (i in 1:length(elc$Parameters)) {
    cat(paste0("\t", names(elc$Parameters)[i], " = ", elc$Parameters[i]))
    cat("\n")
  }
}
