#' Generate a Bass Diffusion curve in continuous time approach
#'
#' @param m upper bound of the number of adoptions
#' @param p coefficient of innovation
#' @param q coefficient of imitation
#' @param t_min initial time point
#' @param t_max the end of time frame
#' @param dt observation interval
#' @param ... arguments for deSolve::ode
#'
#' @return data.frame(Time, N, dN); N for the cumulative adoptions, dN for new adoptions
#' @export
#'
#' @examples
#' dc <- generate_diffusion_continuous(140, 0.03, 0.41)
#' dc <- ts(dc[2:3], 0)
#' ts.plot(dc, col=c("green", "blue"))
#' legend("right", lty=1, legend=c("N", "dN"), col=c("green", "blue"))
generate_diffusion_continuous <- function(m, p, q, t_min=0, t_max=20, dt=1, ...) {
  if (t_min > t_max) {
    stop("t_min must be smaller than t_max")
  }

  parameters <- c(m=m, p=p, q=q)

  fn <- function(t, y, pars) {
    with(as.list(c(y, pars)), {
      da <- p *(m-N) + q*N/m * (m-N)
      return (list(c(dN=da), c(dN=da)))
    })
  }

  yini  <- c(N=0)
  times <- seq(t_min, t_max, dt)
  out   <- deSolve::ode(yini, times, fn, parameters, ...)

  return (data.frame(Time=out[, 1], N=out[, 2], dN=out[, 3]))
}


#' Translation of parameters between (m, p , q) and (m, n1, t)
#'
#' @param m upper bound of the number of adoptions
#' @param p coefficient of innovation
#' @param q coefficient of imitation
#' @param n1 cumulative adoptions in the first period
#' @param t timing when new adoptions start decreasing
#' @param wt weight for n1 and t in finding (m, p, q)
#'
#' @return A list of translated parameters
#' @export
#'
#' @examples
#' pq2nt_continuous(140, 0.03, 0.41)
#' nt2pq_continuous(140, 5.94, 5.08)
pq2nt_continuous <- function(m, p, q) {
  res <- list(m=m)

  res$t <- log(q/p)/(p+q)
  epq <- exp(-(p+q))
  res$n1 <- m*(1-epq)/(1+q/p*epq)

  return (res)
}


#' @rdname pq2nt_continuous
#' @export
nt2pq_continuous <- function(m, n1, t, wt=c(0.5,0.5)) {
  if (n1 > m) {
    stop("n1 must be smaller than m")
  }

  if (length(wt) != 2 | any(wt < 0)) {
    stop("wt must be a vector with positive values")
  }

  fr <- function(x) {
    sol <- pq2nt_continuous(m, x[1], x[2])

    sum((c(n1, t) - c(sol$n1, sol$t))^2 * wt)
  }

  precision <- 0.01

  grr <- function(x) {
    x1u <- x; x1u[1] <- x[1] + precision * 1e-3
    x1l <- x; x1l[1] <- x[1] - precision * 1e-3
    x2u <- x; x2u[2] <- x[2] + precision * 1e-3
    x2l <- x; x2l[2] <- x[2] - precision * 1e-3

    c(
      (fr(x1u) - fr(x1l)) / (precision * 2e-3),
      (fr(x2u) - fr(x2l)) / (precision * 2e-3)
    )
  }

  res <- constrOptim(c(0.01, 1), fr, grr, ui = rbind(c(1, 0), c(0, 1), c(-1, 1)), ci = c(0, 0, 0))
  res <- res$par

  return (list(
    m=m,
    p=res[1],
    q=res[2]
  ))
}
