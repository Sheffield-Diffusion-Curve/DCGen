#' Generate a Bass Diffusion curve in discrete time approach
#'
#' @param m upper bound of the number of adoptions
#' @param p the coefficient of innovation
#' @param q the coefficient of imitation
#' @param t_min initial time point
#' @param t_max the end of time frame
#' @param dt observation interval
#'
#' @return data.frame(Time, N, dN); N for the cumulative adoptions, dN for new adoptions
#' @export
#'
#' @examples
#'
generate_diffusion_discrete <- function(m, p, q, t_min=0, t_max=20, dt=1) {
  if (t_min > t_max) {
    stop("t_min must be smaller than t_max")
  }

  steps <- seq(t_min, t_max, dt)

  ns <- rep(0, length(steps))
  dns <- rep(0, length(steps))

  N <- 0
  t <- 0
  dN <- (m - N)*(1-exp(-(p+ q/m*N)*dt))

  ns[1] <- N
  dns[1] <- dN
  N <- N + dN

  for (i in 2:length(steps)) {
    dN <- (m - N)*(1-exp(-(p+ q/m*N)*dt))

    ns[i] <- N
    dns[i] <- dN

    N <- N + dN
  }

  return (data.frame(Time=steps, N=ns, dN=dns))
}


#' Translation of parameters between (m, p , q) and (m, n1, t) using discrete time approach
#'
#' @param m upper bound of the number of adoptions
#' @param p coefficient of innovation
#' @param q coefficient of imitation
#' @param n1 cumulative adoptions in the first period
#' @param t timing when new adoptions start decreasing
#' @param dt precision of time
#'
#' @return
#' @export
#'
#' @examples
pq2nt_discrete <- function(m, p, q, dt=0.01) {
  res <- list(m=m)

  tmax <- 1.5 * log(q/p)/(p+q)

  N0 <- 0
  t <- 0
  dN0 <- (m - N0)*(1-exp(-(p + q/m*N0)*dt))
  N1 <- N0 + dN0
  while (t < tmax & length(res) < 3) {
    t <- t + dt
    dN1 <- (m - N1)*(1-exp(-(p+ q/m*N1)*dt))

    N0 <- N1
    N1 <- N1 + dN1

    if (dN1 < dN0) {
      res$t <- t
    }

    if (t < 1+dt & t >= 1) {
      res$n1 <- N1
    }
    dN0 <- dN1
  }

  return (res)
}


#' @rdname pq2nt_discrete
#' @export
nt2pq_discrete <- function(m, n1, t) {
  if (n1 > m) {
    stop("n1 must be smaller than m")
  }

  fr <- function(x) {
    sol <- pq2nt_discrete(m, x[1], x[2])

    sum((c(n1, t) - c(sol$n1, sol$t))^2 * c(1, 1))
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
