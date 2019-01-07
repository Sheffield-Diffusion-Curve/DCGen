#' Generate diffusion curves based expert parameters
#'
#' @param pars parameters sampled from expert inputs
#' @param t_min start time
#' @param t_max end time
#' @param dt showing time interval
#' @param progress shiny progress bar; NULL if not needed
#'
#' @return
#' @export
#'
#' @examples
#' ExpA = new_expert("A", "Triangle", c(54.2, 10, 150),
#'                   "Triangle", c(2.3, 0, 5), "Triangle", c(5.1, 3, 8))
#' ExpB = new_expert("B", "Triangle", c(158.8, 30, 230),
#'                   "Triangle", c(5.7, 2, 15), "Triangle", c(9.9, 7, 13))
#' ExpC = new_expert("C", "Triangle", c(204.4, 30, 410),
#'                   "Triangle", c(7.1, 2, 10), "Triangle", c(3.5, 2, 6))
#'
#' experts <- aggregate_experts(list(ExpA, ExpB, ExpC))
#' pars <- rand_parameters(experts, 1000, method='mixture', type='continuous')
#' curves <- generate_diffusion_curves(pars, 0, 10)
generate_diffusion_curves <- function(pars, t_min=0, t_max=NULL, dt=1, progress=NULL) {
  if (dt < 0) stop("dt must be larger than 0")
  UseMethod("generate_diffusion_curves", pars)
}


#' @rdname generate_diffusion_curves
#' @export
generate_diffusion_curves.DCGen.single <- function(pars, t_min=0, t_max=NULL, dt=1, progress=NULL) {
  ps <- apply((pars$Parameters[, c('M', "p", 'q')]), 2, unlist)
  if (is.null(t_max)) {
    t_max <- round(median(ps$t) * 2)
  }

  size <- pars$Size
  dcg <- ifelse(pars$Type == "continuous", generate_diffusion_continuous, generate_diffusion_discrete)

  res <- array(0, c(length(seq(t_min, t_max, dt)), 3, size))
  for (i in 1:size) {
    curve <- dcg(m=ps[i, "M"], p=ps[i, "p"], q=ps[i, "q"], t_min=0, t_max=t_max, dt=dt)
    res[,,i] <- as.matrix(curve)

    if (is.function(progress)) {
      progress(i, detail = paste0(round(i/size * 100), "%"))
    }
  }
  dimnames(res)[[2]] <- c("Time", "N", "dN")
  res
}


#' @rdname generate_diffusion_curves
#' @export
generate_diffusion_curves.DCGen.multi <- function(pars, t_min=0, t_max=NULL, dt=1, progress=NULL) {
  ps <- apply((pars$Parameters[, c('M', "p", 'q')]), 2, unlist)
  if (is.null(t_max)) {
    t_max <- round(median(ps$t) * 2)
  }

  size <- pars$Size
  dcg <- ifelse(pars$Type == "continuous", generate_diffusion_continuous, generate_diffusion_discrete)

  res <- array(0, c(length(seq(t_min, t_max, dt)), 3, size))
  for (i in 1:size) {
    curve <- dcg(m=ps[i, "M"], p=ps[i, "p"], q=ps[i, "q"], t_min=0, t_max=t_max, dt=dt)
    res[,,i] <- as.matrix(curve)

    if (is.function(progress)) {
      progress(i, detail = paste0(round(i/size * 100), "%"))
    }
  }
  dimnames(res)[[2]] <- c("Time", "N", "dN")
  res
}
