#' Generate random values of an elicitation object
#'
#' @param elc an elication object elc.input or elc.agg
#' @param n number of samples
#' @param method "mixture" or "average" see below
#' @param ... optional arguments
#'
#' @return a vector of sample values
#' @export
#'
#' @examples
#' expert1 <- input_norm(c(10, 2))
#' expert2 <- input_gamma(c(0.1, 0.01))
#'
#' # Single expert
#' summary(rand_elcitation(expert1, 100))
#'
#' # Aggregated expert
#' experts <- aggregate_elcitations(list(expert1, expert2))
#' summary(rand_elcitation(experts, 100))
rand_elcitation <- function(elc, n=1, ...) {
  if (n < 1) stop("n must be larger than 0")
  UseMethod("rand_elcitation", elc)
}


#' @rdname rand_elcitation
#' @export
rand_elcitation.elc.input <- function(elc, n=1) {
  elc$rand(n)
}


#' @rdname rand_elcitation
#' @export
rand_elcitation.elc.agg <- function(elc, n=1, method=c("mixture", "average")) {
  method <- match.arg(method)

  rd <- sapply(elc$Source, function(x) matrix(rand_elcitation(x, n)))

  if (is.matrix(rd)) {
    if (method == "mixture") {
      return(rd[, sample(1:ncol(rd), nrow(rd), rep=T)][diag(n)==1])
    } else {
      return(rowMeans(rd))
    }
  } else {
    return(mean(rd))
  }
}
