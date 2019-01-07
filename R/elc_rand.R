#' Generate random values of an elicitation object
#'
#' @param elc an elication object elc.input or elc.agg
#' @param n number of samples
#' @param method "mixture" or "average" see below
#' @param use sampled indices if mixture applied
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
#' summary(rand_elicitation(expert1, 100))
#'
#' # Aggregated expert
#' experts <- aggregate_elicitations(list(expert1, expert2))
#' summary(rand_elicitation(experts, 100))
rand_elicitation <- function(elc, n=1, ...) {
  if (n < 1) stop("n must be larger than 0")
  UseMethod("rand_elicitation", elc)
}


#' @rdname rand_elicitation
#' @export
rand_elicitation.elc.input <- function(elc, n=1) {
  elc$rand(n)
}


#' @rdname rand_elicitation
#' @export
rand_elicitation.elc.agg <- function(elc, n=1, method=c("mixture", "average"), use=NULL) {
  method <- match.arg(method)

  rd <- sapply(elc$Source, function(x) matrix(rand_elicitation(x, n)))

  if (is.matrix(rd)) {
    if (method == "mixture") {
      if (is.null(use)) {
        sam <- sample(1:ncol(rd), nrow(rd), replace=T)
      } else {
        sam <- use
      }
      return(list(i=sam, v=rd[, sam][diag(n)==1]))
    } else {
      return(list(i="average", v=rowMeans(rd)))
    }
  } else {
    if (method == "mixture") {
      return(list(i=1, v=mean(rd)))
    } else {
      return(list(i="average", v=mean(rd)))
    }

  }
}
