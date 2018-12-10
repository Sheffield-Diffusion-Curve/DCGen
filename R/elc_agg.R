#' Pool elicitations data
#'
#' @param elcs a list of elicitation inputs
#' @param ... additional arguments passed to the integrator or to the methods
#'
#' @return An aggregated elicitation object
#' @export
#'
#' @examples
#' expert1 <- input_norm(c(10, 2))
#' expert2 <- input_gamma(c(0.1, 0.01))
#'
#' # Aggregated expert
#' experts <- aggregate_elicitations(list(expert1, expert2))
#' experts
aggregate_elicitations <- function(elcs) {
  res <- list()
  res$Sources <- elcs
  res$Distributions <- unique(sapply(elcs, function(x) x$Distribution))
  res$Values <- rowMeans(sapply(elcs, function(x) x$Values))
  res$Prob <- elcs[[1]]$Prob
  res$InvCDF <- rowMeans(sapply(elcs, function(x) x$InvCDF))
  res$approx <- approxfun(names(res$InvCDF), res$InvCDF)
  class(res) <- "elc.agg"
  res
}


#' @rdname aggregate_elicitations
#' @export
print.elc.agg <- function(elcs, ...) {
  cat("Aggregated elicitation distribution\n")
  cat("\nQuartiles\n\n")
  for (i in 1:length(elcs$Prob)) {
    cat(paste0("\tPr[x < ", elcs$Values[i], "] = ", elcs$Prob[i]))
    cat("\n")
  }
  cat("\nDistributions:", elcs$Distributions, "\n")
  cat("No. of experts:", length(elcs$Sources), "\n")
}


