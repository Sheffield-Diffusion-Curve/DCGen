#' Pool elcitations data
#'
#' @param elcs a list of elcitation inputs
#'
#' @return An aggregated elcitation object
#' @export
#'
#' @examples
aggregate_elcitations <- function(elcs) {
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


print.elc.agg <- function(elcs) {
  cat("Aggregated elicitation distribution\n")
  cat("\nQuartiles\n\n")
  for (i in 1:length(elcs$Prob)) {
    cat(paste0("\tPr[x < ", elcs$Values[i], "] = ", elcs$Prob[i]))
    cat("\n")
  }
  cat("\nDistributions:", elcs$Distributions, "\n")
  cat("No. of experts:", length(elcs$Sources), "\n")
}


