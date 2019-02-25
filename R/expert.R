#' Generate an expert with DCG input distributions
#'
#' @param name Name of the expert
#' @param dist_m distribution of the cap of adoptions
#' @param pars_m parameters of m
#' @param dist_n1 distribution of the adoptions in the first period
#' @param pars_n1 parameters of N1
#' @param dist_t distribution of the time when new adoption starts declining (t')
#' @param pars_t parameters of t'
#'
#' @return
#' @export
#'
#' @examples
#' Exp = new_expert("A", "Triangle", c(54.2, 10, 150), "Triangle", c(2.3, 0, 5), "Triangle", c(5.1, 3, 8))
#' print(Exp)
new_expert <- function(name, dist_m, pars_m, dist_n1, pars_n1, dist_t, pars_t) {
  exp <- list(
    Name = name,
    M = input_elicitation(dist_m, pars_m),
    N1 = input_elicitation(dist_n1, pars_n1),
    t = input_elicitation(dist_t, pars_t)
  )
  class(exp) <- "expert"
  exp
}


#' @rdname new_expert
#' @export
print.expert <- function(expert, ...) {
  m <- expert$M
  pars_m <- paste(paste(names(m$Parameters), m$Parameters, sep="="), collapse = ", ")
  m <- paste0(m$Distribution, "(", pars_m, ")")

  n1 <- expert$N1
  pars_n1 <- paste(paste(names(n1$Parameters), n1$Parameters, sep="="), collapse = ", ")
  n1 <- paste0(n1$Distribution, "(", pars_n1, ")")

  ti <- expert$t
  pars_ti <- paste(paste(names(ti$Parameters), ti$Parameters, sep="="), collapse = ", ")
  ti <- paste0(ti$Distribution, "(", pars_ti, ")")

  cat("Expert(", expert$Name, ", M=", m, ", N1=", n1, ", t=", ti, ")\n")
}



#' Aggregate the elicitation data of many experts
#'
#' @param experts a list of experts
#'
#' @return
#' @export
#'
#' @examples
#' ExpA = new_expert("A", "Triangle", c(54.2, 10, 150), "Triangle", c(2.3, 0, 5), "Triangle", c(5.1, 3, 8))
#' ExpB = new_expert("B", "Triangle", c(158.8, 30, 230), "Triangle", c(5.7, 2, 15), "Triangle", c(9.9, 7, 13))
#' ExpC = new_expert("C", "Triangle", c(204.4, 30, 410), "Triangle", c(7.1, 2, 10), "Triangle", c(3.5, 2, 6))
#'
#' experts <- aggregate_experts(list(ExpA, ExpB, ExpC))
aggregate_experts <- function(experts) {
  res <- list()
  res$Names <- sapply(experts, function(x) x$Name)
  m <- lapply(experts, function(x) x$M)
  res$M <- aggregate_elicitations(m)
  n1 <- lapply(experts, function(x) x$N1)
  res$N1 <- aggregate_elicitations(n1)
  ti <- lapply(experts, function(x) x$t)
  res$t <- aggregate_elicitations(ti)
  res$Experts <- experts
  class(res) <- "experts"
  res
}


#' @rdname aggregate_experts
#' @export
print.experts <- function(experts, ...) {
  m <- experts$M
  pars_m <- paste(paste(c("Q1", "Q2", "Q3"), round(m$Values, 1), sep="="), collapse = ", ")
  m <- paste0("(", pars_m, ")")

  n1 <- experts$N1
  pars_n1 <- paste(paste(c("Q1", "Q2", "Q3"), round(n1$Values, 1), sep="="), collapse = ", ")
  n1 <- paste0("(", pars_n1, ")")

  ti <- experts$t
  pars_ti <- paste(paste(c("Q1", "Q2", "Q3"), round(ti$Values, 1), sep="="), collapse = ", ")
  ti <- paste0("(", pars_ti, ")")

  cat("Expert(", experts$Names, ", M=", m, ", N1=", n1, ", t=", ti, ")\n")
}





#' Generate random samples of diffusion curve parameters given elicitation inputs
#'
#' @param expert
#' @param n
#' @param type "continuous" or "discrete" time
#' @param method aggregation method of "mixture" or "average"
#' @param max_try maximium tests if parameters are infeasible
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
rand_parameters <- function(expert, n=1, type=c("continuous", "discrete"), ...) {
  if (n < 1) stop("n must be larger than 0")
  UseMethod("rand_parameters", expert)
}


#' @rdname rand_parameters
#' @export
rand_parameters.expert <- function(expert, n=100, max_try=20, type=c("continuous", "discrete"), ...) {
  type = match.arg(type)

  m <- rand_elicitation(expert$M, n)
  n1 <- rand_elicitation(expert$N1, n)
  ti <- rand_elicitation(expert$t, n)

  mnt <- data.frame(Expert=expert$Name, M=m, N1=n1, t=ti)
  wt <- c(sd(mnt$N1), sd(mnt$t))
  n_try <- 0
  collected <- c()
  repeat {
    temp <- c()
    for (i in 1:nrow(mnt)) {
      if (mnt[i, "M"] > mnt[i, "N1"]) {
        if (type == "continuous") {
          d <- tryCatch({nt2pq_continuous(mnt[i, "M"], mnt[i, "N1"], mnt[i, "t"], wt)},
                        warning=function(w) {list()})
        } else {
          d <- tryCatch({nt2pq_discrete(mnt[i, "M"], mnt[i, "N1"], mnt[i, "t"], wt)},
                        warning=function(w) {list()})
        }

        d$N1 <- mnt[i, "N1"]
        d$t <- mnt[i, "t"]
        temp <- rbind(temp, d)
      }
    }
    collected <- rbind(collected, temp)
    n_try <- n_try + 1
    if (nrow(collected) == n | n_try >= max_try) {
      break
    }
    nr <- nrow(collected)
    todo <- ifelse(is.null(nr), n, n-nr)
    m <- rand_elicitation(expert$M, n)
    n1 <- rand_elicitation(expert$N1, n)
    ti <- rand_elicitation(expert$t, n)
    mnt <- data.frame(Expert=expert$Name, M=m, N1=n1, t=ti)
  }
  collected <- data.frame(collected, row.names=NULL)
  collected$Expert = expert$Name
  collected <- collected[c("Expert", "M", "N1", "t", "p", "q")]
  collected$Expert <- unlist(collected$Expert)
  collected[c("M", "N1", "t", "p", "q")] <- apply((collected[, c("M", "N1", "t", "p", "q")]), 2, unlist)

  p <- collected$p
  q <- collected$q
  collected$tf <- log(q/p)/(p+q)
  epq <- exp(-(p+q))
  collected$N1f <- unlist(collected$M)*(1-epq)/(1+q/p*epq)

  collected[-1] <- apply(collected[-1], 2, unlist)

  res <- list(Expert=expert, Type=type, Size=n, n_iter=n_try, Parameters=collected)
  class(res) <- "DCGen.single"
  res
}


#' @rdname rand_parameters
#' @export
rand_parameters.experts <- function(expert, n=100, method=c("mixture", "average"),
                                    type=c("continuous", "discrete"), max_try=20, ...) {
  method = match.arg(method)
  type = match.arg(type)

  m <- rand_elicitation(expert$M, n, method=method)
  n1 <- rand_elicitation(expert$N1, n, method=method, use=m$i)
  ti <- rand_elicitation(expert$t, n, method=method, use=m$i)
  if (method=="mixture") {
    e_names <- expert$Names[m$i]
  } else {
    e_names <- "average"
  }
  mnt <- data.frame(Expert=e_names, M=m$v, N1=n1$v, t=ti$v, stringsAsFactors = F)
  wt <- c(sd(mnt$N1), sd(mnt$t))
  n_try <- 0
  collected <- c()
  repeat {
    temp <- c()
    for (i in 1:nrow(mnt)) {
      if (mnt[i, "M"] > mnt[i, "N1"]) {
        if (type == "continuous") {
        d <- tryCatch({nt2pq_continuous(mnt[i, "M"], mnt[i, "N1"], mnt[i, "t"], wt)},
                        warning=function(w) {list()})
        } else {
          d <- tryCatch({nt2pq_discrete(mnt[i, "M"], mnt[i, "N1"], mnt[i, "t"], wt)},
                        warning=function(w) {list()})
        }

        d$N1 <- mnt[i, "N1"]
        d$t <- mnt[i, "t"]
        d$Expert <- mnt[i, "Expert"]
        temp <- rbind(temp, d)
      }
    }
    collected <- rbind(collected, temp)
    n_try <- n_try + 1
    if (nrow(collected) == n | n_try >= max_try) {
      break
    }
    nr <- nrow(collected)
    todo <- ifelse(is.null(nr), n, n-nr)
    m <- rand_elicitation(expert$M, todo, method=method)
    n1 <- rand_elicitation(expert$N1, todo, method=method, use=m$i)
    ti <- rand_elicitation(expert$t, todo, method=method, use=m$i)

    if (method=="mixture") {
      e_names <- expert$Names[m$i]
    } else {
      e_names <- "average"
    }
    mnt <- data.frame(Expert=e_names, M=m$v, N1=n1$v, t=ti$v, stringsAsFactors = F)
  }
  collected <- data.frame(collected, row.names=NULL)
  collected <- collected[c("Expert", "M", "N1", "t", "p", "q")]
  collected$Expert <- unlist(collected$Expert)
  collected[c("M", "N1", "t", "p", "q")] <- apply((collected[, c("M", "N1", "t", "p", "q")]), 2, unlist)
  p <- unlist(collected$p)
  q <- unlist(collected$q)
  epq <- exp(-(p+q))
  collected$N1f <- unlist(collected$M)*(1-epq)/(1+q/p*epq)
  collected$tf <- log(q/p)/(p+q)

  collected[-1] <- apply(collected[-1], 2, unlist)

  res <- list(Expert=expert, Method=method, Type=type, Size=n, n_iter=n_try, Parameters=collected)
  class(res) <- "DCGen.multi"
  res
}
