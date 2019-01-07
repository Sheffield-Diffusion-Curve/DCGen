#' Title
#'
#' @param pars sampled parameters
#' @param curves generated diffusion curves
#' @param col colour of density diagrams
#' @param col_i colour of input variables
#' @param col_o colour of fitted variables
#' @param dN show new adoption or not
#' @param statistics show statistics or curves
#' @param ci_range range of confidence intervals (0 - 100)
#' @param average average from ["mean", "median"]
#'
#' @return
#' @export
#'
#' @examples
visualise_inputs <- function(pars, col="#FF6666") {
  ps <- pars$Parameters
  bins <- round(sqrt(nrow(ps))/3)

  g1 <- ggplot2::ggplot(ps, ggplot2::aes(x=M)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title="M")

  g2 <- ggplot2::ggplot(ps, ggplot2::aes(x=N1)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title=expression(N[1]))

  g3 <- ggplot2::ggplot(ps, ggplot2::aes(x=t)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title="t'")

  g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, ncol=3)
  g
}


#' @rdname visualise_inputs
#' @export
visualise_fitted <- function(pars, col="#FF6666") {
  ps <- pars$Parameters
  bins <- round(sqrt(nrow(ps))/3)

  g1 <- ggplot2::ggplot(ps, ggplot2::aes(x=M)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title="M")

  g2 <- ggplot2::ggplot(ps, ggplot2::aes(x=p)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title="p")

  g3 <- ggplot2::ggplot(ps, ggplot2::aes(x=q)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white", bins = bins)+
    ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
    ggplot2::labs(title="q")

  g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, ncol=3)
  g
}


#' @rdname visualise_inputs
#' @export
visualise_comparing <- function(pars, col_i="blue", col_o="black") {
  ps <- pars$Parameters
  ps$Expert <- unlist(ps$Expert)

  g <- ggplot2::ggplot(ps) +
    ggplot2::geom_point(ggplot2::aes(x=N1, y=t, colour="Input", shape=Expert), alpha=0.4) +
    ggplot2::geom_point(ggplot2::aes(x=N1f, y=tf, colour="Fitted", shape=Expert), alpha=0.4, pch=19) +
    ggplot2::geom_segment(ggplot2::aes(x=N1, y=t, xend=N1f, yend=tf), alpha=0.1) +
    # ggplot2::stat_density_2d(ggplot2::aes(x=N1, y=t, colour="Fitted")) +
    ggplot2::expand_limits(x=0, y=0) +
    ggplot2::labs(title="N1 & t': before and after fitting", x=expression(N[1]), y="t'") +
    ggplot2::scale_colour_manual("", limits= c("Input", "Fitted"), values=c(col_i, col_o)) +
    ggplot2::theme_minimal()

  g
}


#' @rdname visualise_inputs
#' @export
visualise_curves <- function(curves, dN=F, statistics=T, ci_range=95,
                             average=c("mean", "median")) {

  average <- match.arg(average)
  avg <- switch (average, mean=mean, median=median)

  if (statistics==T) {
    ci_range <- max(min(ci_range, 100), 0)

    vs <- curves[, 2, ]
    ds <- curves[, 3, ]

    alpha <- 1-(ci_range/100)
    dat <- cbind(data.frame(Time=curves[, 1, 1]),
                 t(apply(vs, 1, function(x) c(avg(x), quantile(x, c(alpha/2, 1-alpha/2))))),
                 t(apply(ds, 1, function(x) c(avg(x), quantile(x, c(alpha/2, 1-alpha/2)))))
    )
    names(dat) <- c("Time", "N_avg", "N_lc", "N_uc",
                    "dN_avg", "dN_lc", "dN_uc")


    g <- ggplot2::ggplot(data=dat, aes(x=Time)) +
      ggplot2::geom_line(ggplot2::aes(y=N_avg, colour="N(t)", linetype=average)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymax=N_uc, ymin=N_lc, fill="N(t)"), alpha=0.2)

    if (dN) {
      g <- g + ggplot2::geom_line(ggplot2::aes(y=dN_avg, colour="dN(t)", linetype=average)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymax=dN_uc, ymin=dN_lc, fill="dN(t)"), alpha=0.2)
    }

    g <- g +
      ggplot2::scale_linetype_discrete("") +
      ggplot2::scale_colour_discrete("Curve") +
      ggplot2::guides(fill="none", linetype=ggplot2::guide_legend(order=1),
                      colour=ggplot2::guide_legend(order=2)) +
      ggplot2::labs(y="Adoptions", x="Time")

    g

  } else {
    ncurves <- dim(curves)[3]
    nt <- dim(curves)[1]

    dat <- data.frame(Time=c(curves[, 1, ]),
                      N=c(curves[, 2, ]),
                      dN=c(curves[, 3, ]),
                      id=rep(1:ncurves, each=nt))

    al <- max(min(100/ncurves, 0.3), 0.02)

    g <- ggplot2::ggplot(dat, ggplot2::aes(x=Time, group=id)) +
      ggplot2::geom_line(ggplot2::aes(y=N, colour="N(t)"), alpha=al)

    if (dN) {
      g <- g + ggplot2::geom_line(ggplot2::aes(y=dN, colour="dN(t)"), alpha=al)
    }

    g <- g +
      ggplot2::scale_colour_discrete("Curve") +
      ggplot2::labs(y="Adoptions", x="Time")
  }
  g
}

#
#
# visualise_curvefit <- function(pars, curves) {
#   g1 <- visualise_curves(curves, dN=T, ci_range=95)
#   g1 <- g1 + ggplot2::theme(legend.position = "none",
#                             axis.title.y=ggplot2::element_blank(),
#                             axis.title.x=ggplot2::element_blank(),
#                             axis.text.y=ggplot2::element_blank(),
#                             axis.text.x=ggplot2::element_blank())
#
#   gb <- ggplot2::ggplot_build(g1)
#   xlimits <- c(gb$layout$panel_params[[1]]$x.range[1], gb$layout$panel_params[[1]]$x.range[2])
#   ylimits <- c(gb$layout$panel_params[[1]]$y.range[1], gb$layout$panel_params[[1]]$y.range[2])
#
#   ps <- pars$Parameters
#   # bins <- max(10, round(sqrt(nrow(ps))/3))
#
#   gm <- ggplot2::ggplot(ps, ggplot2::aes(x=M)) +
#     ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white")+
#     ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
#     ggplot2::scale_x_continuous(limits=ylimits) +
#     ggplot2::theme(legend.position = "none",
#                    axis.title.y=ggplot2::element_blank(),
#                    axis.title.x=ggplot2::element_blank(),
#                    axis.text.y=ggplot2::element_blank(),
#                    axis.text.x=ggplot2::element_blank()) +
#     ggplot2::coord_flip()
#
#   gn1 <- ggplot2::ggplot(ps, ggplot2::aes(x=N1)) +
#     ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white")+
#     ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
#     ggplot2::scale_x_continuous(limits=ylimits) +
#     ggplot2::scale_y_reverse() +
#     ggplot2::theme(legend.position = "none",
#                    axis.title.y=ggplot2::element_blank(),
#                    axis.title.x=ggplot2::element_blank(),
#                    axis.text.y=ggplot2::element_blank(),
#                    axis.text.x=ggplot2::element_blank()) +
#     ggplot2::coord_flip()
#
#   gt <- ggplot2::ggplot(ps, ggplot2::aes(x=t)) +
#     ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="white")+
#     ggplot2::geom_density(alpha=.2, fill=col, colour=scales::alpha(1, .3)) +
#     ggplot2::scale_x_continuous(limits=xlimits) +
#     ggplot2::theme(legend.position = "none",
#                    axis.title.y=ggplot2::element_blank(),
#                    axis.title.x=ggplot2::element_blank(),
#                    axis.text.y=ggplot2::element_blank(),
#                    axis.text.x=ggplot2::element_blank())
#
#
#   gridExtra::grid.arrange(g1, gm, gn1, gt,
#                           layout_matrix=rbind(c(NA, 4, NA), c(3, 1, 2)),
#                           widths=c(1,4,1), heights=c(1, 3))
# }
