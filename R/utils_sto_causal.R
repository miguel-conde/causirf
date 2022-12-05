# https://royalsocietypublishing.org/doi/10.1098/rspa.2021.0835

#' Title
#'
#' @param in_data
#' @param causa
#' @param efecto
#' @param J
#'
#' @return
#' @import dplyr
#' @import tidyr
#'
#' @examples
make_probe_x_y <- function(in_data, causa, efecto, J = 10) {

  probe_x_y <- in_data
  for (i in -J:J) {
    FUN <- ifelse(i > 0, lag, lead)

    probe_x_y <- probe_x_y %>%
      mutate(!!sym(paste0("lag_", i)) := FUN(!!sym(causa), n = abs(i)))
  }

  probe_x_y <- probe_x_y %>% drop_na()

  return(probe_x_y)
}

#' Title
#'
#' @param g
#' @param L
#' @param J
#' @param X
#' @param Y
#' @param psi
#' @param lambda
#'
#' @return
#'
#' @examples
cost_fun <- function(g, L, J, X, Y, psi, lambda) {

  v_t <- Y - X %*% g
  mu_v <- mean(v_t)
  mu <- rep(mu_v, L - 2*J) %>% as.matrix

  t(Y - mu - X %*% g) %*% (Y - mu - X %*% g) +
    lambda * t(g) %*% t(psi) %*% psi %*% g

}

#' Title
#'
#' @param J
#'
#' @return
#'
#' @examples
make_psi <- function(J) {

  out <- matrix(0, nrow = 2*J-1, ncol = 2*J+1)

  for (i in 1:nrow(out)) {
    for (j in 1:ncol(out)) {
      if (j == i+1) out[i, j] <- 2
      else if (abs(j-i-1) == 1) out[i,j] <- 1
    }
  }
  return(out)
}

#' Title
#'
#' @param in_data
#' @param causa
#' @param efecto
#' @param J
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
est_causal <- function(in_data, causa, efecto, J = 20, lambda = 10) {

  L <- nrow(in_data)
  probe <- make_probe_x_y(in_data %>% select(-date), causa, efecto, J = J)
  X <- probe %>% select(-all_of(c(causa, efecto))) %>% as.matrix()
  Y <- probe %>% select(all_of(efecto)) %>% as.matrix()
  psi <- make_psi(J)

  res_optim <- optim(rep(0, 2*J+1), fn = cost_fun,
                     L = L, J= J, X= X, Y = Y, psi = psi, lambda = lambda,
                     method = "L-BFGS-B", lower = 0)

  out <- list(X = X,
              Y = Y,
              L = L,
              J = J,
              psi = psi,
              cause = causa,
              effect = efecto,
              res_optim = res_optim)

  class(out) <- c("est_causal", class(out))

  return(out)
}

#' Title
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
evr <- function(obj) {
  # Explained variance ratio

  v_t <- obj$Y - obj$X %*% matrix(obj$res_optim$par)
  mu_v <- mean(v_t)
  mu <- rep(mu_v, obj$L - 2*obj$J) %>% as.matrix()
  hat_gamma_nu <- 1 / (obj$L - 2*obj$J) * t(obj$Y - mu - obj$X %*% matrix(obj$res_optim$par)) %*%
    (obj$Y - mu - obj$X %*% matrix(obj$res_optim$par))
  hat_gamma_y = var(obj$Y)

  e <- 1 - hat_gamma_nu / hat_gamma_y

  return(e)
}


#' Title
#'
#' @param object
#' @param ...
#'
#' @return
#' @import ggplot2
#' @importFrom ggplot2 autoplot
#' @export
#'
#' @examples
autoplot.est_causal <- function(object, ...) {

  gg_res_optim <- tibble(x = 1:ncol(object$X),
                         time_lag = colnames(object$X),
                         irf = object$res_optim$par)

  the_title <- paste(object$effect, "--->", object$cause)
  the_subtitle <- paste("Explained Variance Ratio:",
                        sprintf("%.3f", as.numeric(evr(object))))

  p <- ggplot(data = gg_res_optim,
              mapping = aes(x = x, y = irf, color = x > object$J)) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    geom_vline(xintercept = object$J+1, linetype = 2) +
    scale_x_continuous(breaks = 1:(2*object$J+1), labels = gg_res_optim$time_lag) +
    labs(title = the_title, subtitle = the_subtitle) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}


