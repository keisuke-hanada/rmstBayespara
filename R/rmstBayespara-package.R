#' The 'rmstBayespara' package.
#'
#' @description Bayesian regression models using 'Stan' for restricted mean survival time. The package implement the model estimation described in Hanada and Kojima (2024)
#'
#' @name rmstBayespara-package
#' @aliases rmstBayespara
#' @import methods
#' @import brms
#' @import BH
#' @import Rcpp
#' @import RcppEigen
#' @import StanHeaders
#' @importFrom rstan stan_model
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom stats as.formula
#' @importFrom stats pnorm
#' @importFrom loo loo
#' @importFrom loo waic
#' @importFrom loo extract_log_lik
#' @importFrom zipfR Ibeta
#' @importFrom zipfR Igamma
#'
#' @export brm_surv
#' @export rmstpara
#'
#' @references
#' Hanada, K., & Kojima, M. (2024). Bayesian Parametric Methods for Deriving Distribution of Restricted Mean Survival Time. arXiv e-prints, arXiv-2406.
#'
"_PACKAGE"
