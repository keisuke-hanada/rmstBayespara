#'Bayesian regression models using 'Stan' for survival time
#' @description A function of Bayesian regression models using stan for parametric survival time. Exponential, Weibull, log-normal, and log-logistic model with fixed-effect, random-effect and frailty-effect can be available.
#'
#' @name brm_surv
#'
#' @param time name of time variable in data. Need to set character.
#' @param cnsr name of censor variable in data. Need to set character.
#' @param var vector of covariate names in data. Need to set character.
#' @param rvar name of random effect in data. Need to set character.
#' @param family A description of the response distribution and link function to be used in the model. 'exponential', 'Weibull', 'log-normal', and 'log-logistic' can be selected.
#' @param random A description of random effect. 'fixed', 'normal', and 'frailty' are available.
#' @param data An object of class data.frame (or one that can be coerced to that class) containing data of all variables used in the model.
#' @param iter Number of total iterations per chain (including warmup; defaults to 2000).
#' @param warmup A positive integer specifying number of warmup (aka burnin) iterations. This also specifies the number of iterations used for stepsize adaptation, so warmup draws should not be used for inference. The number of warmup should not be larger than iter and the default is iter/2.
#' @param seed The seed for random number generation to make results reproducible. If NA (the default), Stan will set the seed randomly.
#' @param chains Number of Markov chains (defaults to 4).
#'
#' @return A list of an object of class brmsfit or stanfit (see rstan and brms), sampling values from posterior distribution, leave-one-out cross-validation, and widely applicable information criterions.
#'
#' @examples
#'
#' d <- data.frame(time=1:100,
#'       status=sample(0:1, size=100, replace=TRUE),
#'       arm=sample(c("t", "c"), size=100, replace=TRUE),
#'       sex=sample(1:2, size=100, replace=TRUE),
#'       district=sample(1:5, size=100, replace=TRUE)
#'     )
#' head(d)
#' fit_x_r <- brm_surv(time="time", cnsr="1-status",
#'                     var=c("factor(arm)", "factor(sex)"),
#'                     rvar="district", data=d,
#'                     family="Weibull", random="frailty")
#' fit_x_r$fit
#' fit_x_r$post_sample
#' fit_x_r$waic
#' fit_x_r$loo
#'
#' @export
brm_surv <- function(time, cnsr, var, rvar, family="exponential", random="fixed", data, iter=2000, warmup=1000, seed=NA, chains=4){
  v <- base::paste(var, collapse = "+")
  if(random=="fixed"){
    fval <- base::paste(time, "|", "cens(", cnsr, ") ~ ", v, sep="")
    f <- stats::as.formula(fval)
  }else if(random=="normal" | random=="frailty"){
    fval <- base::paste(time, "|", "cens(", cnsr, ") ~ ", v, "+ (1|", rvar, ")", sep="")
    f <- stats::as.formula(fval)
  }else{
    stop("'random' variable must be set to 'fixed', 'normal', or 'frailty'.")
  }
  sdat <- brms::make_standata(f, data)

  if(family=="exponential"){
    if(random=="fixed" | random=="normal"){
      x <- brms::brm(f,
                     data=data,
                     family = exponential(),
                     iter=iter,
                     warmup=warmup,
                     seed=seed,
                     chains=chains)
    }else if(random=="frailty"){
      x <- rstan::sampling(stanmodels$exponential_frail,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }
  }else if(family=="Weibull"){
    if(random=="fixed" | random=="normal"){
      x <- brms::brm(f,
                     data=data,
                     family = weibull(),
                     iter=iter,
                     warmup=warmup,
                     seed=seed,
                     chains=chains)
    }else if(random=="frailty"){
      x <- rstan::sampling(stanmodels$weibull_frail,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }
  }else if(family=="log-normal"){
    if(random=="fixed" | random=="normal"){
      x <- brms::brm(f,
                     data=data,
                     family = lognormal(),
                     iter=iter,
                     warmup=warmup,
                     seed=seed,
                     chains=chains)
    }else if(random=="frailty"){
      x <- rstan::sampling(stanmodels$lognormal_frail,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }
  }else if(family=="log-logistic"){
    if(random=="fixed"){
      x <- rstan::sampling(stanmodels$loglogistic,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }else if(random=="normal"){
      x <- rstan::sampling(stanmodels$loglogistic_n,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }else if(random=="frailty"){
      x <- rstan::sampling(stanmodels$loglogistic_frail,
                           data=sdat,
                           iter=iter,
                           warmup=warmup,
                           seed=seed,
                           chains=chains)
    }
  }else{
    stop("'family' variable must be set to 'exponential', 'Weibull', 'log-normal', 'log-logistic'.")
  }

  post_sample <- brms::as_draws_matrix(x)
  cname <- colnames(post_sample)
  n_var <- 1 + length(var) # intercept + covariate
  cname[1:n_var] <- paste("b_", c("intercept", var), sep="")
  if(family!="exponential"){
    n_var <- n_var + 1 # shape or sigma
  }
  if(random=="normal" | random=="frailty"){
    n_var <- n_var + 1
    if(brms::is.brmsfit(x)){
      cname[n_var-1] <- base::paste("sd_", rvar, sep="")
    }else{
      cname[n_var] <- base::paste("sd_", rvar, sep="")
    }
    n_r <- base::length(base::unique(data[,rvar]))
    cname[n_var+1:n_r] <- base::paste("sd_", rvar, "[", base::sort(base::unique(data[,rvar])), "]", sep="")
  }else{
    n_r <- 0
  }
  post_sample <- post_sample[,1:(n_var+n_r)]
  if(!brms::is.brmsfit(x)){
    post_sample <- post_sample[,c(length(var)+1, 1:length(var), (length(var)+2):n_var, n_var+1:n_r)]
  }
  colnames(post_sample) <- cname[1:(n_var+n_r)]

  x_loo <- loo::loo(x)
  if(brms::is.brmsfit(x)){
    x_waic <- brms::waic(x)
  }else{
    x_waic <- loo::waic(loo::extract_log_lik(x))
  }

  return(list(fit=x, post_sample=post_sample, loo=x_loo, waic=x_waic))
}
