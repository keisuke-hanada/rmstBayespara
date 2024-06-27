#' Restricted mean survival time via parametric models
#' @description A function of calculating restricted mean survival time via parametric models. Exponential, Weibull, log-normal and log-logistic models are available.
#'
#' @name rmstpara
#'
#' @param tau A value of pre-specified evaluation time point.
#' @param var A vector of covariate values.
#' @param frai a vector of frailty effects. It is necessary when log-normal frailty and log-logistic frailty models.
#' @param shape a vector of shape parameters. It is necessary when Weibull and log-logistic models.
#' @param sigma a vector of standard error parameters. It is necessary when log-normal model.
#' @param family A description of the response distribution and link function to be used in the model. 'exponential', 'Weibull', 'log-normal', and 'log-logistic' can be selected.
#' @param random A description of random effect. 'fixed', 'normal', and 'frailty' are available.
#'
#' @return An object of class brmsfit or stanfit. See rstan and brms.
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
#' fit_x_r$post_sample
#' ps_x_r<-fit_x_r$post_sample
#' rmst_x_r<-rmstpara(tau=100, var=ps_x_r[,"b_intercept"]+ps_x_r[,"b_factor(arm)"],
#'                    shape=ps_x_r[,"shape"], family="Weibull",random="frailty")
#' rmst_x_r
#'
#' @export
rmstpara <- function(tau, var, frai=NA, shape=NA, sigma=NA, family="exponential", random="fixed"){
  if(family=="exponential"){
    return((1-base::exp(-tau/base::exp(var)))*base::exp(var))
  }else if(family=="Weibull"){
    if(!base::is.na(shape)[1]){
      return(base::exp(var)*zipfR::Igamma(1/shape+1, (tau/base::exp(var))**shape, lower=TRUE)+tau*base::exp(-(tau/base::exp(var))**shape))
    }else{
      stop("'shape' variable need to calculate RMST when 'family' is 'Weibull'.")
    }
  }else if(family=="log-normal"){
    if(!base::is.na(sigma)[1]){
      if(random=="fixed"){
        return(base::exp(var+sigma**2/2)*stats::pnorm((base::log(tau)-var-sigma**2)/sigma,0,1)+
                 tau*(1-stats::pnorm((base::log(tau)-var)/sigma,0,1)))
      }else if(random=="normal"){
        return(base::exp(var+sigma**2/2)*stats::pnorm((base::log(tau)-var-sigma**2)/sigma,0,1)+
                 tau*(1-stats::pnorm((base::log(tau)-var)/sigma,0,1)))
      }else if(random=="frailty"){
        if(!base::is.na(frai)[1]){
          return(base::exp(var+sigma**2/2)*(1/frai)*(1-(1-stats::pnorm((base::log(tau)-var-sigma**2)/sigma,0,1))**frai)+
                   tau*(1-stats::pnorm((base::log(tau)-var)/sigma,0,1))**frai)
        }else{
          stop("'frai' variable need to calculate RMST for log-normal frailty model.")
        }
      }else{
        stop("'random' variable must be set to 'fixed', 'normal', or 'frailty' when 'family' is 'log-normal'.")
      }
    }else{
      stop("'sigma' variable need to calculate RMST when 'family' is 'log-normal'.")
    }
  }else if(family=="log-logistic"){
    if(!base::is.na(shape)[1]){
      if(random=="fixed"){
        return(base::exp(var)*zipfR::Ibeta((tau/base::exp(var))**shape/(1+(tau/base::exp(var))**shape), 1+1/shape, 1-1/shape, lower=TRUE)+
                 tau/(1+(tau/base::exp(var))**shape))
      }else if(random=="normal"){
        return(base::exp(var)*zipfR::Ibeta((tau/base::exp(var))**shape/(1+(tau/base::exp(var))**shape), 1+1/shape, 1-1/shape, lower=TRUE)+
                   tau/(1+(tau/base::exp(var))**shape))
      }else if(random=="frailty"){
        if(!base::is.na(frai)[1]){
          return(frai*base::exp(var)*zipfR::Ibeta((tau/base::exp(var))**shape/(1+(tau/base::exp(var))**shape), 1+1/shape, frai-1/shape, lower=TRUE)+
                   tau/((1+(tau/base::exp(var))**shape))**frai)
        }else{
          stop("'frai' variable need to calculate RMST for log-normal frailty model.")
        }
      }else{
        stop("'random' variable must be set to 'fixed', 'normal', or 'frailty'.")
      }
    }else{
      stop("'shape' variable need to calculate RMST when 'family' is 'log-logistic'.")
    }
  }else{
    stop("'family' variable must be set to 'exponential', 'Weibull', 'log-normal', 'log-logistic'.")
  }
}
