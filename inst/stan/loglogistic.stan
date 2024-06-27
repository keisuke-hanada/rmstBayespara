// generated with brms 2.21.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  array[N] int<lower=-1,upper=2> cens;  // indicates censoring
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=1> shape;  // shape parameter
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 5.3, 2.5);
  lprior += gamma_lpdf(shape | 0.01, 0.01);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    mu=exp(mu);
    for (n in 1:N) {
    // special treatment of censored data
      if (cens[n] == 0) {
        target += loglogistic_lpdf(Y[n] | mu[n], shape);
      } else if (cens[n] == 1) {
        target += log(1-loglogistic_cdf(Y[n] | mu[n], shape));
      }
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  vector[N] log_lik;
  vector[N] mu = rep_vector(0.0, N);
  mu += Intercept + Xc * b;
  mu=exp(mu);
  for (n in 1:N) {
  // special treatment of censored data
    if (cens[n] == 0) {
      log_lik[n] = loglogistic_lpdf(Y[n] | mu[n], shape);
    } else if (cens[n] == 1) {
      log_lik[n] = log(1-loglogistic_cdf(Y[n] | mu[n], shape));
    }
  }
}