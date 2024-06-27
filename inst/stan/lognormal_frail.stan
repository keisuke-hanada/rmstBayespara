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
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
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
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector<lower=0>[N_1] v;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 5.3, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  v~gamma(1/sd_1[1],1/sd_1[1]);
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
    // special treatment of censored data
      if (cens[n] == 0) {
//        target += lognormal_lpdf(Y[n] | mu[n], sigma)+log(v[J_1[n]])+(v[J_1[n]]-1)*lognormal_lccdf(Y[n] | mu[n], sigma);
        target += lognormal_lpdf(Y[n] | mu[n], sigma)+log(v[J_1[n]])+(v[J_1[n]]-1)*log(1-lognormal_cdf(Y[n] | mu[n], sigma));
      } else if (cens[n] == 1) {
        target += v[J_1[n]]*log(1-lognormal_cdf(Y[n] | mu[n], sigma));
//        target += v[J_1[n]]*lognormal_lccdf(Y[n] | mu[n], sigma);
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
  for (n in 1:N) {
  // special treatment of censored data
      if (cens[n] == 0) {
        log_lik[n] = lognormal_lpdf(Y[n] | mu[n], sigma)+log(v[J_1[n]])+(v[J_1[n]]-1)*lognormal_lccdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == 1) {
        log_lik[n] = v[J_1[n]]*lognormal_lccdf(Y[n] | mu[n], sigma);
      }
  }
}