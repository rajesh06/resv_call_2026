
// The input data is a vector 'case ratios' of length 'N', and the 
// projection length
data {
  int<lower=0> N;
  vector[N] case_ratios;
  int proj_length;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'alpha' and 'beta' - and the trend parameter.
parameters {
  real trend;
  real<lower=0> alpha;
  real<lower=0> beta;
}

// The model to be estimated. We model the product of case ratios
// and trend factor to be Gamma distributed with shape 'alpha'
// and rate 'beta'.
model {
  // Priors
  trend ~ normal(0.15, 0.05);
  
  // Likelihood
  case_ratios *  ((1 + trend) ^ proj_length)  ~ gamma(alpha, beta);
}
