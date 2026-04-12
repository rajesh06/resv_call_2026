
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] case_ratios;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real trend;
  real<lower=0> alpha;
  real<lower=0> beta;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Priors
  trend ~ normal(0.15, 0.10);
  
  // Likelihood
  case_ratios * (1 + trend) ~ gamma(alpha, beta);
}

