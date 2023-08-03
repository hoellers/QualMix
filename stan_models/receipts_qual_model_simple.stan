data {
  int<lower=0> N;     // number of obs
  real x[N]; //predictor
  int success[N];  // # of successes
  int total[N];    // total number of cases
}
parameters {
  real beta_0;           // intercept
  real beta_1;            // slope
}
model {
  real temp[N];
  
  for (n in 1:N)
    temp[n] = beta_0 + beta_1*x[n];
  
  success ~ binomial_logit(total, temp);

  beta_0 ~ normal(0, 1); // tight prior around 0
  beta_1 ~ normal(0, 1); // tight prior around 0
}


