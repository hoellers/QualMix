data {
  int<lower=0> N;     // number of obs    
  int success[N];  // # of successes
  int total[N];    // total number of cases
  real alpha[N]; // prior alpha for x (from data)
  real beta[N]; // prior beta from x (from data)
}
parameters {
  real<lower=0,upper=1> x[N];  // unknown true value for quality (latent)
  real beta_0;           // intercept
  real beta_1;            // slope
}
model {
  real temp[N];
  
  for (n in 1:N)
    temp[n] = beta_0 + beta_1*x[n];
  
  x ~ beta(alpha, beta); // measurement model (latent x distributed beta)
  success ~ binomial_logit(total, temp);

  beta_0 ~ normal(0, 1);
  beta_1 ~ normal(0, 1);
}

