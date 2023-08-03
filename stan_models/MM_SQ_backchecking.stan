data{
  int N; //number of agreement observations
  int K; //number of agreement levels
  int agreements[N, K]; //agreement matrix
  vector[K] alpha_1; //alpha parameter for high-quality dirichlet prior
  vector[K] alpha_0; //alpha parameter for low-quality dirichlet prior
  real sigma_beta_0_p; //sigma parameter of prior grand mean
  real mu_beta_0_p; //mu parameter of prior on grand mean 
  
  int E; //number of enumerators
  int id_E[N]; //numerator id vector
}
parameters{
  real<lower=0> sigma_E; //standard dev of random intercepts by enumerator
  vector[E] beta_E_raw; //random intercepts by enumerator (raw)
  real beta_0; //grand mean of intercept distribution
  real mu_beta_0; //mean of Normal prior for beta_0
  real<lower=0> sigma_beta_0; //sd of Normal prior for beta_0
  
  //enforcing ordering on simplexes
  //this helps identify the model
  positive_ordered[K] pi_k_0_raw;
  positive_ordered[K] pi_k_1_raw;
}
transformed parameters{
  simplex[K] pi_k_0;
  simplex[K] pi_k_1;
  vector[E] beta_E;
  
  //transforming ordered vectors into simplexes
  pi_k_1 = pi_k_1_raw / sum(pi_k_1_raw); 
  for (k in 1:K)
    pi_k_0[k] = pi_k_0_raw[abs(k - (K+1))];
  pi_k_0 = pi_k_0 / sum(pi_k_0); //two-step process for low-quality; order has to
                                 //be reversed
                                 
  beta_E = beta_E_raw * sigma_E; //non-centered parameterization for random intercepts
}
model{
  vector[N] lambda;
  
  //mixture probability
  lambda = inv_logit(beta_0 + beta_E[id_E]);
  
  //main part of model - mixture of two multinomials
  for (n in 1:N)
    target += log_mix(lambda[n],
                      multinomial_lpmf(agreements[n] | pi_k_1),
                      multinomial_lpmf(agreements[n] | pi_k_0));
  
  //priors
  beta_0 ~ normal(mu_beta_0, sigma_beta_0); 
  beta_E_raw ~ normal(0, 1); //implies beta_E ~ normal(0, sigma_E)
  sigma_E ~ gamma(1, 1);
  
  //priors for probabilities for low-quality and high-quality multinomials 
  target += gamma_lpdf(pi_k_0_raw | alpha_0, 1);
  target += gamma_lpdf(pi_k_1_raw | alpha_1, 1);
  
  //hyperpriors
  sigma_beta_0 ~ gamma(1, 1);
  mu_beta_0 ~ normal(mu_beta_0_p, sigma_beta_0_p);
  
}
generated quantities{
  vector[N] responsibilities; //posterior probability of being part of high- 
                              //quality distribution
  vector[N] theta; 
  
  theta = inv_logit(beta_0 + beta_E[id_E]); // same as lambda; needs to be 
                                            // recalculated because lambda saved
                                            // as local variable in model block
  
  for(n in 1:N){
    real temp_total;
    vector[2] temp_resp;
    
    temp_resp[1] = log(theta[n]) + multinomial_lpmf(agreements[n]| pi_k_1);
    temp_resp[2] = log(1 - theta[n]) + multinomial_lpmf(agreements[n] | pi_k_0);
    
    temp_total = log_sum_exp(temp_resp);
    
    responsibilities[n] = temp_resp[1] - temp_total;
  }
  
  responsibilities = exp(responsibilities);
  
} 
