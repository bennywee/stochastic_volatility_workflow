transformed data {
  int<lower=0> T = 1000;
  real phi_sim = uniform_rng(-1,1);
  real sigma_sim = cauchy_rng(0,5);
  real mu_sim = cauchy_rng(0,10);
  vector[T] h_sim;
  
  h_sim[1] = normal_rng(mu_sim, sigma_sim / sqrt(1 - phi_sim * phi_sim));

  for (t in 2:T) {
    h_sim[t] = normal_rng(mu_sim + phi_sim * (h_sim[t - 1] -  mu_sim), sigma_sim);
  }

  vector[T] y_sim;
  for (t in 1:T){
    y_sim[t] = normal_rng(0, exp(h_sim[t]/ 2));
  } 
}
parameters {
  real mu;                     // mean log volatility
  real<lower=-1, upper=1> phi; // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h;                 // log volatility at time t
}
model {
  phi ~ uniform(-1, 1);
  sigma ~ cauchy(0, 5);
  mu ~ cauchy(0, 10);
  h[1] ~ normal(mu, sigma / sqrt(1 - phi * phi));
  for (t in 2:T) {
    h[t] ~ normal(mu + phi * (h[t - 1] -  mu), sigma);
  }
  for (t in 1:T) {
      y_sim[t] ~ normal(0, exp(h[t] / 2));
    }
  }
generated quantities{
  array[9] int<lower=0, upper=1> sim_ranks
    = {mu < mu_sim, 
       phi < phi_sim, 
       sigma < sigma_sim, 
       h[1] < h_sim[1],
       h[100] < h_sim[100],
       h[400] < h_sim[400],
       h[500] < h_sim[500],
       h[600] < h_sim[600],
       h[1000] < h_sim[1000]
    };
}
