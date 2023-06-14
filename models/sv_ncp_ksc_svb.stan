transformed data {
  int<lower=0> T = 1000;
  real p_sim = beta_rng(20, 1.5);
  real sigma_sim = inv_gamma_rng(5./2., (0.01*5.)/2.);
  real mu_sim = normal_rng(0, 10);
  vector[T] h_std_sim;
  for (t in 1:T){
    h_std_sim[t] = normal_rng(0, 1);
  }

  real<lower=-1, upper=1> phi_sim = 2*p_sim-1;
  vector[T] h_sim = h_std_sim * sqrt(sigma_sim); 
  h_sim[1] /= sqrt(1 - phi_sim * phi_sim);
  h_sim += mu_sim;
  for (t in 2:T) {
    h_sim[t] += phi_sim * (h_sim[T - 1] - mu_sim);
  }

  vector[T] y_sim;
  for (t in 1:T){
    y_sim[t] = normal_rng(0, exp(h_sim[t]/ 2));
  }
  
}
parameters {
  real mu;                     // mean log volatility
  real<lower=0, upper=1> p;    // p parameter of beta. starts at 0.5
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h_std;  // std log volatility time t
}
transformed parameters {
  real<lower=-1, upper=1> phi = 2*p-1; // persistence of volatility. Initialising starting point of p is 0.5 which is causing problems
  vector[T] h = h_std * sqrt(sigma);  // now h ~ normal(0, sigma)
  h[1] /= sqrt(1 - phi * phi);  // rescale h[1]
  h += mu;
  for (t in 2:T) {
    h[t] += phi * (h[t - 1] - mu);
  }
}
model {
  p ~ beta(20, 1.5);
  sigma ~ inv_gamma(5./2., (0.01*5.)/2.);
  mu ~ normal(0, 10);
  h_std ~ std_normal();
  y_sim ~ normal(0, exp(h / 2));
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
