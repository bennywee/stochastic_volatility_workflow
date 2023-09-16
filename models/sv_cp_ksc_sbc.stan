data {
  int<lower=0> T;
  vector[T] y_sim;
}
parameters {
  real mu;                     // mean log volatility
  real<lower=0, upper=1> p;    // p parameter of beta. starts at 0.5
  real<lower=0> sigma_sqd;         // white noise shock scale
  vector[T] h; 
}
transformed parameters {
  real<lower=-1, upper=1> phi = 2*p-1; // persistence of volatility. Initialising starting point of p is 0.5 which is causing problems
}
model {
  p ~ beta(20, 1.5);
  sigma_sqd ~ inv_gamma(5./2., (0.01*5.)/2.);
  mu ~ normal(0, sqrt(10));
  h[1] ~ normal(mu, sqrt(sigma_sqd) / sqrt(1 - phi * phi));
  for (t in 2:T) {
    h[t] ~ normal(mu + phi * (h[t - 1] -  mu), sqrt(sigma_sqd));
  }
  y_sim ~ normal(0, exp(h / 2));
}
