functions {
#include checking_functions.stan
}
data {
  int<lower=0> T;  // # time points (equally spaced)
  vector[T] y;     // mean corrected return at time t
  int<lower = 0, upper = 1> sample_prior;  // Flag for prior predictive checks
}
parameters {
  real mu;                     // mean log volatility
  real<lower=0, upper=1> p;    // p parameter of beta 
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h_std;  // std log volatility time t
}
transformed parameters {
  real<lower=-1, upper=1> phi = 2*p-1; // persistence of volatility
  vector[T] h = h_std * sigma;  // now h ~ normal(0, sigma)
  h[1] /= sqrt(1 - phi * phi);  // rescale h[1]
  h += mu;
  for (t in 2:T) {
    h[t] += phi * (h[t - 1] - mu);
  }
}
model {
  p ~ beta(20, 1.5);
  sigma ~ inv_gamma(5./2., (0.01*5.)/2.); // how to put a prior in sigma^2?
  mu ~ normal(0, 10);
  h_std ~ std_normal();
  if(sample_prior==0){
    y ~ normal(0, exp(h / 2));
  }
}
generated quantities{
  vector[T] y_rep;
  vector[T] log_y_squared;
  real y_rep_kurtosis;
  real y_rep_skewness;
  real log_y_squared_autocorr;

  for (t in 1:T){
    y_rep[t] = normal_rng(0, exp(h[t]/2));
    log_y_squared[t] = log(y_rep[t] * y_rep[t]);
  }  

  y_rep_kurtosis = kurtosis(y_rep, T);
  y_rep_skewness = skewness(y_rep, T);
  log_y_squared_autocorr = first_order_autocorr(log_y_squared, T);
}