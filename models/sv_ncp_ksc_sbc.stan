data {
  int<lower=0> T;
  vector[T] y_sim;
}
parameters {
  real mu;                     // mean log volatility
  real<lower=0, upper=1> p;    // p parameter of beta. starts at 0.5
  real<lower=0> sigma_sqd;         // white noise shock scale
  vector[T] h_std;  // std log volatility time t
}
transformed parameters {
  real<lower=-1, upper=1> phi = 2*p-1; // persistence of volatility. Initialising starting point of p is 0.5 which is causing problems
  vector[T] h = h_std * sqrt(sigma_sqd);  // now h ~ normal(0, sigma)
  h[1] /= sqrt(1 - phi * phi);  // rescale h[1]
  h += mu;
  for (t in 2:T) {
    h[t] += phi * (h[t - 1] - mu);
  }
}
model {
  p ~ beta(20, 1.5);
  sigma_sqd ~ inv_gamma(5./2., (0.01*5.)/2.);
  mu ~ normal(0, sqrt(10));
  h_std ~ std_normal();
  y_sim ~ normal(0, exp(h / 2));
}
