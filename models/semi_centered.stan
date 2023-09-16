data {
  int<lower=0> T;   // # time points (equally spaced)
  vector[T] y;      // mean corrected return at time t
  int<lower = 0, upper = 1> sample_prior;  // Flag for prior predictive checks
}
parameters {
  real mu;                     // mean log volatility
  real<lower=-1, upper=1> phi; // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h_std;                 // log volatility at time t
}
model {
  phi ~ uniform(-1, 1);
  sigma ~ cauchy(0, 5);
  mu ~ cauchy(0, 10);
  h_std[1] ~ normal(0, sigma / sqrt(1 - phi * phi));
  for (t in 2:T) {
    h_std[t] ~ normal(phi * h_std[t - 1], sigma);
  }
  if(sample_prior==0){
    for (t in 1:T) {
      y[t] ~ normal(0, exp(h_std[t] / 2));
    }
  }
}
generated quantities{
  vector[T] h;
  h = h_std + mu*(1-phi);
}
