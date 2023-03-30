functions {
  real kurtosis(vector x, int T) {
    real m_4;
    real m_2;

    m_4 = 1.0/T * sum((x - mean(x))^4);
    m_2 = (1.0/T * sum((x - mean(x))^2))^2;
  return m_4 / m_2 - 3;
  }

  real skewness(vector x, int T) {
    real m_3;
    real s_3;

    m_3 = 1.0/T * sum((x - mean(x))^3);
    s_3 = (1.0/(T-1.0) * sum((x - mean(x))^2))^(3./2.);
  return m_3 / s_3;
  }

  real first_order_autocorr(vector x_t, int T) {
    real auto_cov;
    real var_x_t;

    auto_cov = 1./(T-1.) * dot_product((x_t[1:T-1] - mean(x_t)), (x_t[2:T] - mean(x_t)));
    var_x_t = 1.0/T * sum((x_t - mean(x_t))^2);

  return auto_cov / var_x_t;
  }
}
data {
  int<lower=0> T;  // # time points (equally spaced)
  vector[T] y;     // mean corrected return at time t
  int<lower = 0, upper = 1> sample_prior;  // Flag for prior predictive checks
}
parameters {
  real mu;                     // mean log volatility
  real<lower=-1, upper=1> phi; // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  vector[T] h_std;  // std log volatility time t
}
transformed parameters {
  vector[T] h = h_std * sigma;  // now h ~ normal(0, sigma)
  h[1] /= sqrt(1 - phi * phi);  // rescale h[1]
  h += mu;
  for (t in 2:T) {
    h[t] += phi * (h[t - 1] - mu);
  }
}
model {
  phi ~ uniform(-1, 1);
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