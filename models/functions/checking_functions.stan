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