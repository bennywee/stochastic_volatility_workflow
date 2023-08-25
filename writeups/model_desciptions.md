# "Centered" Stochastic Volatility Model
The univariate stochastic volatility model described by Kim, Shepherd and Chib (1998), now denoted as KSC, models the **variance** as a random variable following some latent stochastic process. $y_t$ is the mean corrected returns of some asset for equally spaced intervals t. $\beta$ is a constant scaling factor which is also defined as $exp(\mu / 2)$ representing instantaneous volatility. $h_t$ is log volatility, where $h_1$ is a draw from a stationary distribution and the state equation $h_{t+1}$ follows a stationary process governed by the autoregressive parameter $\phi$ such that $|\phi|<1$. This autoregressive parameter represents the persistence or "stickiness" of log volatility and the dispersion parameter $\sigma_{\eta}$ is its variance. $\epsilon_t$ and $\eta_t$ are standard normal white noise shocks. 

$$
\begin{aligned}
y_t =& \space \beta exp(h_t/2) \epsilon_t \\
h_{t+1} =& \space \mu +\phi(h_t - \mu) + \sigma_{\eta} \eta_t  \\
h_1 \sim& \space normal\left(\mu, \frac{\sigma_{\eta}^2}{1-\phi^2}\right) \\
\end{aligned}
$$


$$
\begin{aligned}
\epsilon_t \sim& \space normal(0,1) \\
\delta_t \sim& \space normal(0,1)
\end{aligned}
$$

Setting $\beta=1$, the model can be expressed more succinctly as:

$$
\begin{aligned}
y_t \sim& \space normal(0, exp(h_t/2)) \\ 
h_1 \sim& \space normal \left(\mu, \frac{\sigma_{\eta}^2}{1-\phi^2}\right) \\
h_{t+1} \sim& \space normal(\mu +\phi(h_t - \mu) , \sigma_{\eta}^2), \space\space t\neq 1\\ 
\end{aligned}
$$

With priors for the static parameters as defined in KSC:

$$
\begin{aligned}
\mu \sim& \space normal(0, 10^2) \\
\sigma_{\eta}^2 \sim& \space IG(5/2, (0.01\times 5) / 2) \\
\phi^{\ast} \sim& \space beta(20, 1.5) \\
\phi &=  2\phi^{\ast} - 1
\end{aligned}
$$

The prior on $\phi$ is a "stretched" beta distribution. This is a beta distribution (as defined on the parameter $\phi^*$) which has been transformed to have support (-1, 1).

This parameterisation of the model is called the "centered" model because the mean of the latent state parameters are "centered" on the mean plus lag of the log volatility $\mu +\phi(h_t - \mu)$. 

## SBC code
Below is code to simulate 1000 datasets from the above centered model.

```r
gen_cp_sv_dataset <- function(seed){
    set.seed(seed)

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, sqrt(10))
    sigma_sqd <- 1 / rgamma(1, 5/2, (0.01*5)/2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2*p - 1

    # Generate log volatilities
    h <- rep(0, T)
    h[1] <- rnorm(1, mean=mu, sd=sigma/sqrt(1-phi^2))
    for(i in 2:T){
        h[i] <- rnorm(1, mean=(mu+phi*(h[(i-1)]-mu)), sd=sigma)
      }

    # Generate data from prior
    y <- exp(h/2)*rnorm(T, 0, 1)

    return(y)
}

set.seed(123)
seed_list <- sample.int(10000)[1:1000] 
simulated_cp_datasets = lapply(seed_list, gen_cp_sv_dataset)
```

# "Non Centered" Stochastic Volatility Model
For computational purposes, it may be difficult to sample directly from the centered parameterisation of a model, conditional on the type of algorithm. We can reparameterise the model such that the states are first sampled on a distribution centered on 0 (i.e "non centered") which are then transformed to have the correct mean. 

First sample from a standard normal distribuition and multiply by the variance of the log volatility.

$$
\begin{aligned}
h_{std} \sim& \space normal(0,1) \\
h =& h_{std} \times \sigma_{\eta} \\ 
h \sim& normal(0, \sigma_{\eta}^2)
\end{aligned}
$$

Then we can apply the appropriate rescaling to get samples from the log volatility. 

$$
\begin{aligned}
h_1 =& \space \frac{h_{std, 1}\times \sigma_{\eta}} {\sqrt{1 - \phi^2}} + \mu \\
h_t =& \space h_{std, t}\times \sigma_{\eta} + \mu  + \phi(h_{std, t-1} - \mu),\space t\neq 1
\end{aligned}
$$

This returns log volatility as desired.

$$
\begin{aligned}
h_1 \sim& \space normal \left(\mu, \frac{\sigma_{\eta}^2}{1-\phi^2}\right) \\
h_{t+1} \sim& \space normal(\mu +\phi(h_t - \mu) , \sigma_{\eta}^2), \space\space t\neq 1\\ 
\end{aligned}
$$

## SBC code
Below is code to simulate 1000 datasets from the non centered model. Note all the priors are the same - so this sampling from the _same model_, except we sample from from a standard normal distribution and apply rescaling to get the log volatilities.

```r
gen_ncp_sv_dataset <- function(seed){
    set.seed(seed)

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, sqrt(10))
    sigma_sqd <- 1 / rgamma(1, 5/2, (0.01*5)/2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2*p - 1

    # Sample from standardised normal dist and multiply by sigma
    h_std <- rnorm(T, mean = 0, sd = sigma)

    # Generate log volatilities
    h[1] <- rnorm(1, mean=mu, sd=sigma/sqrt(1-phi^2))
    for(i in 2:T){
        h[i] <- h_std[i] + mu + phi*(h[i-1] - mu)
      }

    # Generate data from prior
    y <- exp(h/2)*rnorm(T, 0, 1)

    return(y)
}

set.seed(123)
seed_list <- sample.int(10000)[1:1000] 
simulated_ncp_datasets = lapply(seed_list, gen_ncp_sv_dataset)
```


# KSC Model 
The KSC model uses a Kalman filter with "forward filtering and backwards sampling" (FFBS) strategy to sample from the model. This requires the model to be:

1) Lienar
2) Gaussian

The SV model is not linear. So to deal with this we will transform the model by squaring and taking the log of $y_t$.

$$
\begin{aligned}
y_t^{*} &= log(y_t^2) \\ 
&= log((\epsilon_t exp(h_t/2))^2) \\
&=  log(exp(h_t)) + log(\epsilon_t^2) \\
&= h_t + log(\epsilon_t^2)  \\
&= h_t + z_t \\
\end{aligned}
$$

Where $z_t = log(\epsilon_t^2)$ follows a log chi-squared distribution with mean -1.2704 and varaince 4.93. However, it is not simple to sample from this parameterisation of the model. KSC use a gaussian mixture model to **approximate** the first 4 moments of the log chi squared distribution. This is defined by:

$$
\begin{aligned}
f(z_t) = \sum_{i=1}^{K} q_if_N(z_i|m_i-1.2704, \nu_i^2)
\end{aligned}
$$

Where K is the mixture of normal densities $f_N$, component probabilities $q_i$, mean $m_i-1.2704$ and variance $\nu_i^2$. These parameters were seleted using moment matching where they found 7 normal densities with varying mean and variance parameters best approximated the log chi squared moments.

Importantly, since the model is now linear and gaussian, the model can be sampled in its state space form via a kalman filter and associated full conditional distributions (i.e the strategy used to sample linear gaussian state space models).

Lastly KSC also apply a Metropois Hastings correction for the fact that the sampling strategy approximates the error distribution. 

# SBC
So what does this mean for our research question? 

- CP and NCP are mathematically the "same" model with different parameterisation. Each model should produce samples from the same data generating process. 

- KSC's model is an approximation of the true data generating process through the use of gaussian mixtures and MH correction. 

We could:

- Generate data from CP or NCP and do SBC with the CP, NCP and KSC models. See what the difference is in "calibration" diagnostics. We can provide a new set of diagnostics for testing the calibration of the mixture approxmiation model. In the paper they performed two sets of diagnostics:

1) Simulated data from the SV model and compare to a GARCH model (which is another type volatility model) by fitting to the simulated data and performing likelihood ratio tests. Where the null is "SV model is correct". They used the posterior mean of the SV model to get the log likelihoods and a MLE estimate of GARCH

2) Used bayes factors to compare performance of SV and GARCH on different currency data.

**Note:** The sampling in KSC has many more iterations from their sampling algorithms (orders of 100k). I'm unsure how this will affect SBC. 

- Generate data from KSC's mixture model and run SBC using the KSC model and an implementation of KSC in Stan. This would be comparing the difference between Stan and the KF + MH correction for sampling the model.

$$
\begin{aligned}
\alpha_1 & = a + A \delta + R_0 \eta_0 \\
\delta & \sim N(0, \kappa I), \kappa \to \infty \\
\eta_0 & \sim N(0, Q_0)
\end{aligned}
$$

$$
\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})
$$

$$
(I - T^{(i)})^{-1} c_t
$$

$$
  T^{(i)} Q_0^{(i)} T^{(i)} + (R Q R')^{(i)} = Q_0^{(i)}
$$

$$
\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})
$$
$$
a^{(i)} = 0`
$$
$$
`\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})
$$
$$
`\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})
$$
$$
`\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})
$$

   If a block is initialized as known, then a known (possibly degenerate)
    distribution is used; in particular, the block of states is understood to
    be distributed
    :math:`\alpha_1^{(i)} \sim N(a^{(i)}, Q_0^{(i)})`. Here, is is possible to
    set :math:`a^{(i)} = 0`, and it is also possible that
    :math:`Q_0^{(i)}` is only positive-semidefinite; i.e.
    :math:`\alpha_1^{(i)}` may be degenerate. One particular example is
    that if the entire block's initial values are known, then
    :math:`R_0^{(i)} = 0`, and so `Var(\alpha_1^{(i)}) = 0`.

    Here, `constant` must be provided (although it can be zeros), and
    `stationary_cov` is optional (by default it is a matrix of zeros).
