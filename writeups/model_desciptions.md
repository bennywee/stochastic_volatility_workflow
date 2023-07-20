# "Centered" Stochastic Volatility Model
This uses notation from the Stan user guide. The KSC version of this has slightly different notation but is the same model.

$$
\begin{aligned}
y_t =& \space \epsilon_t exp(h_t/2) \\
h_{t+1} =& \space \mu +\phi(h_t - \mu) + \delta_t \sigma\\
h_1 \sim& \space normal\left(\mu, \frac{\sigma}{\sqrt{1-\phi^2}}\right) \\
\epsilon_t \sim& \space normal(0,1) \\
\delta_t \sim& \space normal(0,1)
\end{aligned}
$$

This can be expressed more succinctly as:

$$
\begin{aligned}
y_t \sim& \space normal(0, exp(h_t/2)) \\ 
h_1 \sim& \space normal \left(\mu, \frac{\sigma}{\sqrt{1-\phi^2}}\right) \\
h_{t+1} \sim& \space normal(\mu +\phi(h_t - \mu) , \sigma), \space\space t\neq 1\\ 
\end{aligned}
$$

With KSC priors:

$$
\begin{aligned}
\mu \sim& \space normal(0, 10) \\
\sigma^2 \sim& \space IG(5/2, (0.01*5)/2) \\
\phi \sim& \space stretched\space beta(20, 1.5) \\
\end{aligned}
$$

Aside on stretched beta: this is a beta distribution which has been transformed to have support (-1, 1). Suppose we have a beta distributed variable $p\sim beta(a,b)$ with support (0,1). Then to apply this transformation, we define $\phi = 2p - 1$


## SBC code
Below is code to simulate 1000 datasets from the above centered model.

```r
gen_cp_sv_dataset <- function(seed){
    set.seed(seed)

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, 10)
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
The preposed reparameterisation samples from a *standard normal distribution* and rescales the parameters to give log volatility (as opposed to sampling log volatility directly). First sample from a standard normal distribuition and multiply by the variance of the log volatility parameter.

$$
\begin{aligned}
h_{std} \sim& \space normal(0,1) \\
h =& h_{std} \times \sigma \\ 
h \sim& normal(0, \sigma)
\end{aligned}
$$

Then we can apply the appropriate rescaling to get samples from the log volatility. 

$$
\begin{aligned}
h_1 =& \space \frac{h_{std, 1}\times \sigma} {\sqrt{1 - \phi^2}} + \mu \\
h_t =& \space h_{std, t}\times \sigma + \mu  + \phi(h_{std, t-1} - \mu),\space t\neq 1
\end{aligned}
$$

This returns log volatility as desired.

$$
\begin{aligned}
h_1 \sim& \space normal \left(\mu, \frac{\sigma}{\sqrt{1-\phi^2}}\right) \\
h_{t+1} \sim& \space normal(\mu +\phi(h_t - \mu) , \sigma), \space\space t\neq 1\\ 
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
    mu <- rnorm(1, 0, 10)
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

The SV model is not linear. So to deal with this we will transform the model. 

# SBC
So what does this mean for our research question? 

- CP and NCP are mathematically the "same" model with different parameterisation. Each model should produce samples from the same data generating process. 

- KSC's model is an approximation of the true data generating process through the use of gaussian mixtures and MH correction. 

We could:

- Generate data from CP or NCP and do SBC with the CP, NCP model and KSC model. See what the difference is in "calibration". We can provide a new set of diagnostics for testing the calibration of the mixture approxmiation model. In the paper they performed two sets of diagnostics:

1) Simulated data from the SV model and compare to a GARCH model, which is another type of model for modelling volatility by fitting to the simulated data and performing likelihood ratio tests. Where the null is "SV model is correct". They used the posterior mean of the SV model to get the log likelihoods and a MLE estimate of GARCH

2) Used bayes factors to compare performance of SV and GARCH on different currency data.

- Generate data from KSC's mixture model and run SBC using the KSC model and an implementation of KSC in Stan. This would be comparing the difference between Stan and the KF + MH correction for sampling the model.