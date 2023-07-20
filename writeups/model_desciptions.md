# "Centered" Stochastic Volatility Model
This uses notation from the Stan user guide. The KSC version of this has slightly different notation but is the same model.

$$
\begin{aligned}
y_t =& \space \epsilon_t exp(h_t/2) \\
h_{1} =& \space \mu +\phi(h_t - \mu) + \delta_t \sigma\\
h_1 \sim& \space normal\left(\mu, \frac{\sigma}{\sqrt{1-\phi^2}}\right) \\
\epsilon_t \sim& \space normal(0,1) \\
\delta_t \sim& \space normal(0,1)
\end{aligned}
$$

This can be expressed more succinctly as:

$$
\begin{aligned}
y_t =& \space normal(0, exp(h_t/2)) \\ 
h_1 \sim& \space normal \left(\mu, \frac{\sigma}{\sqrt{1-\phi^2}}\right) \\
h_{t+1} =& \space normal(\mu +\phi(h_t - \mu) , \sigma), \space\space t\neq 1\\ 
\end{aligned}
$$

With KSC priors:



## SBC code

# "Non Centered" Stochastic Volatility Model