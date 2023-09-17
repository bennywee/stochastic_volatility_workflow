from __future__ import division

import numpy as np
import scipy as sp
import statsmodels.api as sm

from statsmodels.tsa.statespace.tools import (
    constrain_stationary_univariate, unconstrain_stationary_univariate)

from scipy.stats import norm, invgamma
from scipy.special import logsumexp
from scipy.signal import lfilter

ksc_params = np.array([[0.04395, 2.77786,   0.16735],
                       [0.24566, 1.79518,   0.34023],
                       [0.34001, 0.61942,  0.64009],
                       [0.25750, -1.08819,  1.26261],
                       [0.10556, -3.97281,  2.61369],
                       [0.00002, -8.56686,  5.17950],
                       [0.00730, -10.12999, 5.79596]])



class TVLLDT(sm.tsa.statespace.MLEModel):
    """
    Time-varying local linear deterministic trend
    Mathematical form of class attributes:
    https://www.statsmodels.org/dev/generated/statsmodels.tsa.statespace.representation.Representation.html#statsmodels.tsa.statespace.representation.Representation-attributes
    """
    def __init__(self, endog, offset=0.001):
        # Convert to log squares, with offset
        endog = np.log(endog**2 + offset)

        # Initialize base model
        super(TVLLDT, self).__init__(endog, k_states=1, k_posdef=1,
                                     initialization='stationary')

        # Setup time-varying arrays for observation equation
        self['obs_intercept'] = np.zeros((1, self.nobs)) # mod.ssm.obs_intercept
        self['obs_cov'] = np.zeros((1, 1, self.nobs)) # mod.ssm.obs_cov

        # Setup fixed components of state space matrices
        self['design', 0, 0] = 1 # mod.ssm.design. 
        self['selection', 0, 0] = 1 # mod.ssm.selection

    def update_mixing(self, indicators, params, parameterisation):
        # z_t | s_t ~ N(m_i - 1.27036, v_i^2)
        # See equation (10), p. 371
        # Indicators is a vector
        if parameterisation == "centered":
            self['obs_intercept', 0] = ksc_params[indicators, 1] - 1.27036
        elif parameterisation == "noncentered":
            self['obs_intercept', 0] = ksc_params[indicators, 1] - 1.27036 + params[0] * (1 - params[1])
        else:
            raise ValueError("Incorrect parameterisation")

        self['obs_cov', 0, 0] = ksc_params[indicators, 2]

    def update(self, params, parameterisation, **kwargs):
        params = super(TVLLDT, self).update(params, **kwargs)
        
        if parameterisation == "centered":
            self['state_intercept', 0, 0] = params[0] * (1 - params[1])
        elif parameterisation == "noncentered":
            self['state_intercept', 0, 0] = 0
        else:
            raise ValueError("Incorrect parameterisation")
        
        self['transition', 0, 0] = params[1]
        self['state_cov', 0, 0] = params[2]

        
def draw_posterior_sigma2(model, states, phi, mu, prior_params=(5, 0.05)):
    sigma_r, S_sigma = prior_params

    v1 = sigma_r + model.nobs
    tmp1 = (states[0, 0] - mu)**2 * (1 - phi**2)
    tmp = np.sum(((states[0, 1:] - mu) - phi * (states[0, :-1] - mu))**2)
    delta1 = S_sigma + tmp1 + tmp

    return invgamma.rvs(v1/2, scale=delta1/2) # Updated: Divide parameters by 2

def g(phi, states, mu, sigma2, prior_params=(20, 1.5)):
    phi_1, phi_2 = prior_params

    # Prior distribution gives zero weight to non-stationary processes
    if np.abs(phi) >= 1:
        return -np.inf

    prior = ((1 + phi) / 2)**(phi_1 - 1) * ((1 - phi) / 2)**(phi_2 - 1)

    tmp1 = (states[0, 0] - mu)**2 * (1 - phi**2) / 2 * sigma2
    tmp2 = 0.5 * np.log(1 - phi**2)

    return np.log(prior) - tmp1 + tmp2

def draw_posterior_phi(model, states, phi, mu, sigma2, prior_params=(20, 1.5)):
    tmp1 = np.sum((states[0, 1:] - mu) * (states[0, :-1] - mu))
    tmp2 = np.sum((states[0, :-1] - mu)**2)
    phi_hat = tmp1 / tmp2
    V_phi = sigma2 / tmp2

    proposal = norm.rvs(phi_hat, scale=V_phi**0.5)
    g_proposal = g(proposal, states, mu, sigma2, prior_params)
    g_previous = g(phi, states, mu, sigma2, prior_params)
    acceptance_probability = np.exp(g_proposal - g_previous)

    return proposal if acceptance_probability > np.random.uniform() else phi

def draw_posterior_mu(model, states, phi, sigma2):
    sigma2_mu = sigma2 / ((model.nobs - 1) * (1 - phi)**2 + (1 - phi**2))

    tmp1 = ((1 - phi**2) / sigma2) * states[0, 0]
    tmp = ((1 - phi) / sigma2) * np.sum(states[0, 1:] - phi * states[0, :-1])
    mu_hat = sigma2_mu * (tmp1 + tmp)

    return norm.rvs(loc=mu_hat, scale=sigma2_mu**0.5)

def mixing_posterior(mod, states):
    resid = mod.endog[:, 0] - states[0]

    # Construct the means (nobs x 7), variances (7,), prior probabilities (7,)
    means = ksc_params[None, :, 1] - 1.27036
    variances = ksc_params[:, 2]
    prior_probabilities = ksc_params[:, 0]

    # Make dimensions compatible for broadcasting
    resid = np.repeat(resid[:, None], len(variances), axis=-1)
    variances = np.repeat(variances[None, :], mod.nobs, axis=0)
    prior_probabilities = np.repeat(prior_probabilities[None, :], mod.nobs,
                                    axis=0)

    # Compute loglikelihood (nobs x 7)
    loglikelihoods = -0.5 * ((resid - means)**2 / variances +
                             np.log(2 * np.pi * variances))

    # Get (values proportional to) the (log of the) posterior (nobs x 7)
    posterior_kernel = loglikelihoods + np.log(prior_probabilities)

    # Normalize to get the actual posterior probabilities
    tmp = logsumexp(posterior_kernel, axis=1)
    posterior_probabilities = np.exp(posterior_kernel - tmp[:, None])

    return posterior_probabilities

def draw_mixing(mod, states):
    posterior_probabilities = mixing_posterior(mod, states)

    # Draw from the posterior
    variates = np.random.uniform(size=mod.nobs)
    tmp = np.cumsum(posterior_probabilities, axis=1) > variates[:, None]
    sample = np.argmax(tmp, axis=1)

    return sample

def mixture_loglike_v(data,states, burn):
    state_val = states[burn+1:states.shape[0]]
    norm_list = [ksc_params[i,0] * norm.pdf(x=data, loc=state_val+ksc_params[i,1]- 1.2704, scale=ksc_params[i,2]**0.5) for i in range(0,7)]
    log_kn = np.log(sum(norm_list))
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

def logchi2_loglike(x):
    return np.sum(np.log(sp.stats.chi2.pdf(np.exp(x), df = 1) * np.exp(x)))

def logchi2_loglike_v(data, states, burn):
    index = np.arange(burn+1, states.shape[0])
    sum_log_fn = [logchi2_loglike(data - states[i]) for i in index]
    return sum_log_fn

def importance_weights(data, states, burn):
    log_k = mixture_loglike_v(data = data, 
            states = states, 
            burn = burn)

    log_f = logchi2_loglike_v(data = data, 
            states = states, 
            burn = burn)

    w = log_f - log_k
    c = np.exp(w) / np.sum(np.exp(w))
    return c