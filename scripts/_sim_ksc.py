from __future__ import division
import json

import numpy as np
import pandas as pd
np.set_printoptions(suppress=True, precision=5)

from src.gaussianMixture import *

config_path = "configs/ksc.json"

with open(config_path) as f:
    config = json.load(f)

np.random.seed(config["seed"])

df = pd.read_csv("../../data/simulated/ksc/phi_0.97779_sig_0.1585_beta_0.64733.csv")
endog = df['yobs']

# Setup the model and simulation smoother
mod = TVLLDT(endog)
mod.set_smoother_output(0, smoother_state=True) # Calculate and return the smoothed states. Other options like cov matrices available
sim = mod.simulation_smoother()

# Storage for traces
trace_smoothed = np.zeros((n_iterations + 1, mod.nobs))
trace_states = np.zeros((n_iterations + 1, mod.nobs))
trace_mixing = np.zeros((n_iterations + 1, mod.nobs), int)
trace_mu = np.zeros((n_iterations + 1, 1))
trace_phi = np.zeros((n_iterations + 1, 1))
trace_sigma2 = np.zeros((n_iterations + 1, 1))

# Initial values (p. 367)
trace_mixing[0] = 0 # Initialised all with the 4th gaussian. So all draws from a specific normal distribution. Not specified in the paper how this is done
trace_mu[0] = 0
trace_phi[0] = 0.95
trace_sigma2[0] = 0.02 #0.5 in the code. says 0.02 in the paper

# Iterations
for s in range(1, n_iterations + 1):
    # Update the parameters of the model
    mod.update_mixing(trace_mixing[s-1])
    params = np.r_[trace_mu[s-1], trace_phi[s-1], trace_sigma2[s-1]]
    mod.update(params, transformed=True)
    
    # Simulation smoothing
    sim.simulate()
    states = sim.simulated_state
    trace_states[s] = states[0]

    # Draw mixing indicators
    trace_mixing[s] = draw_mixing(mod, states)
    
    # Draw parameters
    trace_phi[s] = draw_posterior_phi(mod, states, trace_phi[s-1], trace_mu[s-1], trace_sigma2[s-1])
    trace_sigma2[s] = draw_posterior_sigma2(mod, states, trace_phi[s-1], trace_mu[s-1])
    trace_mu[s] = draw_posterior_mu(mod, states, trace_phi[s-1], trace_sigma2[s-1])


trace_sigma2
trace_mu
trace_phi.shape
trace_states.shape

static = np.concatenate((trace_mu, trace_phi, trace_sigma2), axis = 1)
col_names = np.array([['mu', 'phi', 'sigma2']], dtype = np.ndarray)
test = np.concatenate((col_names, static), axis = 0)
test.shape

np.savetxt("foo.csv", test, delimiter=",", fmt='%s')

static.shape
# Means of parameters
mean_phi = np.mean(trace_phi[burn::thin])
mean_sigma = np.mean(trace_sigma2[burn::thin]**0.5)
mean_beta = np.mean(np.exp(trace_mu[burn::thin] / 2))

print('Replication of Table 5 from KSC: means of posterior draws from MCMC (value from KSC in parentheses)')
print('  phi | y        = %.5f (0.97779)' % mean_phi)
print('  sigma | y      = %.5f (0.15850)' % mean_sigma)
print('  beta | y       = %.5f (0.64733)' % mean_beta)