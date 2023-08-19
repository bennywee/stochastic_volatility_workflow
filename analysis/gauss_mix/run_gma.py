#Multiple chains
#Seeds + datasets
from __future__ import division

import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
np.set_printoptions(suppress=True, precision=5)

from gma import *

np.random.seed(1234)

df = pd.read_csv("../../data/simulated/ksc/phi_0.97779_sig_0.1585_beta_0.64733.csv")
endog = df['yobs']

# Setup the model and simulation smoother
mod = TVLLDT(endog)
mod.set_smoother_output(0, smoother_state=True) # Calculate and return the smoothed states. Other options like cov matrices available
sim = mod.simulation_smoother()

# Simulation parameters
n_iterations = 1000
burn = 100
thin = 10

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

# Create dataframe
draws = np.concatenate((trace_mu, trace_phi, trace_sigma2, trace_states), axis = 1)
static_names = ['mu', 'phi', 'sigma2']
state_names = [f"h[{state}]" for state in np.arange(1, trace_states.shape[1]+1)]
samples = pd.DataFrame(draws[burn:draws.shape[0]])
samples.columns = static_names + state_names

# Save as parquet
table = pa.Table.from_pandas(samples)
pq.write_table(table, 'example.parquet')




np.savetxt("foo.csv", test, delimiter=",", fmt='%s')

import pyreadr
result = pyreadr.read_r('data/simulated/sbc/sbc_data_gen_sv_ncp_ksc_priors/9.RDS')