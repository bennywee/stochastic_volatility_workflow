#Multiple chains
#Seeds + datasets
from __future__ import division
import math

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pyarrow as pa
import pyarrow.parquet as pq
np.set_printoptions(suppress=True, precision=5)

from src.gaussianMixture import *

seed=1349
np.random.seed(seed)

# ex = pd.read_excel("data/raw/xrates.xls", engine = "xlrd")
# dta = np.log(ex).diff().iloc[1:]
# endog = (dta['USXUK'] - dta['USXUK'].mean()) * 100

# df = pd.read_csv("data/preprocessed/GSPC/20190104_20230201.csv")
df = pd.read_csv("data/preprocessed/GSPC/20230101_20230910.csv")
dta = df[["log_return"]].iloc[1:]
endog = (dta - dta.mean()) * 100

# Setup the model and simulation smoother
mod = TVLLDT(endog)
mod.set_smoother_output(0, smoother_state=True) # Calculate and return the smoothed states. Other options like cov matrices available
sim = mod.simulation_smoother()

# Simulation parameters
n_iterations = 11000
burn = 10000

seeds = [4324, 5435, 8574, 7675]

results = list()

for idx, val in enumerate(seeds):
    np.random.seed(val)
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
        params = np.r_[trace_mu[s-1], trace_phi[s-1], trace_sigma2[s-1]]
        # mod.update_mixing(trace_mixing[s-1])
        # mod.update(params, transformed=True)
    
        mod.update_mixing(indicators = trace_mixing[s-1], 
                              params = params,
                              parameterisation = "centered")
                              
        mod.update(params = params, 
                    transformed=True,
                    parameterisation = "centered")
        
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
    
    burn_draws = np.concatenate((trace_mu, trace_phi, trace_sigma2), axis = 1)
    param_draws = burn_draws[burn+1:burn_draws.shape[0]]
    samples = pd.DataFrame(param_draws)
        
    static_names = ['mu', 'phi', 'sigma2']
    samples.columns = static_names
    df = pd.melt(samples)
    df["chain"] = idx+1

    results.append(df)

draws = pd.concat(results)
draws.to_csv("manuscript/motivating_example/gma_draws_2023.csv")
draws.to_csv("manuscript/motivating_example/gma_draws1.csv")
draws.to_csv("manuscript/motivating_example/gma_draws.csv")