#Multiple chains
#Seeds + datasets
from __future__ import division
import math

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pyarrow as pa
import pyarrow.parquet as pq
import scipy as sp
np.set_printoptions(suppress=True, precision=5)

from gma import *

np.random.seed(1234)

# df = pd.read_csv("../../data/simulated/ksc/phi_0.97779_sig_0.1585_beta_0.64733.csv")
# endog = df['yobs']
ex = pd.read_excel("../../data/raw/xrates.xls", engine = "xlrd")
dta = np.log(ex).diff().iloc[1:]
endog = (dta['USXUK'] - dta['USXUK'].mean()) * 100

# Setup the model and simulation smoother
mod = TVLLDT(endog)
mod.set_smoother_output(0, smoother_state=True) # Calculate and return the smoothed states. Other options like cov matrices available
sim = mod.simulation_smoother()

# Simulation parameters
n_iterations = 19999
burn = 10000
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
burn_draws = np.concatenate((trace_mu, trace_phi, trace_sigma2, trace_states), axis = 1)
draws = burn_draws[burn:burn_draws.shape[0]]
static_names = ['mu', 'phi', 'sigma2']
state_names = [f"h[{state}]" for state in np.arange(1, trace_states.shape[1]+1)]

# Weights
y_star = np.log(endog**2 + 0.001)

def f_weights(data, states, burn):
    fn = lambda h: norm.logpdf(x=data, loc=0, scale=np.exp(h)**0.5)
    log_fn = fn(states[burn:states.shape[0]])
    # log_fn = fn(states[burn::thin])
    sum_log_fn = np.sum(log_fn, axis = 1)
    return sum_log_fn

# l = pd.DataFrame(sp.stats.loggamma.pdf(loc = 0, c = 1/2, scale = 2, size = 1000))
def f_weights(data, states, burn):
    fn = lambda h: sp.stats.loggamma.logpdf(x=data, c = 1/2, scale = np.log(2), loc = states - 1.36)
    log_fn = fn(states[burn:states.shape[0]])
    # log_fn = fn(states[burn::thin])
    sum_log_fn = np.sum(log_fn, axis = 1)
    return sum_log_fn

f =f_weights(data = y_star, states = trace_states, burn = burn)
f
# f = f_weights(data = endog, states = trace_states, burn = burn)
# sp.stats.chi2.logpdf(x=, df = 1, scale = np.sqrt(4.93), loc = trace_states[burn][0] - 1.2704)

# fn = lambda h: norm.pdf(x=endog, loc=0, scale=np.exp(trace_states[burn+2])**0.5)
# log_fn = np.log(fn(states[burn:states.shape[0]])**2)

# fn = lambda h: norm.logpdf(x=endog, loc=0, scale=np.exp(trace_states[burn+2])**0.5)
# fn(states[burn:states.shape[0]])
# np.sum(log_fn)
# norm.logpdf(x=endog, loc=0, scale=np.exp(trace_states[burn:trace_states.shape[0]])**0.5)

def k_weights(data, weights, indicators, states, burn):
    kn = lambda m, v: norm.logpdf(x=data, loc=m, scale=v**0.5)
    means = weights[indicators[burn:indicators.shape[0]], 1]
    variances = weights[indicators[burn:indicators.shape[0]], 2]
    log_kn = kn(m = states[burn:states.shape[0]] + means - 1.2704,
                v = variances)
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

f = f_weights(data = endog, states = trace_states, burn = burn)
k = k_weights(data = y_star, 
          weights = ksc_params,
          states = trace_states, 
          indicators = trace_mixing,
          burn = burn)

c = np.exp(f-k) / sum(np.exp(f-k))

counts, bins = np.histogram(np.log(c*(n_iterations-burn)))
plt.stairs(counts, bins)

fn = lambda h: norm.logpdf(x=endog, loc=0, scale=np.exp(h)**0.5)
log_fn = fn(trace_states[burn:trace_states.shape[0]])
sum_log_fn = np.sum(log_fn, axis = 0)

mean_phi = np.mean(trace_phi[burn::thin])
mean_sigma = np.mean(trace_sigma2[burn::thin]**0.5)
mean_beta = np.mean(np.exp(trace_mu[burn::thin] / 2))
# trace_states[burn:trace_states.shape[0]] + means




norm.logpdf(x=endog[1], loc=0, scale=np.exp(-0.90232)**0.5)

samples = pd.DataFrame(draws)
samples.columns = static_names + state_names

# Save as parquet
table = pa.Table.from_pandas(samples)
pq.write_table(table, 'example.parquet')




np.savetxt("foo.csv", test, delimiter=",", fmt='%s')

import pyreadr
result = pyreadr.read_r('data/simulated/sbc/sbc_data_gen_sv_ncp_ksc_priors/9.RDS')


def k_weights(data,states, burn):
    q0 = ksc_params[0,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[0,1]- 1.2704, scale=ksc_params[0,2]**0.5)
    q1 = ksc_params[1,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[1,1]- 1.2704, scale=ksc_params[1,2]**0.5)
    q2 = ksc_params[2,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[2,1]- 1.2704, scale=ksc_params[2,2]**0.5)
    q3 = ksc_params[3,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[3,1]- 1.2704, scale=ksc_params[3,2]**0.5)
    q4 = ksc_params[4,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[4,1]- 1.2704, scale=ksc_params[4,2]**0.5)
    q5 = ksc_params[5,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[5,1]- 1.2704, scale=ksc_params[5,2]**0.5)
    q6 = ksc_params[6,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[6,1]- 1.2704, scale=ksc_params[6,2]**0.5)
    log_kn = np.log((q0+q1+q2+q3+q4+q5+q6))
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

q0 = ksc_params[0,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[0,1]- 1.2704, scale=ksc_params[0,2]**0.5)
q1 = ksc_params[1,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[1,1]- 1.2704, scale=ksc_params[1,2]**0.5)
q2 = ksc_params[2,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[2,1]- 1.2704, scale=ksc_params[2,2]**0.5)
q3 = ksc_params[3,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[3,1]- 1.2704, scale=ksc_params[3,2]**0.5)
q4 = ksc_params[4,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[4,1]- 1.2704, scale=ksc_params[4,2]**0.5)
q5 = ksc_params[5,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[5,1]- 1.2704, scale=ksc_params[5,2]**0.5)
q6 = ksc_params[6,0] * norm.pdf(x=y_star, loc=trace_states[burn:trace_states.shape[0]]+ksc_params[6,1]- 1.2704, scale=ksc_params[6,2]**0.5)
log_kn = np.log((q0+q1+q2+q3+q4+q5+q6))
sum_log_kn = np.sum(log_kn, axis = 1)

k=k_weights(data = y_star, states = trace_states, burn = burn)
t-k

y += ksc_params[0,0] * norm.logpdf(x=y_star, loc=ksc_params[0,1], scale=ksc_params[0,2]**0.5)
    # means = weights[indicators[burn::thin], 1]
    # variances = weights[indicators[burn::thin], 2]
    # log_kn = kn(m = states[burn::thin] + means - 1.2704,
    #             v = variances)
    sum_log_kn = np.sum(log_kn, axis = 1)


g = pd.DataFrame(sp.stats.gamma.rvs(loc = 0, a = 1/2, scale = 2, size = 1000))
np.log(g).plot.hist()

c = pd.DataFrame(sp.stats.chi2.rvs(df=1, size = 1000000))
np.mean(c)
np.log(c).plot.hist()

l = pd.DataFrame(sp.stats.loggamma.rvs(loc = 0, c = 1/2, scale = 2, size = 1000000))
np.mean(l)
np.var(l)


l.plot.hist()

import math
def loggamma(x, a = 1/2, b = 2):
    return((1/(b**2*math.gamma(a)))*np.exp(a*x-np.exp(x)/b))

def f_weights(data, states, burn):
    log_fn = loggamma(x = data - states[burn:states.shape[0]] + 1.2)
    # log_fn = fn(states[burn::thin])
    sum_log_fn = np.sum(log_fn, axis = 1)
    return sum_log_fn

def k_weights(data, weights, indicators, states, burn):
    kn = lambda m, v: loggamma(x=data, loc=m, scale=v**0.5)
    means = weights[indicators[burn:indicators.shape[0]], 1]
    variances = weights[indicators[burn:indicators.shape[0]], 2]
    log_kn = kn(m = states[burn:states.shape[0]] + means - 1.2704,
                v = variances)
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

###############################################################
y_star = np.log(endog**2 + 0.001)

def f_weights(data, states, burn):
    fn = lambda h: norm.logpdf(x=data, loc=0, scale=np.exp(h)**0.5)
    log_fn = fn(states[burn:states.shape[0]])
    sum_log_fn = np.sum(log_fn, axis = 1)
    return sum_log_fn
f = f_weights(data = endog, states = trace_states, burn = burn)

def k_weights(data,states, burn):
    q0 = ksc_params[0,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[0,1]- 1.2704, scale=ksc_params[0,2]**0.5)
    q1 = ksc_params[1,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[1,1]- 1.2704, scale=ksc_params[1,2]**0.5)
    q2 = ksc_params[2,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[2,1]- 1.2704, scale=ksc_params[2,2]**0.5)
    q3 = ksc_params[3,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[3,1]- 1.2704, scale=ksc_params[3,2]**0.5)
    q4 = ksc_params[4,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[4,1]- 1.2704, scale=ksc_params[4,2]**0.5)
    q5 = ksc_params[5,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[5,1]- 1.2704, scale=ksc_params[5,2]**0.5)
    q6 = ksc_params[6,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[6,1]- 1.2704, scale=ksc_params[6,2]**0.5)
    log_kn = np.log((q0+q1+q2+q3+q4+q5+q6))
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn


k = k_weights(data = y_star, 
          states = trace_states, 
          burn = burn)

f/k

def k_weights(data, weights, indicators, states, burn):
    kn = lambda m, v: norm.logpdf(x=data, loc=m, scale=v**0.5)
    means = weights[indicators[burn:indicators.shape[0]], 1]
    variances = weights[indicators[burn:indicators.shape[0]], 2]
    log_kn = kn(m = states[burn:states.shape[0]] + means - 1.2704,
                v = variances)
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

f = f_weights(data = endog, states = trace_states, burn = burn)
k = k_weights(data = y_star, 
          weights = ksc_params,
          states = trace_states, 
          indicators = trace_mixing,
          burn = burn)


f-k

def k_wsseights(data,states, burn):
    q0 = ksc_params[0,0] * norm.rvs(size=data, loc=ksc_params[0,1]- 1.2704, scale=ksc_params[0,2]**0.5)
    q1 = ksc_params[1,0] * norm.rvs(size=data, loc=ksc_params[1,1]- 1.2704, scale=ksc_params[1,2]**0.5)
    q2 = ksc_params[2,0] * norm.rvs(size=data, loc=ksc_params[2,1]- 1.2704, scale=ksc_params[2,2]**0.5)
    q3 = ksc_params[3,0] * norm.rvs(size=data, loc=ksc_params[3,1]- 1.2704, scale=ksc_params[3,2]**0.5)
    q4 = ksc_params[4,0] * norm.rvs(size=data, loc=ksc_params[4,1]- 1.2704, scale=ksc_params[4,2]**0.5)
    q5 = ksc_params[5,0] * norm.rvs(size=data, loc=ksc_params[5,1]- 1.2704, scale=ksc_params[5,2]**0.5)
    q6 = ksc_params[6,0] * norm.rvs(size=data, loc=ksc_params[6,1]- 1.2704, scale=ksc_params[6,2]**0.5)
    # log_kn = np.log((q0+q1+q2+q3+q4+q5+q6))
    return q0+q1+q2+q3+q4+q5+q6


np.mean(k_wsseights(data = 1000000, 
          states = trace_states, 
          burn = burn))



### WORKING#######################
import math
def logchi2(x):
    return np.sum(-0.5 * np.log(2) - np.log(math.gamma(0.5)) + 0.5*(x-np.exp(x)))

def k_weights(data,states, burn):
    q0 = ksc_params[0,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[0,1]- 1.2704, scale=ksc_params[0,2]**0.5)
    q1 = ksc_params[1,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[1,1]- 1.2704, scale=ksc_params[1,2]**0.5)
    q2 = ksc_params[2,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[2,1]- 1.2704, scale=ksc_params[2,2]**0.5)
    q3 = ksc_params[3,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[3,1]- 1.2704, scale=ksc_params[3,2]**0.5)
    q4 = ksc_params[4,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[4,1]- 1.2704, scale=ksc_params[4,2]**0.5)
    q5 = ksc_params[5,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[5,1]- 1.2704, scale=ksc_params[5,2]**0.5)
    q6 = ksc_params[6,0] * norm.pdf(x=data, loc=states[burn:states.shape[0]]+ksc_params[6,1]- 1.2704, scale=ksc_params[6,2]**0.5)
    log_kn = np.log((q0+q1+q2+q3+q4+q5+q6))
    sum_log_kn = np.sum(log_kn, axis = 1)
    return sum_log_kn

test = list()

for idx, val in enumerate(list((np.arange(burn, trace_states.shape[0])))):
    test.append(logchi2(y_star - trace_states[val]))

k = k_weights(data = y_star, 
          states = trace_states, 
          burn = burn)

pd.DataFrame(test - k).plot.hist()

pd.DataFrame(f - k).plot.hist()
#######################
logchi2(y_star - trace_states[1000])



logchi2(x = )

x=-1
np.exp(x**0.5 - (np.exp(x)/2))

np.exp(-1)/2

# -0.5*log(2) - log(gamma(0.5)) + 0.5*(eps.grid-exp(eps.grid))