# 0. Abstract

# 1. Introduction

# 2. Literature review?

# 3. SV model
- Vanilla case
- Estimation strategies (KSC, "gold standard" MCMC, Kalman/particle filters?)

# 4. Stan (HMC/NUTs) and MCMC
## 4.1 What is HMC

## 4.2 Error messages and diagnostics
- E-BFMI
- Divergent transitions

## 4.3 How to estimate SV (KSC)?
- What is the fundamental difference in KSC estimation strategies vs HMC

# 5. Implementing SV in Stan
## 5.1 Data generation
## 5.2 Reparameterisation
- Centered and non centered

## 5.3 Simulation study
- Box plots across seeds

## 5.4 Limitations in understanding high dimensional posterior (state space model and parameters > data points)
- Should probably visualise pair plots or something to compare models with/without divergent transitions

# 6. "Comparison" with other algorithms
- How to compare? What to compare
- SBC
- Are inferences valid with divergences vs gold standard MCMC. Are they the same?
- What inferences are we trying to make?

# 7. Recommendations and Limitations
- Non centered + high adapt delta?

# 8. Conclusion

