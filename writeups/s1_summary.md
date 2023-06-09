# Semester 1

## Preamble
Goal of this research is to understand the implementation of SV models using Stan and HMC. This is a very broad goal with many potential research questions. The literature on SV models, specifically Bayesian implementations of SV models, has a variety of estimation stratgies. There are multiple tradeoffs, advantages and disadvantages to consider. Is it "faster", does it provide more accurate inferences and approximations, more efficient computation, statistical properties. The fun part of this work is introducing two areas of Bayesian research which typically don't overlap (different networks, workflows and philosophies). E.g. use of bayes factors, model validation strategies, MCMC/sampling/estimation strategies (MH, particle filtering, etc).

We started with a broad set questions and a [paper](https://arxiv.org/abs/1712.02326). The brainstorming and discussion can be reduced into 3 key areas.

- Bayesian workflow for SV models in Stan
    + Prior/posterior predictive checks
    + Fake data simulation for detecting computational inefficiencies
    + Visulisations as part of model diagnostics. How to best visualiase model perforamnce and various metrics we track
    + Extensions beyond vanulla SV

- Comparing forecast efficiency of different MCMC algorithms
    + How do we evaluate “efficiency” and reliability of these algorithms. Need to work on determining precise definitions
    + Comparison against existing methods for forecast evaluation
    + Extensions beyond vanulla SV

- Model evaluation for SV models
    + Bayes factors vs LOO-CV vs forecast evaluation

There was also an interesting discussion about "comparing" MCMC algorithms more generally.

- Comparing different MCMC algorithms for SV models
    + What are we comparing - speed, inferernce, forecast evaluation, etc. 
    + Community extremely critical about "algorithmic comparisons"

The direction we decided: **Developing a workflow for fitting SV models in Stan**

## Workflow for implementing SV in Stan
We started by implementing the SV models from the Stan user guide. Catherine also provided code for simulating data. There were 3 implementations of the SV models.

- Centered parameterisation
- Non Centered parameterisation
- Non Centered parameterisation with KSC priors

We also considered "checking functions" as part of model diagnostics (Catherine's paper). Checking functions of interests:

- Autocorrelation order 1,2,3 of log(y^2)
- Skewness and kurtosis of Y (Expect constant mean but fatter tails by way of higher kurtosis)
 
 These checking functions were used to check the sensibility of the prior (compared against the data). I thought that the prior should be represent information or beliefs _before_ data is observed. However, these "data informed" priors seem to be an accepted way of checking if the GDP_implied_ by priors/model is sensible (similar to using empirical bayes I guess).

### Centered parameterisation
`sv_user_guide_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.8`

**Prior**
- Warning: 40 of 4000 (1.0%) transitions ended with a divergence.
- Warning: 818 of 4000 (20.0%) transitions hit the maximum treedepth limit of 10.
- Warning: 4 of 4 chains had an E-BFMI less than 0.2. 

**Posterior**
- Warning: 4 of 4 chains had an E-BFMI less than 0.2.

### Non Centered parameterisation
`sv_user_guide_reparameterised_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.8`

**Prior**
- No errors

**Posterior**
- Warning: 30 of 4000 (1.0%) transitions ended with a divergence.
- Model runs faster (than CP)

### Non centered parameterisation 
`sv_user_guide_reparameterised_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.8`

**Prior**
- No errors

**Posterior**
- Warning: 13 of 4000 (0.0%) transitions ended with a divergence.
- Model runs faster (than CP)

Sometimes the posterior sampling gave the following error fro each chain during warmup:

```
Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
Chain 1 Exception: normal_lpdf: Scale parameter[1] is 0, but must be positive!
CHain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
```

However, the warning only occurs at the beginning of warmup. We think the initialised value picks p = 0.5 (since it takes the midpoint) which results in phi =1 and NaN at h[1].

Preliminary results showed CP giving more concerning errors (E-BFMI) and taking longer to sample. NCP however resulted in more divergent transitions and no E-BFMI issues. This is addressed by increasing the adapt delta (reducing the step size of the MCMC sampler and the cost of longer sampling time). However, while increasing the adapt delta reduced the number of divergent samples, there was one instance where the number of divergences increased to a high value (200). This prompted a simulation study using the MonARCH cluter. Was there something special about this adapt delta? What did the neighbourhood of adapt values tell us? Was it just the particular seed value which initialsed the MCMC chain into a difficult area of the posterior distribution?

### Simulation study
Experiment design: For each adapt delta value from between 0.9 and 0.99 inclusive (incrementing at 0.05) sample the model using a different seed value. Set the seed, create a random grid of integers and pick the seed corresponding to the run id. 100 seeds were used for each adapt delta value. 

This experiment was run with a single dataset, as well as a newly simulated dataset for each seed value as a robustness check (want to make sure that any errors isn't due to a random quirk in a single dataset). The NCP model with KSC priors was used for the experiment.

Findings: The median number of divergences slowly decreased over time. However, there were massive outliers (divergences in the 1000s). This suggests that there are areas in the high dimensional posterior space which are difficult to sample from. The canonical example of this divergent transitions is Neal's funnel in hierarhcical models (using the 8 schools example). The problem with this is high divergences may result in the MCMC samples being biased and over sampling from a difficult area of the joint posterior distribution. This negatively impacts inference and the reliability of results. This is solved using NCP. 

Unfortunately for SV, it is less clear what the problematic parameters are. Furthermore, since this is a state space model, there is a latent variance parameter for each data point. So the dimension of the parameter + latent parameter space is larger than the number of data points. Pinpointing any set of problematic parameters is difficult.

Visualising pair plots showed clusters of divergent transitions across pairs of static parameters and h[1]. This was for NCP KSC priors adapt_delta=0.95 (albeit wrong prior on sigma). The divergent transitions mostly occurred in one chain (chain 3) and it occurred in the last 5th of the chain (200ish divergent samples). So this paricular chain for this model got "stuck" in a tough area of the joint posterior space (difficult geomtry to sample from). We are yet to fully investigate other models and to visualise them.

The joint posterior seems to gave a difficult shape to sample from. So despite increasing adapt delta we still may observe some divergences if we accidentally choose a bad seed. Technically, if we let the MCMC sampler run longer we should get convergence to the "true" posterior.

<!-- Should show plots here -->

#### Simulation with Stan User guide CP and NCP models
- CP model have no divergences, but have issues with E-BFMI
- NCP models have similar if not worse results than NCP with KSC priors (KSC priors are tighter/more informative).

<!-- Should show plots here -->

### Aside: save warmup
There was another issue where choosing to save the warmup samples changed the number of divergent samples (with priors, model, seed are set the same). Thanks to the Stan forums, we found out that the `*.rng` in the generated quantities blocked took some of the pseudo seed values which resulted in different draws. Removing this resulted in the samples samples. 

## Next steps
Our research direction of developing a workflow for fitting SV in Stan has pivoted slightly into understanding the errors given by the HMC/NUTs sampler. Namely we need to understand how HMC works in more detail and what these errors are telling us. 

We want to next understand how the inference from HMC compares with other MCMC methods for estimating this vanilla SV model. Despite the divergences, do we get similar/different inferences from the "gold standard"? How do these results differ? What kind of statistics/metrics/analysis do we need to conduct to "compare" across MCMC strategies. What are the tradeoffs from each? The next steps feels like a pivot in reserach direction, from "developing a SV in Stan workflow" to "understanding the implications, similarities and differences of HMC with other samplers for SV models". Although answering the latter would provide some ground work for developing a SV in Stan workflow.

- Compare differences in samples of runs with/without divergences. WHat do the pair plots look like?
- Use tourr to see if different projections and visualiastions can pinpoint the difficult parameters. 
- Run "gold standard" MCMC (what about KSC MCMC methods?). Use QQ plots to check for any differences in posterior samples. 