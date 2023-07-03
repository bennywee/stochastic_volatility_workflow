#!/bin/bash
#SBATCH --job-name=sbc_ncp_ksc_priors_0.99_adapt_delta
#SBATCH --time=01:00:00
#SBATCH --mem-per-cpu=8096
#SBATCH --cpus-per-task=1
#SBATCH --array=1-999

R --vanilla < scripts/sim_sbc.r