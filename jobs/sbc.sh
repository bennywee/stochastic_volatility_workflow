#!/bin/bash
#SBATCH --job-name=sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r9_5000_iterations
#SBATCH --time=16:00:00
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=24
#SBATCH --array=1

R --vanilla < scripts/sim_sbc.r