#!/bin/bash
#SBATCH --job-name=adapt_delta_0.9_0.99_0.005_sigsqd_prior_dataset_100
#SBATCH --time=03:00:00
#SBATCH --mem-per-cpu=8096
#SBATCH --cpus-per-task=4
#SBATCH --array=1-100

R --vanilla < scripts/sim_adapt_delta.r