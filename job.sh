#!/bin/bash
#SBATCH --job-name=sv_adapt_delta_sim
#SBATCH --time=03:00:00
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=4
#SBATCH --array=1-100

module load  R/4.0.5
R --vanilla < scripts/sim_adapt_delta_data.r