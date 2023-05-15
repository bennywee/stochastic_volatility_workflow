#!/bin/bash
#SBATCH --job-name=test_sim_bw
#SBATCH --time=01:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=4
#SBATCH --array=1-100

module load  R/4.0.5
R --vanilla < scripts/sim_adapt_delta.r