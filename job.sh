#!/bin/bash
#SBATCH --job-name=test_sim_bw
#SBATCH --time=01:00:00
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=12

module load  R/4.0.5
R --vanilla < scripts/sim_adapt_delta.r