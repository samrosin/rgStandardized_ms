#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=1g
#SBATCH -n 1
#SBATCH -t 8:00:00
#SBATCH --mail-type=END,FAIL,REQUEUE,STAGE_OUT
#SBATCH --mail-user=srosin@live.unc.edu
module load r/4.0.1  

#define variables
N_SIMS=10

sbatch --output=/dev/null --error=/dev/null --time=2:30:00 --mem=350m --array=1-$N_SIMS --job-name=results_dgp6 --wait R CMD BATCH --no-save --no-restore dgp6_results.R dgp6_results.Rout
