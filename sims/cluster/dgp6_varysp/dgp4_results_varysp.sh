#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=4g
#SBATCH -n 1
#SBATCH -t 8:00:00
#SBATCH --mail-type=END,FAIL,REQUEUE,STAGE_OUT
#SBATCH --mail-user=srosin@live.unc.edu
module load r/4.0.1  

#define variables
N_SIMS=10

sbatch --output=/dev/null --error=/dev/null --time=4:30:00 --mem=750m --array=1-$N_SIMS --job-name=results_dgp4_varysp --wait R CMD BATCH --no-save --no-restore dgp4_results_varysp.R dgp4_results_varysp.Rout
