#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=8g
#SBATCH -n 1
#SBATCH -t 04:00:00
#SBATCH --mail-type=END,FAIL,REQUEUE,STAGE_OUT
#SBATCH --mail-user=srosin@live.unc.edu
module load r/4.0.1  

#define variables
N_SIMS=2

sbatch --output=/dev/null --error=/dev/null --time=2:00:00 --mem=2g --array=1-$N_SIMS --job-name=results_gen_dgp4 --wait R CMD BATCH --no-save --no-restore scenario4_var_results_cluster.R scenario4_var_results_cluster.Rout
