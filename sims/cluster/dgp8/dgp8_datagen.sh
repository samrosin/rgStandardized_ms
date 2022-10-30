#!/bin/bash

#SBATCH -p general
#SBATCH -N 1
#SBATCH --mem=4g
#SBATCH -n 1
#SBATCH -t 02:00:00
#SBATCH --mail-type=END,FAIL,REQUEUE,STAGE_OUT
#SBATCH --mail-user=srosin@live.unc.edu
module load r/4.1.3 

#define variables
N_SIMS=1000

sbatch --output=/dev/null --error=/dev/null --time=0:30:00 --mem=200m --array=1-$N_SIMS --job-name=datasets_gen_dgp8 --wait R CMD BATCH --no-save --no-restore dgp8_datagen.R dgp8_datagen.Rout
