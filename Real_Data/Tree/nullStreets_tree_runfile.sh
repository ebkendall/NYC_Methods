#!/bin/bash

#SBATCH -o ../Output_tree/a_out/tree%a.out
#SBATCH --array=1-77
#SBATCH --account=jantonelli
#SBATCH --qos=jantonelli-b
#SBATCH --job-name=trees
#SBATCH --time=01:00:00
#SBATCH -t 4000
#SBATCH --mem=5gb

module load R/4.0

R CMD BATCH --no-save nullStreets_tree_runfile.r ../Output_tree/a_out/tree${SLURM_ARRAY_TASK_ID}.Rout
