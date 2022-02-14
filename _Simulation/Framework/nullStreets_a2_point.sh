#!/bin/bash

#SBATCH -o OutputStrInfo_new/nullPoint%a.out
#SBATCH --array=1-77
#SBATCH --account=jantonelli
#SBATCH --qos=jantonelli-b
#SBATCH --job-name=nullStreetPoint
#SBATCH --time=01:00:00
#SBATCH -t 4000
#SBATCH --mem=5gb

module load R/4.0

R CMD BATCH --no-save nullStreets_a2_point.r OutputStrInfo_new/nullPoint${SLURM_ARRAY_TASK_ID}.Rout
