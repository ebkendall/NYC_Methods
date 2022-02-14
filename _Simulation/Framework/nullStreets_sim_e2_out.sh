#!/bin/bash

#SBATCH -o ../Output/a_out/simOrigAndPlot%a.out
#SBATCH --array=1-100
#SBATCH --account=jantonelli
#SBATCH --qos=jantonelli-b
#SBATCH --job-name=simOrigAndPlot
#SBATCH --time=01:00:00
#SBATCH -t 4000
#SBATCH --mem=5gb

module load R/4.0

R CMD BATCH --no-save nullStreets_sim_e2_out.r ../Output/a_out/simOrigAndPlot${SLURM_ARRAY_TASK_ID}.Rout
