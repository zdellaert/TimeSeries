#!/bin/bash
#SBATCH -t 02:00:00  # Job time limit
#SBATCH -o slurm-%j.out  # %j = job ID
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=20
#SBATCH --mail-type=BEGIN,END,FAIL,TIME_LIMIT_80

module load r-rocker-ml-verse/4.5.1_cuda12.8.1+apptainer
shopt -s expand_aliases

Rscript --no-restore --quiet --no-save 00_run_all_analyses.R