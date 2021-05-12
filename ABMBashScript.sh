#!/bin/sh
#SBATCH --account=plantpath                 # what account you are with
#SBATCH --qos=plantpath                     # use which account 
#SBATCH --job-name=SS.Run1            # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=betherton@ufl.edu           # Where to send mail	
#SBATCH --nodes=1                     # Use one node
#SBATCH --ntasks=1                    # Run a single task
#SBATCH --cpus-per-task=1             # Use 1 core
#SBATCH --mem=30gb                   # Memory limit
#SBATCH --time=20:00:00               # Time limit hrs:min:sec
#SBATCH --output=ABM.Run2%j.out   # Standard output and error log
#SBATCH --array=1-4:1

pwd;hostname;date

module load ufrc
module load R/3.6

ARGS=$(sed -n {SLURM_ARRAY_TASK_ID}p ParameterTable.txt)

Rscript --vanilla /blue/garrett/betherton/LW2020/ABMforHPG.R ${ARGS}
date