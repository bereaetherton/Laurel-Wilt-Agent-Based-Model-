#!/bin/sh
#SBATCH --account=epi                # what account you are with
#SBATCH --qos=epi-b                   # use which account 
#SBATCH --job-name=ABM.Run6.1            # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=betherton@ufl.edu           # Where to send mail	
#SBATCH --nodes=1                   # Use one node
#SBATCH --nodes=1                     # Use one node
#SBATCH --ntasks=1                    # Run a single task
#SBATCH --cpus-per-task=12             # Use 1 core
#SBATCH --mem=6GB                   # Memory limit
#SBATCH --time=90:00:00               # Time limit hrs:min:sec
#SBATCH --output=ABM.Run6.1%j.out   # Standard output and error log
#SBATCH --array=1-267%100

pwd;hostname;date

module load ufrc
module load R/3.6

ARG=$(sed -n ${SLURM_ARRAY_TASK_ID}p ParameterTable.txt)
echo ${ARG} 
Rscript --vanilla NewABMforHPG_par.R ${ARG}
date
