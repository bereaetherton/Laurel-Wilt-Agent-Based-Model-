#!/bin/sh
#SBATCH --account=epi                 # what account you are with
#SBATCH --qos=epi                     # use which account 
#SBATCH --job-name=SS.Run1            # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=betherton@ufl.edu           # Where to send mail	
#SBATCH --nodes=1                     # Use one node
#SBATCH --ntasks=1                    # Run a single task
#SBATCH --cpus-per-task=12             # Use 1 core
#SBATCH --mem=4gb                   # Memory limit
#SBATCH --time=150:00:00               # Time limit hrs:min:sec
#SBATCH --output=ABM.Run2%j.out   # Standard output and error log
#SBATCH --array=1-14:1

pwd;hostname;date

module load ufrc
module load R/3.6

ARG=$(sed -n ${SLURM_ARRAY_TASK_ID}p ParameterTable.txt)
echo ${ARG} 
Rscript --vanilla ABMforHPG_par.R ${ARG}
date
