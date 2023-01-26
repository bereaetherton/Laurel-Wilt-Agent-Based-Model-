# Laurel-Wilt-Agent-Based-Model-

Authors: Berea Etheron,
	 Ricardo I Alcala

An agent-based model designed to simulate epidemic expansion and social information dissemination regarding how to manage an epidemic. This agent-based mode incoperates policy incentive structures (carrots and sticks) and the social behavior of stubbornness. This model was used to simulate the laurel wilt epidemic across Homestead Florida's avocado orchards. 

#ABMBashScript.sh is the SLURM script used when submitting this job to UF's HiPerGator; this script reads in the ParameterTable.txt and runs the simulation for each paramter combination. 

#distance.xlsx is a 1132x1132 matrix with the geodesic distances between groves.

#AgentBasedModel_LaurelWilt.R is the entire agent based model used for simulations

#av.centers.xlsx is a matrix containing data for Homestead's avocadp groves, used for simulations. This file is not available for privacy to the growers. 

#ParameterTable.txt is the list of parameters tested within the script. The parameters are the biophysical dispersal kernel, the social connectivity kernel, the lower bound of stubbornness tested, the upper bound, the money allocated to growers (carrots), and the percent tax on growers (sticks), respectively. 
