Instructions for reproducing paper results:

- The file Run_Paralell_ALL.R runs the simulation and prints all results to a text file and also saves them to an RData object.
- The results used in the paper are the combined results from the files results_real.txt and results_real_2.txt (first 100 parameter sets and then subsequent 300 parameter sets). All parameter vectors and their corresponding predicted population vectors in 2000 and 2010 can be found in this file.
- The file Bayesian_Melding.R produces the main results for the paper, including the coverage plots and the reported parameter values. 
- The file Bayesian_Melding_2.R reproduces the main results in the setting where the bias a = 0.
- The file Make_Posteriors_Narrower_2.R carries out the version where standard errors are capped at a maximum. 
- The file Square_Root.R redoes the main results with square root transformation applied to all outputs. This change did not change the covereage results at all. And while the standard errors get smaller due to the scale of the data, the posteriors are still unreasoanbly large. No big change from adding this.