#------------------------------------------------------
#------------------------------------------------------
# master.R
# - Implements the simulation study as described in Section 6 of the paper.
# - Summarises the simulation results to create the numbers shown in Table 2 and Supplementary Table 1.
#   and the plots in Figures 4-7 and Supplementary Figures 1-4.
# - Saves tables and Figures in the "results" folder. Intermediate results used to create the tables and figures are saved in the folders "results_detailed_msm" and "results_detailed_seq". 
# Created by Ruth Keogh
#------------------------------------------------------
#------------------------------------------------------

#--------------------------
#packages
#--------------------------

library(survival)
library(timereg)
library(tidyverse)
library(ggplot2)
library(gridExtra)

#--------------------------
#simulation parameters
#--------------------------

#numberof simulated data sets
n.sim=1000

#times at which survival probabilities are to be estimated
t.hor=seq(0,5,0.01)

#--------------------------
# analysis_msm.R
# scenario=1,2,3
#--------------------------
# - Data are simulated using the file dat_sim.R. 
# - Simulates longitudinal and time-to-event data as described in Section 6.1 of the paper.
# - Data are simulated under three scenarios (see Table 1). 
# - All three scenarios use this same simulation file, but different parameters are used in scenarios 1,2,3. 
#
# - After simulating the data, a correctly specified MSM (using IPTW) is fitted to the simulated data.
# - We fit an MSM that is not conditonal on baseline variable L0 (equation 33), and an MSM that is conditonal on baseline L0 (equation 34). 
# - Both MSMs are fitted using stabilized weights (equations 36 and 37). 
# - The two MSMs are also fitted using truncated weights. 
#
# - Obtains estimates of survival probabilities under two treatment strategies: "always treat" and "never treat".
# - Survival probabilities are obtained at times between 0 and 5 in increments of 0.01 (i.e. at times 0,0.01,0.02,...,4.98,4.99,5)
# - Corresponding differences in survival probabilities between the two treatment strategies are also obtained at each time point.
# - Saves the maximum weight in each time period (0,1),[1,2),[2,3),[3,4),[4,5).
# - Saves the number of individuals under observation at each visit  at times 0,1,2,3,4.
# - Saves the number of individuals "always treated" and "never treated" from visit 0 to k (k=0,1,2,3,4).
#
# - Everything is repeated over 1000 simulated data sets.
# - The seed is set at the start of the simuations.
#
# - The results are saved in the folder "results_detailed_msm".

scenario=1
source("analysis_msm.R")

scenario=2
source("analysis_msm.R")

scenario=3
source("analysis_msm.R")

#--------------------------
# analysis_seq.R
# scenario=1,2,3
#--------------------------
# - Data are simulated using the file dat_sim.R. 
# - Simulates longitudinal and time-to-event data as described in Section 6.1 of the paper.
# - Data are simulated under three scenarios (see Table 1). 
# - All three scenarios use this same simulation file, but different parameters are used in scenarios 1,2,3. 
#
# - After simulating the data, a sequence of trials is created from time origins 0,1,2,3,4, as required for the sequential trials approach.
# - A correctly specified MSM is fitted to the simulated data (equation 35) using the sequential trials aproach.
# - Stabilized weights are used for the IPACW (equation 38).
# - The sequential trials MSM is also fitted using truncated IPACW. 
#
# - Obtains estimates of survival probabilities under two treatment strategies: "always treat" and "never treat".
# - Survival probabilities are obtained at times between 0 and 5 in increments of 0.01 (i.e. at times 0,0.01,0.02,...,4.98,4.99,5)
# - Corresponding differences in survival probabilities between the two treatment strategies are also obtained at each time point.
# - Saves the maximum weight (IPACW) in each time period (0,1),[1,2),[2,3),[3,4),[4,5).
# - Saves the number of individuals under observation at each visit  at times 0,1,2,3,4.
# - Saves the number of individuals "always treated" and "never treated" from visit 0 to k (k=0,1,2,3,4).
# - Note that time is measured relative to the start of trial in the sequential trials approach. 
#
# - Everything is repeated over 1000 simulated data sets. 
# - The seed is set at the start of the simuations.
#
# - The results are saved in the folder "results_detailed_seq".

scenario=1
source("analysis_seq.R")

scenario=2
source("analysis_seq.R")

scenario=3
source("analysis_seq.R")

#--------------------------
# true_values.R
#--------------------------
# - Obtains (approximate) true values of the estimands.
# - Simulates longitudinal and time-to-event data as though from a large RCT, as described in Section 6.1 (under "performance measures"). 
# - The seed is set at the start of the simulation.
#
# - Saves survival probabilities under treatment strategies "always treated" and "never treated", and corresponding risk differences.
#
# - The results are saved in the folder "results_detailed_msm".
# - NOTE that we save these results in the folder "results_detailed_msm" just for convenience, and not because they are specific to the MSM-IPTW approach.

source("true_values.R")

#--------------------------
# results_presentation_setup.R
#--------------------------
# - Takes the results generated in analysis_msm.R and analysis_seq.R (for scenarios 1,2,3) and renames things with "_scen1", "_scen2", "_scen3".
# - I could have included this within the files analysis_msm.R and analysis_seq.R.
# - This file enables someone to take the results stored in the "results_detailed_msm" and "results_detailed_seq" folders and create tables and figures, without having to actually run the simulations themselves.

source("results_presentation_setup.R")

#--------------------------
# results_survival_curves_setup.R
#--------------------------
# - Puts the results in the format required to create the plots in Figures 4 and 5. 
# - This involves obtaining the average survival curves across the 1000 simulations.

source("results_survival_curves_setup.R")

#--------------------------
# results_plots_survival_curves.R
#--------------------------
# - Creates Figure 4: estimates of survival curves under the treatment strategies "always treated" and "never treated" using the two methods. 
# - The plot is saved in the folder "results" as figure4.pdf
# - This file also shows how to create similar plots, e.g. for the analyses using truncated weights, which are not shown in the paper. 

source("results_plots_survival_curves.R")

#--------------------------
# results_plots_bias.R
#--------------------------
# - Creates Figure 5: estimates of bias in risk differences under the treatment strategies "always treated" and "never treated" using the two methods.  
# - Creates Supplementary Figure 1 and Supplementary Figure 3, which show the estimates of bias obtained using different weights. 
# - The plots are saved in the folder "results" as figure5.pdf, suppfigure1.pdf, suppfigure3.pdf.

source("results_plots_bias.R")

#--------------------------
# results_plots_efficiency.R
#--------------------------
# - Creates Figure 6: estimates of the relative efficiency of the sequential trials approach relative to the MSM-IPTW approach for estimating the risk differences.
# - Creates Supplementary Figure 2, Supplementary Figure 4, which show the estimates of bias obtained using different weights.
# - The plots are saved in the folder "results" as figure6.pdf, suppfigure2.pdf, suppfigure4.pdf.

source("results_plots_efficiency.R")

#--------------------------
# results_plots_weights.R
#--------------------------
# - Creates Figure 7: plots of the largest weight (by time period) in each of the 1000 simulated data sets under the sequential trials analysis (IPACW) compared with the MSM-IPTW analysis
# - The plot is saved in the folder "results" as figure7.pdf.

source("results_plots_weights.R")

#--------------------------
# results_table_estimates.R
#--------------------------
# - Creates Table 2: estimates of survival probabilities under the treatment strategies "always treated" and "never treated" using the two methods. 
# - The table is saved in 6 parts corresponding to the two methods in three scenarios.
# - The subtables are saved in the folder "results" as:
# table2_MSMIPTW_scenario1.csv,table2_MSMIPTW_scenario2.csv,table2_MSMIPTW_scenario3
# table2_SEQTRIALS_scenario1.csv,table2_SEQTRIALS_scenario2.csv,table2_SEQTRIALS_scenario3

source("results_table_estimates.R")

#--------------------------
# results_table_summary.R
#--------------------------
# - Creates Supplementary Table 1: summary of number of individuals observed at each time, number treatment strategies "always treated" and "never treated", and corresponding percentages
# - The table is saved in 6 parts corresponding to the two methods in three scenarios.
# - The subtables are saved in the folder "results" as:
# supptable1_MSMIPTW_scenario1.csv,supptable1_MSMIPTW_scenario2.csv,supptable1_MSMIPTW_scenario3
# supptable1_SEQTRIALS_scenario1.csv,supptable1_SEQTRIALS_scenario2.csv,supptable1_SEQTRIALS_scenario3

source("results_table_summary.R")

