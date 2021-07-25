###########################################################################################
###########################################################################################
# - Creates Supplementary Table 1: summary of number of individuals observed at each time, number treatment strategies "always treated" and "never treated", and corresponding percentages
# - The table is saved in 6 parts corresponding to the two methods in three scenarios.
# - The subtables are saved in the folder "results" as:
# supptable1_MSMIPTW_scenario1.csv,supptable1_MSMIPTW_scenario2.csv,supptable1_MSMIPTW_scenario3
# supptable1_SEQTRIALS_scenario1.csv,supptable1_SEQTRIALS_scenario2.csv,supptable1_SEQTRIALS_scenario3
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#some set-up for the tables
#------------------------

#function to print numbers to 0dp
sp=function(x){sprintf("%0.0f",x)}

#row and column names for table
names0=c("Observed at time k","","Always treated from time 0 to k","","Never treated from time 0 to k","")
names=c("N","%","N","%","N","%")
colnames=c("","","k=0","k=1","k=2","k=3","k=4")

#----------------------------------------------
#Create and save tables (Supplementary table 1 in 6 parts)
#----------------------------------------------

#MSM-IPTW, scenario 1

msm.scen1=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_msm_scen1)),
  sp(100*colMeans(prop_by_visit_msm_scen1)),
  sp(colMeans(num_alwaystreat_msm_scen1)),
  sp(100*colMeans(prop_alwaystreat_msm_scen1)),
  sp(colMeans(num_nevertreat_msm_scen1)),
  sp(100*colMeans(prop_nevertreat_msm_scen1))))
write.table(msm.scen1,"results/supptable1_MSMIPTW_scenario1.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 1

seq.scen1=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_seq_scen1)),
  sp(100*colMeans(prop_by_visit_seq_scen1)),
  sp(colMeans(num_alwaystreat_seq_scen1)),
  sp(100*colMeans(prop_alwaystreat_seq_scen1)),
  sp(colMeans(num_nevertreat_seq_scen1)),
  sp(100*colMeans(prop_nevertreat_seq_scen1))))
write.table(seq.scen1,"results/supptable1_SEQTRIALS_scenario1.csv",sep=",",row.names = F,col.names = colnames)

#MSM-IPTW, scenario 2

msm.scen2=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_msm_scen2)),
  sp(100*colMeans(prop_by_visit_msm_scen2)),
  sp(colMeans(num_alwaystreat_msm_scen2)),
  sp(100*colMeans(prop_alwaystreat_msm_scen2)),
  sp(colMeans(num_nevertreat_msm_scen2)),
  sp(100*colMeans(prop_nevertreat_msm_scen2))))
write.table(msm.scen2,"results/supptable1_MSMIPTW_scenario2.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 2

seq.scen2=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_seq_scen2)),
  sp(100*colMeans(prop_by_visit_seq_scen2)),
  sp(colMeans(num_alwaystreat_seq_scen2)),
  sp(100*colMeans(prop_alwaystreat_seq_scen2)),
  sp(colMeans(num_nevertreat_seq_scen2)),
  sp(100*colMeans(prop_nevertreat_seq_scen2))))
write.table(seq.scen2,"results/supptable1_SEQTRIALS_scenario2.csv",sep=",",row.names = F,col.names = colnames)

#MSM-IPTW, scenario 3

msm.scen3=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_msm_scen3)),
  sp(100*colMeans(prop_by_visit_msm_scen3)),
  sp(colMeans(num_alwaystreat_msm_scen3)),
  sp(100*colMeans(prop_alwaystreat_msm_scen3)),
  sp(colMeans(num_nevertreat_msm_scen3)),
  sp(100*colMeans(prop_nevertreat_msm_scen3))))
write.table(msm.scen3,"results/supptable1_MSMIPTW_scenario3.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 3

seq.scen3=cbind(names0,names,rbind(
  sp(colMeans(num_by_visit_seq_scen3)),
  sp(100*colMeans(prop_by_visit_seq_scen3)),
  sp(colMeans(num_alwaystreat_seq_scen3)),
  sp(100*colMeans(prop_alwaystreat_seq_scen3)),
  sp(colMeans(num_nevertreat_seq_scen3)),
  sp(100*colMeans(prop_nevertreat_seq_scen3))))
write.table(seq.scen3,"results/supptable1_SEQTRIALS_scenario3.csv",sep=",",row.names = F,col.names = colnames)
