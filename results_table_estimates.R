###########################################################################################
###########################################################################################
# - Creates Table 2: estimates of survival probabilities under the treatment strategies "always treated" and "never treated" using the two methods. 
# - The table is saved in 6 parts corresponding to the two methods in three scenarios.
# - The subtables are saved in the folder "results" as:
# table2_MSMIPTW_scenario1.csv,table2_MSMIPTW_scenario2.csv,table2_MSMIPTW_scenario3
# table2_SEQTRIALS_scenario1.csv,table2_SEQTRIALS_scenario2.csv,table2_SEQTRIALS_scenario3
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#some set-up for the tables
#------------------------

#positions in t.hor corresponding to tau=1,2,3,4,5
t.pos=c(which(t.hor==1),which(t.hor==2),which(t.hor==3),which(t.hor==4),which(t.hor==5))

#function to print numbers to 3dp
sp=function(x){sprintf("%0.3f",x)}

#row and column names for table
names0=c("S1","","S0","","RD","")
names=c("Est (SD)","Bias (MC error)","Est (SD)","Bias (MC error)","Est (SD)","Bias (MC error)")
colnames=c("","","tau=1","tau=2","tau=3","tau=4","tau=5")

#------------------------
#Create numbers needed for the table
#------------------------

#MSM-IPTW, scenario 1

surv1_msm_scen1=sp(colMeans(surv1_msm_swL_scen1[,t.pos]))
surv0_msm_scen1=sp(colMeans(surv0_msm_swL_scen1[,t.pos]))
survdiff_msm_scen1=sp(colMeans(survdiff_msm_swL_scen1[,t.pos]))

sdsurv1_msm_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen1[,x])})[t.pos])
sdsurv0_msm_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen1[,x])})[t.pos])
sdsurvdiff_msm_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen1[,x])})[t.pos])

surv1bias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_msm_swL_scen1[,x]-surv1_true[x])})[t.pos])
surv0bias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_msm_swL_scen1[,x]-surv0_true[x])})[t.pos])
survdiffbias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_msm_swL_scen1[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen1[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen1[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_msm_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen1[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#Sequential trials, scenario 1

surv1_seq_scen1=sp(colMeans(surv1_seq_sw_scen1[,t.pos]))
surv0_seq_scen1=sp(colMeans(surv0_seq_sw_scen1[,t.pos]))
survdiff_seq_scen1=sp(colMeans(survdiff_seq_sw_scen1[,t.pos]))

sdsurv1_seq_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen1[,x])})[t.pos])
sdsurv0_seq_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen1[,x])})[t.pos])
sdsurvdiff_seq_scen1=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen1[,x])})[t.pos])

surv1bias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_seq_sw_scen1[,x]-surv1_true[x])})[t.pos])
surv0bias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_seq_sw_scen1[,x]-surv0_true[x])})[t.pos])
survdiffbias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_seq_sw_scen1[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen1[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen1[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_seq_scen1=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen1[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#MSM-IPTW, scenario 2

surv1_msm_scen2=sp(colMeans(surv1_msm_swL_scen2[,t.pos]))
surv0_msm_scen2=sp(colMeans(surv0_msm_swL_scen2[,t.pos]))
survdiff_msm_scen2=sp(colMeans(survdiff_msm_swL_scen2[,t.pos]))

sdsurv1_msm_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen2[,x])})[t.pos])
sdsurv0_msm_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen2[,x])})[t.pos])
sdsurvdiff_msm_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen2[,x])})[t.pos])

surv1bias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_msm_swL_scen2[,x]-surv1_true[x])})[t.pos])
surv0bias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_msm_swL_scen2[,x]-surv0_true[x])})[t.pos])
survdiffbias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_msm_swL_scen2[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen2[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen2[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_msm_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen2[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#Sequential trials, scenario 2

surv1_seq_scen2=sp(colMeans(surv1_seq_sw_scen2[,t.pos]))
surv0_seq_scen2=sp(colMeans(surv0_seq_sw_scen2[,t.pos]))
survdiff_seq_scen2=sp(colMeans(survdiff_seq_sw_scen2[,t.pos]))

sdsurv1_seq_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen2[,x])})[t.pos])
sdsurv0_seq_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen2[,x])})[t.pos])
sdsurvdiff_seq_scen2=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen2[,x])})[t.pos])

surv1bias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_seq_sw_scen2[,x]-surv1_true[x])})[t.pos])
surv0bias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_seq_sw_scen2[,x]-surv0_true[x])})[t.pos])
survdiffbias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_seq_sw_scen2[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen2[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen2[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_seq_scen2=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen2[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#MSM-IPTW, scenario 3

surv1_msm_scen3=sp(colMeans(surv1_msm_swL_scen3[,t.pos]))
surv0_msm_scen3=sp(colMeans(surv0_msm_swL_scen3[,t.pos]))
survdiff_msm_scen3=sp(colMeans(survdiff_msm_swL_scen3[,t.pos]))

sdsurv1_msm_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen3[,x])})[t.pos])
sdsurv0_msm_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen3[,x])})[t.pos])
sdsurvdiff_msm_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen3[,x])})[t.pos])

surv1bias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_msm_swL_scen3[,x]-surv1_true[x])})[t.pos])
surv0bias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_msm_swL_scen3[,x]-surv0_true[x])})[t.pos])
survdiffbias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_msm_swL_scen3[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_msm_swL_scen3[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_msm_swL_scen3[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_msm_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen3[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#Sequential trials, scenario 3

surv1_seq_scen3=sp(colMeans(surv1_seq_sw_scen3[,t.pos]))
surv0_seq_scen3=sp(colMeans(surv0_seq_sw_scen3[,t.pos]))
survdiff_seq_scen3=sp(colMeans(survdiff_seq_sw_scen3[,t.pos]))

sdsurv1_seq_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen3[,x])})[t.pos])
sdsurv0_seq_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen3[,x])})[t.pos])
sdsurvdiff_seq_scen3=sp(sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen3[,x])})[t.pos])

surv1bias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv1_seq_sw_scen3[,x]-surv1_true[x])})[t.pos])
surv0bias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(surv0_seq_sw_scen3[,x]-surv0_true[x])})[t.pos])
survdiffbias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){mean(survdiff_seq_sw_scen3[,x]-survdiff_true[x])})[t.pos])

sdsurv1bias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv1_seq_sw_scen3[,x]-surv1_true[x])/sqrt(1000)})[t.pos])
sdsurv0bias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(surv0_seq_sw_scen3[,x]-surv0_true[x])/sqrt(1000)})[t.pos])
sdsurvdiffbias_seq_scen3=sp((10^2)*sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen3[,x]-survdiff_true[x])/sqrt(1000)})[t.pos])

#----------------------------------------------
#Create and save tables (Table 2 in 6 parts)
#----------------------------------------------

#MSM-IPTW, scenario 1

msm.scen1=cbind(names0,names,rbind(paste0(surv1_msm_scen1," (",sdsurv1_msm_scen1,")"),
                               paste0(surv1bias_msm_scen1," (",sdsurv1bias_msm_scen1,")"),
                               paste0(surv0_msm_scen1," (",sdsurv0_msm_scen1,")"),
                               paste0(surv0bias_msm_scen1," (",sdsurv0bias_msm_scen1,")"),
                               paste0(survdiff_msm_scen1," (",sdsurvdiff_msm_scen1,")"),
                               paste0(survdiffbias_msm_scen1," (",sdsurvdiffbias_msm_scen1,")")))
write.table(msm.scen1,"results/table2_MSMIPTW_scenario1.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 1

seq.scen1=cbind(names0,names,rbind(paste0(surv1_seq_scen1," (",sdsurv1_seq_scen1,")"),
                               paste0(surv1bias_seq_scen1," (",sdsurv1bias_seq_scen1,")"),
                               paste0(surv0_seq_scen1," (",sdsurv0_seq_scen1,")"),
                               paste0(surv0bias_seq_scen1," (",sdsurv0bias_seq_scen1,")"),
                               paste0(survdiff_seq_scen1," (",sdsurvdiff_seq_scen1,")"),
                               paste0(survdiffbias_seq_scen1," (",sdsurvdiffbias_seq_scen1,")")))
write.table(seq.scen1,"results/table2_SEQTRIALS_scenario1.csv",sep=",",row.names = F,col.names = colnames)

#MSM-IPTW, scenario 2

msm.scen2=cbind(names0,names,rbind(paste0(surv1_msm_scen2," (",sdsurv1_msm_scen2,")"),
                               paste0(surv1bias_msm_scen2," (",sdsurv1bias_msm_scen2,")"),
                               paste0(surv0_msm_scen2," (",sdsurv0_msm_scen2,")"),
                               paste0(surv0bias_msm_scen2," (",sdsurv0bias_msm_scen2,")"),
                               paste0(survdiff_msm_scen2," (",sdsurvdiff_msm_scen2,")"),
                               paste0(survdiffbias_msm_scen2," (",sdsurvdiffbias_msm_scen2,")")))
write.table(msm.scen2,"results/table2_MSMIPTW_scenario2.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 2

seq.scen2=cbind(names0,names,rbind(paste0(surv1_seq_scen2," (",sdsurv1_seq_scen2,")"),
                               paste0(surv1bias_seq_scen2," (",sdsurv1bias_seq_scen2,")"),
                               paste0(surv0_seq_scen2," (",sdsurv0_seq_scen2,")"),
                               paste0(surv0bias_seq_scen2," (",sdsurv0bias_seq_scen2,")"),
                               paste0(survdiff_seq_scen2," (",sdsurvdiff_seq_scen2,")"),
                               paste0(survdiffbias_seq_scen2," (",sdsurvdiffbias_seq_scen2,")")))
write.table(seq.scen2,"results/table2_SEQTRIALS_scenario2.csv",sep=",",row.names = F,col.names = colnames)


#MSM-IPTW, scenario 3

msm.scen3=cbind(names0,names,rbind(paste0(surv1_msm_scen3," (",sdsurv1_msm_scen3,")"),
                    paste0(surv1bias_msm_scen3," (",sdsurv1bias_msm_scen3,")"),
                    paste0(surv0_msm_scen3," (",sdsurv0_msm_scen3,")"),
                    paste0(surv0bias_msm_scen3," (",sdsurv0bias_msm_scen3,")"),
                    paste0(survdiff_msm_scen3," (",sdsurvdiff_msm_scen3,")"),
                    paste0(survdiffbias_msm_scen3," (",sdsurvdiffbias_msm_scen3,")")))
write.table(msm.scen3,"results/table2_MSMIPTW_scenario3.csv",sep=",",row.names = F,col.names = colnames)

#Sequential trials, scenario 3

seq.scen3=cbind(names0,names,rbind(paste0(surv1_seq_scen3," (",sdsurv1_seq_scen3,")"),
                                             paste0(surv1bias_seq_scen3," (",sdsurv1bias_seq_scen3,")"),
                                             paste0(surv0_seq_scen3," (",sdsurv0_seq_scen3,")"),
                                             paste0(surv0bias_seq_scen3," (",sdsurv0bias_seq_scen3,")"),
                                             paste0(survdiff_seq_scen3," (",sdsurvdiff_seq_scen3,")"),
                                             paste0(survdiffbias_seq_scen3," (",sdsurvdiffbias_seq_scen3,")")))
write.table(seq.scen3,"results/table2_SEQTRIALS_scenario3.csv",sep=",",row.names = F,col.names = colnames)

