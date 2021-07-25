###########################################################################################
###########################################################################################
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
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#--------------------------
#Store simulation results
#--------------------------

surv1_seq_sw=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_seq_sw=matrix(nrow=n.sim,ncol=length(t.hor))

surv1_seq_swtrunc=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_seq_swtrunc=matrix(nrow=n.sim,ncol=length(t.hor))

weights_max_sw=matrix(nrow=n.sim,ncol=5)
weights_max_swtrunc=matrix(nrow=n.sim,ncol=5)

prop_time_seq=matrix(nrow=n.sim,ncol=5)
prop_exp_seq=matrix(nrow=n.sim,ncol=5)

num_by_visit_seq=matrix(nrow=n.sim,ncol=5)
prop_by_visit_seq=matrix(nrow=n.sim,ncol=5)
num_alwaystreat_seq=matrix(nrow=n.sim,ncol=5)
prop_alwaystreat_seq=matrix(nrow=n.sim,ncol=5)
num_nevertreat_seq=matrix(nrow=n.sim,ncol=5)
prop_nevertreat_seq=matrix(nrow=n.sim,ncol=5)

#--------------------------
#Simulation loop
#--------------------------

#Set seed
set.seed(2611)

for(sim in 1:n.sim){
  print(sim)
  
  #Simulate data
  source("dat_sim.R")
  
  #----
  #Create sequence of trials to set-up the data for the sequential trials analysis
  
  #Trial starting at time 0
  trial.0=dat.long
  
  trial.0=trial.0 %>%
    mutate(trial=0) %>%
    group_by(id) %>%
    mutate(ones=1)%>% mutate(rownum=cumsum(ones))%>%
    mutate(A.baseline=first(A))%>%
    mutate(L.baseline=first(L))%>%
    mutate(time.new=time-trial)%>%
    mutate(time.stop.new=time.stop-trial)
  
  #Trial starting at time 1
  trial.1=dat.long[dat.long$time>=1,]
  
  trial.1=trial.1 %>%
    mutate(trial=1) %>%
    group_by(id) %>%
    mutate(ones=1)%>% mutate(rownum=cumsum(ones))%>%
    mutate(time.new=time-trial)%>%
    mutate(time.stop.new=time.stop-trial)%>%
    mutate(Alag1.baseline=first(Alag1))%>%
    filter(Alag1.baseline==0)%>% #restrict to those not previously treated at the start of the trial
    mutate(A.baseline=first(A))%>%
    mutate(L.baseline=first(L))
  
  #Trial starting at time 2
  trial.2=dat.long[dat.long$time>=2,]
  
  trial.2=trial.2 %>%
    mutate(trial=2) %>%
    group_by(id) %>%
    mutate(ones=1)%>% mutate(rownum=cumsum(ones))%>%
    mutate(time.new=time-trial)%>%
    mutate(time.stop.new=time.stop-trial)%>%
    mutate(Alag1.baseline=first(Alag1))%>%
    filter(Alag1.baseline==0)%>% #restrict to those not previously treated at the start of the trial
    mutate(A.baseline=first(A))%>%
    mutate(L.baseline=first(L))
  
  #Trial starting at time 3
  trial.3=dat.long[dat.long$time>=3,]
  
  trial.3=trial.3 %>%
    mutate(trial=3) %>%
    group_by(id) %>%
    mutate(ones=1)%>% mutate(rownum=cumsum(ones))%>%
    mutate(time.new=time-trial)%>%
    mutate(time.stop.new=time.stop-trial)%>%
    mutate(Alag1.baseline=first(Alag1))%>%
    filter(Alag1.baseline==0)%>% #restrict to those not previously treated at the start of the trial
    mutate(A.baseline=first(A))%>%
    mutate(L.baseline=first(L))
  
  #Trial starting at time 4
  trial.4=dat.long[dat.long$time>=4,]
  
  trial.4=trial.4 %>%
    mutate(trial=4) %>%
    group_by(id) %>%
    mutate(ones=1)%>% mutate(rownum=cumsum(ones))%>%
    mutate(time.new=time-trial)%>%
    mutate(time.stop.new=time.stop-trial)%>%
    mutate(Alag1.baseline=first(Alag1))%>%
    filter(Alag1.baseline==0)%>% #restrict to those not previously treated at the start of the trial
    mutate(A.baseline=first(A))%>%
    mutate(L.baseline=first(L))
  
  #Combine trials starting at time 0,1,2,3,4 into one dataset
  dat.seq.orig=rbind(trial.0,trial.1,trial.2,trial.3,trial.4)
  
  #Generate indicator of whether the person is not censored at the end of the current period, i.e. whether their *next* A is equal to A.baseline
  dat.seq.orig=dat.seq.orig %>%group_by(id,trial) %>% mutate(A.lead1 = lead(A,n=1)) 
  dat.seq.orig=dat.seq.orig %>%group_by(id,trial) %>% mutate(L.lead1 = lead(L,n=1)) #this is needed for the IPACW modelbelow (wt.mod.denom)scens
  dat.seq.orig=dat.seq.orig %>% mutate(Anext.equal.to.baseline = ifelse(A.lead1==A.baseline,1,0)) 
  
  #Now impose the artificial censoring: restrict to rows where current treatment status is equal to A.baseline (treatment status at the start of the trial)
  dat.seq=dat.seq.orig[dat.seq.orig$A==dat.seq.orig$A.baseline,]
  
  #------------------
  #Estimate stabilized weights for analysis
  #The weights take into account that once a person starts treatment they always continue
  #------------------
  
  #Denominator of weights
  #Note that it is appropriate that Anext.equal.to.baseline is NA in the last row of data for a given individual. 
  #These rows do not contribute to the estimation of the artificial censoring weights
  wt.mod.denom=glm(Anext.equal.to.baseline~L.lead1,family = "binomial",data=dat.seq[dat.seq$A.baseline==0,])
  
  dat.seq$wt.denom=1
  dat.seq[dat.seq$rownum!=5,]$wt.denom=predict(wt.mod.denom,newdata=dat.seq[dat.seq$rownum!=5,],type="response")
  dat.seq$wt.denom=ifelse(dat.seq$A.baseline==1,1,dat.seq$wt.denom)
  
  #Numerator of weights
  wt.mod.num=glm(Anext.equal.to.baseline~as.factor(rownum)*L.baseline,family = "binomial",data=dat.seq[dat.seq$A.baseline==0,])
  
  dat.seq$wt.num=1
  dat.seq[dat.seq$rownum!=5,]$wt.num=predict(wt.mod.num,newdata=dat.seq[dat.seq$rownum!=5,],type="response")
  dat.seq$wt.num=ifelse(dat.seq$A.baseline==1,1,dat.seq$wt.num)
  
  #Stabilized weights
  dat.seq$ipw.stab=dat.seq$wt.num/dat.seq$wt.denom
  
  #We want to take the cumulative sum of the weights up to the previous row. i.e. the cumulative weight in row 3 is wt1*wt2. 
  #The weight in row 1 is always 1
  #The IPACW used in the analysis are ipw.stab.cum
  dat.seq=dat.seq %>% group_by(id, trial) %>%mutate(ipw.stab.lag = lag(ipw.stab,n=1))
  dat.seq=dat.seq %>% mutate(ipw.stab.lag = ifelse(rownum==1,1,ipw.stab.lag))
  
  dat.seq=dat.seq %>% group_by(id, trial) %>%mutate(ipw.stab.cum= cumprod(ipw.stab.lag))
  
  #Truncated weights
  pct95=quantile(dat.seq$ipw.stab.cum,0.95)
  dat.seq$ipw.stab.cum.trunc=ifelse(dat.seq$ipw.stab.cum>pct95,pct95,dat.seq$ipw.stab.cum)
  
  #-----------------
  #Sequential trials analysis using stabilized weights (IPACW)
  #-----------------

  ah.sw=aalen(Surv(time.new,time.stop.new,event)~A.baseline+L.baseline,data=dat.seq,n.sim=0,weights = dat.seq$ipw.stab.cum)

  ah.sw.stepfun.int=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,2]))
  ah.sw.stepfun.A=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,3]))
  ah.sw.stepfun.L=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,4]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)

  #Values of L measured at time 0: used to get marginal survival probabilities
  L.baseline.dat=dat.long$L[dat.long$visit==1]

  surv1_seq_sw[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.A(h)+ah.sw.stepfun.L(h)*L.baseline.dat)))},t.hor)
  surv0_seq_sw[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.L(h)*L.baseline.dat)))},t.hor)

  #-----------------
  #Sequential trials analysis using truncated stabilized weights (IPACW)
  #-----------------

  ah.sw=aalen(Surv(time.new,time.stop.new,event)~A.baseline+L.baseline,data=dat.seq,n.sim=0,weights = dat.seq$ipw.stab.cum.trunc)

  ah.sw.stepfun.int=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,2]))
  ah.sw.stepfun.A=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,3]))
  ah.sw.stepfun.L=stepfun(ah.sw$cum[,1],c(0,ah.sw$cum[,4]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)

  surv1_seq_swtrunc[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.A(h)+ah.sw.stepfun.L(h)*L.baseline.dat)))},t.hor)
  surv0_seq_swtrunc[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.L(h)*L.baseline.dat)))},t.hor)

  #------------------
  #Save maximum weight in each time period
  #------------------
  
  weights_max_sw[sim,]=c(max(dat.seq$ipw.stab.cum[dat.seq$time==0]),max(dat.seq$ipw.stab.cum[dat.seq$time==1]),
                          max(dat.seq$ipw.stab.cum[dat.seq$time==2]),max(dat.seq$ipw.stab.cum[dat.seq$time==3]),
                          max(dat.seq$ipw.stab.cum[dat.seq$time==4]))
  
  weights_max_swtrunc[sim,]=c(max(dat.seq$ipw.stab.cum.trunc[dat.seq$time==0]),max(dat.seq$ipw.stab.cum.trunc[dat.seq$time==1]),
                         max(dat.seq$ipw.stab.cum.trunc[dat.seq$time==2]),max(dat.seq$ipw.stab.cum.trunc[dat.seq$time==3]),
                         max(dat.seq$ipw.stab.cum.trunc[dat.seq$time==4]))
  
  #------------------
  #Number (%) of individuals at each time point, and number (%) "always treated" and "never treated"
  #------------------
  
  num_by_visit_seq[sim,]=c(dim(dat.seq[dat.seq$rownum==1,])[1],
                           dim(dat.seq[dat.seq$rownum==2,])[1],
                           dim(dat.seq[dat.seq$rownum==3,])[1],
                           dim(dat.seq[dat.seq$rownum==4,])[1],
                           dim(dat.seq[dat.seq$rownum==5,])[1])
  
  prop_by_visit_seq[sim,]=c(dim(dat.seq[dat.seq$rownum==1,])[1]/dim(dat.seq[dat.seq$rownum==1,])[1],
                            dim(dat.seq[dat.seq$rownum==2,])[1]/dim(dat.seq[dat.seq$rownum==1,])[1],
                            dim(dat.seq[dat.seq$rownum==3,])[1]/dim(dat.seq[dat.seq$rownum==1,])[1],
                            dim(dat.seq[dat.seq$rownum==4,])[1]/dim(dat.seq[dat.seq$rownum==1,])[1],
                            dim(dat.seq[dat.seq$rownum==5,])[1]/dim(dat.seq[dat.seq$rownum==1,])[1])

  num_alwaystreat_seq[sim,]=c(sum(dat.seq$A[dat.seq$rownum==1]==1),
                              sum(dat.seq$A[dat.seq$rownum==2]==1),
                              sum(dat.seq$A[dat.seq$rownum==3]==1),
                              sum(dat.seq$A[dat.seq$rownum==4]==1),
                              sum(dat.seq$A[dat.seq$rownum==5]==1))
  
  prop_alwaystreat_seq[sim,]=c(num_alwaystreat_seq[sim,1]/num_by_visit_seq[sim,1],
                               num_alwaystreat_seq[sim,2]/num_by_visit_seq[sim,2],
                               num_alwaystreat_seq[sim,3]/num_by_visit_seq[sim,3],
                               num_alwaystreat_seq[sim,4]/num_by_visit_seq[sim,4],
                               num_alwaystreat_seq[sim,5]/num_by_visit_seq[sim,5])
  
  num_nevertreat_seq[sim,]=c(sum(dat.seq$A[dat.seq$rownum==1]==0),
                             sum(dat.seq$A[dat.seq$rownum==2]==0),
                             sum(dat.seq$A[dat.seq$rownum==3]==0),
                             sum(dat.seq$A[dat.seq$rownum==4]==0),
                             sum(dat.seq$A[dat.seq$rownum==5]==0))
  
  prop_nevertreat_seq[sim,]=c(num_nevertreat_seq[sim,1]/num_by_visit_seq[sim,1],
                              num_nevertreat_seq[sim,2]/num_by_visit_seq[sim,2],
                              num_nevertreat_seq[sim,3]/num_by_visit_seq[sim,3],
                              num_nevertreat_seq[sim,4]/num_by_visit_seq[sim,4],
                              num_nevertreat_seq[sim,5]/num_by_visit_seq[sim,5])
}

#-----------------
#Generate risk differences
#-----------------

survdiff_seq_sw=surv1_seq_sw-surv0_seq_sw
survdiff_seq_swtrunc=surv1_seq_swtrunc-surv0_seq_swtrunc

#-----------------
#Save results
#-----------------

if(scenario==1){
  save(surv1_seq_sw,file="results_detailed_seq/surv1_seq_sw_scen1.RData")
  save(surv0_seq_sw,file="results_detailed_seq/surv0_seq_sw_scen1.RData")
  save(survdiff_seq_sw,file="results_detailed_seq/survdiff_seq_sw_scen1.RData")
  
  save(surv1_seq_swtrunc,file="results_detailed_seq/surv1_seq_swtrunc_scen1.RData")
  save(surv0_seq_swtrunc,file="results_detailed_seq/surv0_seq_swtrunc_scen1.RData")
  save(survdiff_seq_swtrunc,file="results_detailed_seq/survdiff_seq_swtrunc_scen1.RData")
  
  save(weights_max_sw,file="results_detailed_seq/weights_max_seq_sw_scen1.RData")
  save(weights_max_swtrunc,file="results_detailed_seq/weights_max_seq_swtrunc_scen1.RData")
  
  save(num_by_visit_seq,file="results_detailed_seq/num_by_visit_seq_scen1.RData")
  save(prop_by_visit_seq,file="results_detailed_seq/prop_by_visit_seq_scen1.RData")
  
  save(num_alwaystreat_seq,file="results_detailed_seq/num_alwaystreat_seq_scen1.RData")
  save(prop_alwaystreat_seq,file="results_detailed_seq/prop_alwaystreat_seq_scen1.RData")
  
  save(num_nevertreat_seq,file="results_detailed_seq/num_nevertreat_seq_scen1.RData")
  save(prop_nevertreat_seq,file="results_detailed_seq/prop_nevertreat_seq_scen1.RData")
}

if(scenario==2){
  save(surv1_seq_sw,file="results_detailed_seq/surv1_seq_sw_scen2.RData")
  save(surv0_seq_sw,file="results_detailed_seq/surv0_seq_sw_scen2.RData")
  save(survdiff_seq_sw,file="results_detailed_seq/survdiff_seq_sw_scen2.RData")
  
  save(surv1_seq_swtrunc,file="results_detailed_seq/surv1_seq_swtrunc_scen2.RData")
  save(surv0_seq_swtrunc,file="results_detailed_seq/surv0_seq_swtrunc_scen2.RData")
  save(survdiff_seq_swtrunc,file="results_detailed_seq/survdiff_seq_swtrunc_scen2.RData")
  
  save(weights_max_sw,file="results_detailed_seq/weights_max_seq_sw_scen2.RData")
  save(weights_max_swtrunc,file="results_detailed_seq/weights_max_seq_swtrunc_scen2.RData")
  
  save(num_by_visit_seq,file="results_detailed_seq/num_by_visit_seq_scen2.RData")
  save(prop_by_visit_seq,file="results_detailed_seq/prop_by_visit_seq_scen2.RData")
  
  save(num_alwaystreat_seq,file="results_detailed_seq/num_alwaystreat_seq_scen2.RData")
  save(prop_alwaystreat_seq,file="results_detailed_seq/prop_alwaystreat_seq_scen2.RData")
  
  save(num_nevertreat_seq,file="results_detailed_seq/num_nevertreat_seq_scen2.RData")
  save(prop_nevertreat_seq,file="results_detailed_seq/prop_nevertreat_seq_scen2.RData")
}

if(scenario==3){
  save(surv1_seq_sw,file="results_detailed_seq/surv1_seq_sw_scen3.RData")
  save(surv0_seq_sw,file="results_detailed_seq/surv0_seq_sw_scen3.RData")
  save(survdiff_seq_sw,file="results_detailed_seq/survdiff_seq_sw_scen3.RData")
  
  save(surv1_seq_swtrunc,file="results_detailed_seq/surv1_seq_swtrunc_scen3.RData")
  save(surv0_seq_swtrunc,file="results_detailed_seq/surv0_seq_swtrunc_scen3.RData")
  save(survdiff_seq_swtrunc,file="results_detailed_seq/survdiff_seq_swtrunc_scen3.RData")
  
  save(weights_max_sw,file="results_detailed_seq/weights_max_seq_sw_scen3.RData")
  save(weights_max_swtrunc,file="results_detailed_seq/weights_max_seq_swtrunc_scen3.RData")
  
  save(num_by_visit_seq,file="results_detailed_seq/num_by_visit_seq_scen3.RData")
  save(prop_by_visit_seq,file="results_detailed_seq/prop_by_visit_seq_scen3.RData")
  
  save(num_alwaystreat_seq,file="results_detailed_seq/num_alwaystreat_seq_scen3.RData")
  save(prop_alwaystreat_seq,file="results_detailed_seq/prop_alwaystreat_seq_scen3.RData")
  
  save(num_nevertreat_seq,file="results_detailed_seq/num_nevertreat_seq_scen3.RData")
  save(prop_nevertreat_seq,file="results_detailed_seq/prop_nevertreat_seq_scen3.RData")
}
