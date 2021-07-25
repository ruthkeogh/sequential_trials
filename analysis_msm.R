###########################################################################################
###########################################################################################
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
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#--------------------------
#Store simulation results
#--------------------------

surv1_msm_sw=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_msm_sw=matrix(nrow=n.sim,ncol=length(t.hor))

surv1_msm_swtrunc=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_msm_swtrunc=matrix(nrow=n.sim,ncol=length(t.hor))

surv1_msm_swL=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_msm_swL=matrix(nrow=n.sim,ncol=length(t.hor))

surv1_msm_swLtrunc=matrix(nrow=n.sim,ncol=length(t.hor))
surv0_msm_swLtrunc=matrix(nrow=n.sim,ncol=length(t.hor))

weights_max_sw=matrix(nrow=n.sim,ncol=5)
weights_max_swL=matrix(nrow=n.sim,ncol=5)
weights_max_swtrunc=matrix(nrow=n.sim,ncol=5)
weights_max_swLtrunc=matrix(nrow=n.sim,ncol=5)

num_by_visit_msm=matrix(nrow=n.sim,ncol=5)
prop_by_visit_msm=matrix(nrow=n.sim,ncol=5)
num_alwaystreat_msm=matrix(nrow=n.sim,ncol=5)
prop_alwaystreat_msm=matrix(nrow=n.sim,ncol=5)
num_nevertreat_msm=matrix(nrow=n.sim,ncol=5)
prop_nevertreat_msm=matrix(nrow=n.sim,ncol=5)

#--------------------------
#Simulation loop
#--------------------------

#Set seed
set.seed(2611)

for(sim in 1:n.sim){
  print(sim)
  
  #Simulate data
  source("dat_sim.R")
  
  #------------------
  #Estimate stabilized weights for analysis using the MSM that is not conditional on L0
  #The weights take into account that once a person starts treatment they always continue
  #------------------
  
  #Denominator of weights
  wt.mod=glm(A~L,family="binomial",data=dat.long[dat.long$Alag1==0,])
  pred.wt=predict(wt.mod,type = "response",newdata = dat.long)
  dat.long$wt=ifelse(dat.long$A==1,pred.wt,1-pred.wt)
  dat.long$wt=ifelse(dat.long$Alag1==1,1,dat.long$wt)
  dat.long$wt.cum=ave(dat.long$wt,dat.long$id,FUN=cumprod)
  
  #Numerator of stabilised weights: for the MSM that is not conditional on L0
  wt.mod.num=glm(A~Alag1*as.factor(visit),family="binomial",data=dat.long[dat.long$Alag1==0,])
  pred.wt.num=predict(wt.mod.num,type = "response",newdata = dat.long)
  dat.long$wt.num=ifelse(dat.long$A==1,pred.wt.num,1-pred.wt.num)
  dat.long$wt.num=ifelse(dat.long$Alag1==1,1,dat.long$wt.num)
  dat.long$wt.cum.num=ave(dat.long$wt.num,dat.long$id,FUN=cumprod)
  
  #Numerator of stabilised weights: for the MSM that is conditional on L0
  wt.mod.num.L=glm(A~L.baseline*as.factor(visit),family="binomial",data=dat.long[dat.long$Alag1==0,])
  pred.wt.num.L=predict(wt.mod.num.L,type = "response",newdata = dat.long)
  dat.long$wt.num.L=ifelse(dat.long$A==1,pred.wt.num.L,1-pred.wt.num.L)
  dat.long$wt.num.L=ifelse(dat.long$Alag1==1,1,dat.long$wt.num.L)
  dat.long$wt.cum.num.L=ave(dat.long$wt.num.L,dat.long$id,FUN=cumprod)
  
  #Stabilized weights: for the MSM that is not conditional on L0
  dat.long$ipw.s=dat.long$wt.cum.num/dat.long$wt.cum
  
  #Stabilized weights: for the MSM that is conditional on L0
  dat.long$ipw.s.L=dat.long$wt.cum.num.L/dat.long$wt.cum
  
  #Truncated weights
  pct95=quantile(dat.long$ipw.s,0.95)
  dat.long$ipw.s.trunc=ifelse(dat.long$ipw.s>pct95,pct95,dat.long$ipw.s)
  
  pct95=quantile(dat.long$ipw.s.L,0.95)
  dat.long$ipw.s.L.trunc=ifelse(dat.long$ipw.s.L>pct95,pct95,dat.long$ipw.s.L)
  
  #-----------------
  #MSM-IPTW analysis using stabilized weights, for the MSM that is not conditional on L0 
  #-----------------

  ah.sw.p0=aalen(Surv(time,time.stop,event)~A,data=dat.long[dat.long$time==0,],n.sim=0,weights = dat.long[dat.long$time==0,]$ipw.s)
  ah.sw.p1=aalen(Surv(time,time.stop,event)~A+Alag1,data=dat.long[dat.long$time==1,],n.sim=0,weights = dat.long[dat.long$time==1,]$ipw.s)
  ah.sw.p2=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2,data=dat.long[dat.long$time==2,],n.sim=0,weights = dat.long[dat.long$time==2,]$ipw.s)
  ah.sw.p3=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3,data=dat.long[dat.long$time==3,],n.sim=0,weights = dat.long[dat.long$time==3,]$ipw.s)
  ah.sw.p4=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+Alag4,data=dat.long[dat.long$time==4,],n.sim=0,weights = dat.long[dat.long$time==4,]$ipw.s)

  maxrow.p0=dim(ah.sw.p0$cum)[1]
  maxrow.p1=dim(ah.sw.p1$cum)[1]
  maxrow.p2=dim(ah.sw.p2$cum)[1]
  maxrow.p3=dim(ah.sw.p3$cum)[1]
  maxrow.p4=dim(ah.sw.p4$cum)[1]

  ah.sw.stepfun.int=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                           c(0,ah.sw.p0$cum[,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[maxrow.p2,2]+ah.sw.p3$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[maxrow.p2,2]+ah.sw.p3$cum[maxrow.p3,2]+ah.sw.p4$cum[-1,2]))

  ah.sw.stepfun.A=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                         c(0,ah.sw.p0$cum[,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[maxrow.p2,3]+ah.sw.p3$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[maxrow.p2,3]+ah.sw.p3$cum[maxrow.p3,3]+ah.sw.p4$cum[-1,3]))

  ah.sw.stepfun.Alag1=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               ah.sw.p1$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[maxrow.p2,4]+ah.sw.p3$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[maxrow.p2,4]+ah.sw.p3$cum[maxrow.p3,4]+ah.sw.p4$cum[-1,4]))

  ah.sw.stepfun.Alag2=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               ah.sw.p2$cum[-1,5],
                               ah.sw.p2$cum[maxrow.p2,5]+ah.sw.p3$cum[-1,5],
                               ah.sw.p2$cum[maxrow.p2,5]+ah.sw.p3$cum[maxrow.p3,5]+ah.sw.p4$cum[-1,5]))

  ah.sw.stepfun.Alag3=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               rep(0,dim(ah.sw.p2$cum)[1]-1),
                               ah.sw.p3$cum[-1,6],
                               ah.sw.p3$cum[maxrow.p3,6]+ah.sw.p4$cum[-1,6]))

  ah.sw.stepfun.Alag4=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               rep(0,dim(ah.sw.p2$cum)[1]-1),
                               rep(0,dim(ah.sw.p3$cum)[1]-1),
                               ah.sw.p4$cum[-1,7]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)

  surv1_msm_sw[sim,]=sapply(FUN=function(h){exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.A(h)+ah.sw.stepfun.Alag1(h)+ah.sw.stepfun.Alag2(h)+ah.sw.stepfun.Alag3(h)+ah.sw.stepfun.Alag4(h)))},t.hor)
  surv0_msm_sw[sim,]=sapply(FUN=function(h){exp(-(ah.sw.stepfun.int(h)))},t.hor)

  #-----------------
  #MSM-IPTW analysis using truncated stabilized weights, for the MSM that is not conditional on L0 
  #-----------------

  ah.sw.p0=aalen(Surv(time,time.stop,event)~A,data=dat.long[dat.long$time==0,],n.sim=0,weights = dat.long[dat.long$time==0,]$ipw.s.trunc)
  ah.sw.p1=aalen(Surv(time,time.stop,event)~A+Alag1,data=dat.long[dat.long$time==1,],n.sim=0,weights = dat.long[dat.long$time==1,]$ipw.s.trunc)
  ah.sw.p2=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2,data=dat.long[dat.long$time==2,],n.sim=0,weights = dat.long[dat.long$time==2,]$ipw.s.trunc)
  ah.sw.p3=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3,data=dat.long[dat.long$time==3,],n.sim=0,weights = dat.long[dat.long$time==3,]$ipw.s.trunc)
  ah.sw.p4=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+Alag4,data=dat.long[dat.long$time==4,],n.sim=0,weights = dat.long[dat.long$time==4,]$ipw.s.trunc)

  maxrow.p0=dim(ah.sw.p0$cum)[1]
  maxrow.p1=dim(ah.sw.p1$cum)[1]
  maxrow.p2=dim(ah.sw.p2$cum)[1]
  maxrow.p3=dim(ah.sw.p3$cum)[1]
  maxrow.p4=dim(ah.sw.p4$cum)[1]

  ah.sw.stepfun.int=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                           c(0,ah.sw.p0$cum[,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[maxrow.p2,2]+ah.sw.p3$cum[-1,2],
                             ah.sw.p0$cum[maxrow.p0,2]+ah.sw.p1$cum[maxrow.p1,2]+ah.sw.p2$cum[maxrow.p2,2]+ah.sw.p3$cum[maxrow.p3,2]+ah.sw.p4$cum[-1,2]))

  ah.sw.stepfun.A=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                         c(0,ah.sw.p0$cum[,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[maxrow.p2,3]+ah.sw.p3$cum[-1,3],
                           ah.sw.p0$cum[maxrow.p0,3]+ah.sw.p1$cum[maxrow.p1,3]+ah.sw.p2$cum[maxrow.p2,3]+ah.sw.p3$cum[maxrow.p3,3]+ah.sw.p4$cum[-1,3]))

  ah.sw.stepfun.Alag1=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               ah.sw.p1$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[maxrow.p2,4]+ah.sw.p3$cum[-1,4],
                               ah.sw.p1$cum[maxrow.p1,4]+ah.sw.p2$cum[maxrow.p2,4]+ah.sw.p3$cum[maxrow.p3,4]+ah.sw.p4$cum[-1,4]))

  ah.sw.stepfun.Alag2=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               ah.sw.p2$cum[-1,5],
                               ah.sw.p2$cum[maxrow.p2,5]+ah.sw.p3$cum[-1,5],
                               ah.sw.p2$cum[maxrow.p2,5]+ah.sw.p3$cum[maxrow.p3,5]+ah.sw.p4$cum[-1,5]))

  ah.sw.stepfun.Alag3=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               rep(0,dim(ah.sw.p2$cum)[1]-1),
                               ah.sw.p3$cum[-1,6],
                               ah.sw.p3$cum[maxrow.p3,6]+ah.sw.p4$cum[-1,6]))

  ah.sw.stepfun.Alag4=stepfun(c(ah.sw.p0$cum[,1], ah.sw.p1$cum[-1,1],ah.sw.p2$cum[-1,1],ah.sw.p3$cum[-1,1],ah.sw.p4$cum[-1,1]),
                             c(0,rep(0,dim(ah.sw.p0$cum)[1]),
                               rep(0,dim(ah.sw.p1$cum)[1]-1),
                               rep(0,dim(ah.sw.p2$cum)[1]-1),
                               rep(0,dim(ah.sw.p3$cum)[1]-1),
                               ah.sw.p4$cum[-1,7]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)
  
  surv1_msm_swtrunc[sim,]=sapply(FUN=function(h){exp(-(ah.sw.stepfun.int(h)+ah.sw.stepfun.A(h)+ah.sw.stepfun.Alag1(h)+ah.sw.stepfun.Alag2(h)+ah.sw.stepfun.Alag3(h)+ah.sw.stepfun.Alag4(h)))},t.hor)
  surv0_msm_swtrunc[sim,]=sapply(FUN=function(h){exp(-(ah.sw.stepfun.int(h)))},t.hor)

  #-----------------
  #MSM-IPTW analysis using stabilized weights, for the MSM that is conditional on L0 
  #-----------------

  ah.sw.L.p0=aalen(Surv(time,time.stop,event)~A+L.baseline,data=dat.long[dat.long$time==0,],n.sim=0,weights = dat.long[dat.long$time==0,]$ipw.s.L)
  ah.sw.L.p1=aalen(Surv(time,time.stop,event)~A+Alag1+L.baseline,data=dat.long[dat.long$time==1,],n.sim=0,weights = dat.long[dat.long$time==1,]$ipw.s.L)
  ah.sw.L.p2=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+L.baseline,data=dat.long[dat.long$time==2,],n.sim=0,weights = dat.long[dat.long$time==2,]$ipw.s.L)
  ah.sw.L.p3=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+L.baseline,data=dat.long[dat.long$time==3,],n.sim=0,weights = dat.long[dat.long$time==3,]$ipw.s.L)
  ah.sw.L.p4=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+Alag4+L.baseline,data=dat.long[dat.long$time==4,],n.sim=0,weights = dat.long[dat.long$time==4,]$ipw.s.L)

  maxrow.p0=dim(ah.sw.L.p0$cum)[1]
  maxrow.p1=dim(ah.sw.L.p1$cum)[1]
  maxrow.p2=dim(ah.sw.L.p2$cum)[1]
  maxrow.p3=dim(ah.sw.L.p3$cum)[1]
  maxrow.p4=dim(ah.sw.L.p4$cum)[1]

  ah.sw.L.stepfun.int=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                              c(0,ah.sw.L.p0$cum[,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[maxrow.p2,2]+ah.sw.L.p3$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[maxrow.p2,2]+ah.sw.L.p3$cum[maxrow.p3,2]+ah.sw.L.p4$cum[-1,2]))

  ah.sw.L.stepfun.A=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                            c(0,ah.sw.L.p0$cum[,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[maxrow.p2,3]+ah.sw.L.p3$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[maxrow.p2,3]+ah.sw.L.p3$cum[maxrow.p3,3]+ah.sw.L.p4$cum[-1,3]))

  ah.sw.L.stepfun.Alag1=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  ah.sw.L.p1$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[maxrow.p2,4]+ah.sw.L.p3$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[maxrow.p2,4]+ah.sw.L.p3$cum[maxrow.p3,4]+ah.sw.L.p4$cum[-1,4]))

  ah.sw.L.stepfun.Alag2=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  ah.sw.L.p2$cum[-1,5],
                                  ah.sw.L.p2$cum[maxrow.p2,5]+ah.sw.L.p3$cum[-1,5],
                                  ah.sw.L.p2$cum[maxrow.p2,5]+ah.sw.L.p3$cum[maxrow.p3,5]+ah.sw.L.p4$cum[-1,5]))

  ah.sw.L.stepfun.Alag3=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p2$cum)[1]-1),
                                  ah.sw.L.p3$cum[-1,6],
                                  ah.sw.L.p3$cum[maxrow.p3,6]+ah.sw.L.p4$cum[-1,6]))

  ah.sw.L.stepfun.Alag4=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p2$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p3$cum)[1]-1),
                                  ah.sw.L.p4$cum[-1,7]))

  ah.sw.L.stepfun.L=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                            c(0,ah.sw.L.p0$cum[,4],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[-1,5],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[-1,6],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[maxrow.p2,6]+ah.sw.L.p3$cum[-1,7],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[maxrow.p2,6]+ah.sw.L.p3$cum[maxrow.p3,7]+ah.sw.L.p4$cum[-1,8]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)
  
  #Values of L measured at time 0: used to get marginal survival probabilities
  L.baseline.dat=dat.long$L[dat.long$visit==1]

  surv1_msm_swL[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.L.stepfun.int(h)+ah.sw.L.stepfun.A(h)+ah.sw.L.stepfun.Alag1(h)+ah.sw.L.stepfun.Alag2(h)+ah.sw.L.stepfun.Alag3(h)+ah.sw.L.stepfun.Alag4(h)+ah.sw.L.stepfun.L(h)*L.baseline.dat)))},t.hor)
  surv0_msm_swL[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.L.stepfun.int(h)+ah.sw.L.stepfun.L(h)*L.baseline.dat)))},t.hor)

  #-----------------
  #MSM-IPTW analysis using truncated stabilized weights, for the MSM that is conditional on L0 
  #-----------------

  ah.sw.L.p0=aalen(Surv(time,time.stop,event)~A+L.baseline,data=dat.long[dat.long$time==0,],n.sim=0,weights = dat.long[dat.long$time==0,]$ipw.s.L.trunc)
  ah.sw.L.p1=aalen(Surv(time,time.stop,event)~A+Alag1+L.baseline,data=dat.long[dat.long$time==1,],n.sim=0,weights = dat.long[dat.long$time==1,]$ipw.s.L.trunc)
  ah.sw.L.p2=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+L.baseline,data=dat.long[dat.long$time==2,],n.sim=0,weights = dat.long[dat.long$time==2,]$ipw.s.L.trunc)
  ah.sw.L.p3=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+L.baseline,data=dat.long[dat.long$time==3,],n.sim=0,weights = dat.long[dat.long$time==3,]$ipw.s.L.trunc)
  ah.sw.L.p4=aalen(Surv(time,time.stop,event)~A+Alag1+Alag2+Alag3+Alag4+L.baseline,data=dat.long[dat.long$time==4,],n.sim=0,weights = dat.long[dat.long$time==4,]$ipw.s.L.trunc)

  maxrow.p0=dim(ah.sw.L.p0$cum)[1]
  maxrow.p1=dim(ah.sw.L.p1$cum)[1]
  maxrow.p2=dim(ah.sw.L.p2$cum)[1]
  maxrow.p3=dim(ah.sw.L.p3$cum)[1]
  maxrow.p4=dim(ah.sw.L.p4$cum)[1]

  ah.sw.L.stepfun.int=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                              c(0,ah.sw.L.p0$cum[,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[maxrow.p2,2]+ah.sw.L.p3$cum[-1,2],
                                ah.sw.L.p0$cum[maxrow.p0,2]+ah.sw.L.p1$cum[maxrow.p1,2]+ah.sw.L.p2$cum[maxrow.p2,2]+ah.sw.L.p3$cum[maxrow.p3,2]+ah.sw.L.p4$cum[-1,2]))

  ah.sw.L.stepfun.A=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                            c(0,ah.sw.L.p0$cum[,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[maxrow.p2,3]+ah.sw.L.p3$cum[-1,3],
                              ah.sw.L.p0$cum[maxrow.p0,3]+ah.sw.L.p1$cum[maxrow.p1,3]+ah.sw.L.p2$cum[maxrow.p2,3]+ah.sw.L.p3$cum[maxrow.p3,3]+ah.sw.L.p4$cum[-1,3]))

  ah.sw.L.stepfun.Alag1=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  ah.sw.L.p1$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[maxrow.p2,4]+ah.sw.L.p3$cum[-1,4],
                                  ah.sw.L.p1$cum[maxrow.p1,4]+ah.sw.L.p2$cum[maxrow.p2,4]+ah.sw.L.p3$cum[maxrow.p3,4]+ah.sw.L.p4$cum[-1,4]))

  ah.sw.L.stepfun.Alag2=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  ah.sw.L.p2$cum[-1,5],
                                  ah.sw.L.p2$cum[maxrow.p2,5]+ah.sw.L.p3$cum[-1,5],
                                  ah.sw.L.p2$cum[maxrow.p2,5]+ah.sw.L.p3$cum[maxrow.p3,5]+ah.sw.L.p4$cum[-1,5]))

  ah.sw.L.stepfun.Alag3=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p2$cum)[1]-1),
                                  ah.sw.L.p3$cum[-1,6],
                                  ah.sw.L.p3$cum[maxrow.p3,6]+ah.sw.L.p4$cum[-1,6]))

  ah.sw.L.stepfun.Alag4=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                                c(0,rep(0,dim(ah.sw.L.p0$cum)[1]),
                                  rep(0,dim(ah.sw.L.p1$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p2$cum)[1]-1),
                                  rep(0,dim(ah.sw.L.p3$cum)[1]-1),
                                  ah.sw.L.p4$cum[-1,7]))

  ah.sw.L.stepfun.L=stepfun(c(ah.sw.L.p0$cum[,1], ah.sw.L.p1$cum[-1,1],ah.sw.L.p2$cum[-1,1],ah.sw.L.p3$cum[-1,1],ah.sw.L.p4$cum[-1,1]),
                            c(0,ah.sw.L.p0$cum[,4],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[-1,5],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[-1,6],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[maxrow.p2,6]+ah.sw.L.p3$cum[-1,7],
                              ah.sw.L.p0$cum[maxrow.p0,4]+ah.sw.L.p1$cum[maxrow.p1,5]+ah.sw.L.p2$cum[maxrow.p2,6]+ah.sw.L.p3$cum[maxrow.p3,7]+ah.sw.L.p4$cum[-1,8]))

  #----
  #Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)

  surv1_msm_swLtrunc[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.L.stepfun.int(h)+ah.sw.L.stepfun.A(h)+ah.sw.L.stepfun.Alag1(h)+ah.sw.L.stepfun.Alag2(h)+ah.sw.L.stepfun.Alag3(h)+ah.sw.L.stepfun.Alag4(h)+ah.sw.L.stepfun.L(h)*L.baseline.dat)))},t.hor)
  surv0_msm_swLtrunc[sim,]=sapply(FUN=function(h){mean(exp(-(ah.sw.L.stepfun.int(h)+ah.sw.L.stepfun.L(h)*L.baseline.dat)))},t.hor)
  
  #------------------
  #Save maximum weight in each time period
  #------------------
  
  weights_max_sw[sim,]= c(max(dat.long$ipw.s[dat.long$time==0]), max(dat.long$ipw.s[dat.long$time==1]),
                          max(dat.long$ipw.s[dat.long$time==2]),max(dat.long$ipw.s[dat.long$time==3]),
                          max(dat.long$ipw.s[dat.long$time==4]))
  
  weights_max_swL[sim,]= c(max(dat.long$ipw.s.L[dat.long$time==0]), max(dat.long$ipw.s.L[dat.long$time==1]),
                          max(dat.long$ipw.s.L[dat.long$time==2]),max(dat.long$ipw.s.L[dat.long$time==3]),
                          max(dat.long$ipw.s.L[dat.long$time==4]))

  weights_max_swtrunc[sim,]= c(max(dat.long$ipw.s.trunc[dat.long$time==0]), max(dat.long$ipw.s.trunc[dat.long$time==1]),
                          max(dat.long$ipw.s.trunc[dat.long$time==2]),max(dat.long$ipw.s.trunc[dat.long$time==3]),
                          max(dat.long$ipw.s.trunc[dat.long$time==4]))
  
  weights_max_swLtrunc[sim,]= c(max(dat.long$ipw.s.L.trunc[dat.long$time==0]), max(dat.long$ipw.s.L.trunc[dat.long$time==1]),
                           max(dat.long$ipw.s.L.trunc[dat.long$time==2]),max(dat.long$ipw.s.L.trunc[dat.long$time==3]),
                           max(dat.long$ipw.s.L.trunc[dat.long$time==4]))
  
  #------------------
  #Number (%) of individuals at each time point, and number (%) "always treated" and "never treated"
  #------------------
  
  dat.long=dat.long %>% group_by(id) %>% mutate(Acum=cumsum(A))
  
  #number remaining at each visit
  
  num_by_visit_msm[sim,]=c(dim(dat.long[dat.long$time==0,])[1],
                           dim(dat.long[dat.long$time==1,])[1],
                           dim(dat.long[dat.long$time==2,])[1],
                           dim(dat.long[dat.long$time==3,])[1],
                           dim(dat.long[dat.long$time==4,])[1])
  
  prop_by_visit_msm[sim,]=c(dim(dat.long[dat.long$time==0,])[1]/n,
                            dim(dat.long[dat.long$time==1,])[1]/n,
                            dim(dat.long[dat.long$time==2,])[1]/n,
                            dim(dat.long[dat.long$time==3,])[1]/n,
                            dim(dat.long[dat.long$time==4,])[1]/n)
  
  #proportion treated at visit 1, 1+2, 1+2+3, etc
  

  
  num_alwaystreat_msm[sim,]=c(sum(dat.long$Acum[dat.long$time==0]==1),
                              sum(dat.long$Acum[dat.long$time==1]==2),
                              sum(dat.long$Acum[dat.long$time==2]==3),
                              sum(dat.long$Acum[dat.long$time==3]==4),
                              sum(dat.long$Acum[dat.long$time==4]==5))
  
  prop_alwaystreat_msm[sim,]=c(sum(dat.long$Acum[dat.long$time==0]==1)/num_by_visit_msm[sim,1],
                               sum(dat.long$Acum[dat.long$time==1]==2)/num_by_visit_msm[sim,2],
                               sum(dat.long$Acum[dat.long$time==2]==3)/num_by_visit_msm[sim,3],
                               sum(dat.long$Acum[dat.long$time==3]==4)/num_by_visit_msm[sim,4],
                               sum(dat.long$Acum[dat.long$time==4]==5)/num_by_visit_msm[sim,5])
  
  num_nevertreat_msm[sim,]=c(sum(dat.long$Acum[dat.long$time==0]==0),
                             sum(dat.long$Acum[dat.long$time==1]==0),
                             sum(dat.long$Acum[dat.long$time==2]==0),
                             sum(dat.long$Acum[dat.long$time==3]==0),
                             sum(dat.long$Acum[dat.long$time==4]==0))
  
  prop_nevertreat_msm[sim,]=c(sum(dat.long$Acum[dat.long$time==0]==0)/num_by_visit_msm[sim,1],
                              sum(dat.long$Acum[dat.long$time==1]==0)/num_by_visit_msm[sim,2],
                              sum(dat.long$Acum[dat.long$time==2]==0)/num_by_visit_msm[sim,3],
                              sum(dat.long$Acum[dat.long$time==3]==0)/num_by_visit_msm[sim,4],
                              sum(dat.long$Acum[dat.long$time==4]==0)/num_by_visit_msm[sim,5])
  
}

#-----------------
#Generate risk differences
#-----------------

survdiff_msm_sw=surv1_msm_sw-surv0_msm_sw

survdiff_msm_swtrunc=surv1_msm_swtrunc-surv0_msm_swtrunc

survdiff_msm_swL=surv1_msm_swL-surv0_msm_swL

survdiff_msm_swLtrunc=surv1_msm_swLtrunc-surv0_msm_swLtrunc

#-----------------
#Save results
#-----------------

if(scenario==1){
  save(surv1_msm_sw,file="results_detailed_msm/surv1_msm_sw_scen1.RData")
  save(surv0_msm_sw,file="results_detailed_msm/surv0_msm_sw_scen1.RData")
  save(survdiff_msm_sw,file="results_detailed_msm/survdiff_msm_sw_scen1.RData")
  
  save(surv1_msm_swtrunc,file="results_detailed_msm/surv1_msm_swtrunc_scen1.RData")
  save(surv0_msm_swtrunc,file="results_detailed_msm/surv0_msm_swtrunc_scen1.RData")
  save(survdiff_msm_swtrunc,file="results_detailed_msm/survdiff_msm_swtrunc_scen1.RData")
  
  save(surv1_msm_swL,file="results_detailed_msm/surv1_msm_swL_scen1.RData")
  save(surv0_msm_swL,file="results_detailed_msm/surv0_msm_swL_scen1.RData")
  save(survdiff_msm_swL,file="results_detailed_msm/survdiff_msm_swL_scen1.RData")
  
  save(surv1_msm_swLtrunc,file="results_detailed_msm/surv1_msm_swLtrunc_scen1.RData")
  save(surv0_msm_swLtrunc,file="results_detailed_msm/surv0_msm_swLtrunc_scen1.RData")
  save(survdiff_msm_swLtrunc,file="results_detailed_msm/survdiff_msm_swLtrunc_scen1.RData")
  
  save(weights_max_sw,file="results_detailed_msm/weights_max_sw_scen1.RData")
  save(weights_max_swtrunc,file="results_detailed_msm/weights_max_swtrunc_scen1.RData")
  save(weights_max_swL,file="results_detailed_msm/weights_max_swL_scen1.RData")
  save(weights_max_swLtrunc,file="results_detailed_msm/weights_max_swLtrunc_scen1.RData")
  
  save(num_by_visit_msm,file="results_detailed_msm/num_by_visit_msm_scen1.RData")
  save(prop_by_visit_msm,file="results_detailed_msm/prop_by_visit_msm_scen1.RData")
  
  save(num_alwaystreat_msm,file="results_detailed_msm/num_alwaystreat_msm_scen1.RData")
  save(prop_alwaystreat_msm,file="results_detailed_msm/prop_alwaystreat_msm_scen1.RData")
  
  save(num_nevertreat_msm,file="results_detailed_msm/num_nevertreat_msm_scen1.RData")
  save(prop_nevertreat_msm,file="results_detailed_msm/prop_nevertreat_msm_scen1.RData")
}

if(scenario==2){
  save(surv1_msm_sw,file="results_detailed_msm/surv1_msm_sw_scen2.RData")
  save(surv0_msm_sw,file="results_detailed_msm/surv0_msm_sw_scen2.RData")
  save(survdiff_msm_sw,file="results_detailed_msm/survdiff_msm_sw_scen2.RData")
  
  save(surv1_msm_swtrunc,file="results_detailed_msm/surv1_msm_swtrunc_scen2.RData")
  save(surv0_msm_swtrunc,file="results_detailed_msm/surv0_msm_swtrunc_scen2.RData")
  save(survdiff_msm_swtrunc,file="results_detailed_msm/survdiff_msm_swtrunc_scen2.RData")
  
  save(surv1_msm_swL,file="results_detailed_msm/surv1_msm_swL_scen2.RData")
  save(surv0_msm_swL,file="results_detailed_msm/surv0_msm_swL_scen2.RData")
  save(survdiff_msm_swL,file="results_detailed_msm/survdiff_msm_swL_scen2.RData")
  
  save(surv1_msm_swLtrunc,file="results_detailed_msm/surv1_msm_swLtrunc_scen2.RData")
  save(surv0_msm_swLtrunc,file="results_detailed_msm/surv0_msm_swLtrunc_scen2.RData")
  save(survdiff_msm_swLtrunc,file="results_detailed_msm/survdiff_msm_swLtrunc_scen2.RData")
  
  save(weights_max_sw,file="results_detailed_msm/weights_max_sw_scen2.RData")
  save(weights_max_swtrunc,file="results_detailed_msm/weights_max_swtrunc_scen2.RData")
  save(weights_max_swL,file="results_detailed_msm/weights_max_swL_scen2.RData")
  save(weights_max_swLtrunc,file="results_detailed_msm/weights_max_swLtrunc_scen2.RData")
  
  save(num_by_visit_msm,file="results_detailed_msm/num_by_visit_msm_scen2.RData")
  save(prop_by_visit_msm,file="results_detailed_msm/prop_by_visit_msm_scen2.RData")
  
  save(num_alwaystreat_msm,file="results_detailed_msm/num_alwaystreat_msm_scen2.RData")
  save(prop_alwaystreat_msm,file="results_detailed_msm/prop_alwaystreat_msm_scen2.RData")
  
  save(num_nevertreat_msm,file="results_detailed_msm/num_nevertreat_msm_scen2.RData")
  save(prop_nevertreat_msm,file="results_detailed_msm/prop_nevertreat_msm_scen2.RData")
}

if(scenario==3){
  save(surv1_msm_sw,file="results_detailed_msm/surv1_msm_sw_scen3.RData")
  save(surv0_msm_sw,file="results_detailed_msm/surv0_msm_sw_scen3.RData")
  save(survdiff_msm_sw,file="results_detailed_msm/survdiff_msm_sw_scen3.RData")
  
  save(surv1_msm_swtrunc,file="results_detailed_msm/surv1_msm_swtrunc_scen3.RData")
  save(surv0_msm_swtrunc,file="results_detailed_msm/surv0_msm_swtrunc_scen3.RData")
  save(survdiff_msm_swtrunc,file="results_detailed_msm/survdiff_msm_swtrunc_scen3.RData")
  
  save(surv1_msm_swL,file="results_detailed_msm/surv1_msm_swL_scen3.RData")
  save(surv0_msm_swL,file="results_detailed_msm/surv0_msm_swL_scen3.RData")
  save(survdiff_msm_swL,file="results_detailed_msm/survdiff_msm_swL_scen3.RData")
  
  save(surv1_msm_swLtrunc,file="results_detailed_msm/surv1_msm_swLtrunc_scen3.RData")
  save(surv0_msm_swLtrunc,file="results_detailed_msm/surv0_msm_swLtrunc_scen3.RData")
  save(survdiff_msm_swLtrunc,file="results_detailed_msm/survdiff_msm_swLtrunc_scen3.RData")
  
  save(weights_max_sw,file="results_detailed_msm/weights_max_sw_scen3.RData")
  save(weights_max_swtrunc,file="results_detailed_msm/weights_max_swtrunc_scen3.RData")
  save(weights_max_swL,file="results_detailed_msm/weights_max_swL_scen3.RData")
  save(weights_max_swLtrunc,file="results_detailed_msm/weights_max_swLtrunc_scen3.RData")

  save(num_by_visit_msm,file="results_detailed_msm/num_by_visit_msm_scen3.RData")
  save(prop_by_visit_msm,file="results_detailed_msm/prop_by_visit_msm_scen3.RData")
  
  save(num_alwaystreat_msm,file="results_detailed_msm/num_alwaystreat_msm_scen3.RData")
  save(prop_alwaystreat_msm,file="results_detailed_msm/prop_alwaystreat_msm_scen3.RData")
  
  save(num_nevertreat_msm,file="results_detailed_msm/num_nevertreat_msm_scen3.RData")
  save(prop_nevertreat_msm,file="results_detailed_msm/prop_nevertreat_msm_scen3.RData")
}

