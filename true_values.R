###########################################################################################
###########################################################################################
# - Obtains (approximate) true values of the estimands.
# - Simulates longitudinal and time-to-event data as though from a large RCT, as described in Section 6.1 (under "performance measures"). 
# - The seed is set at the start of the simulation.
#
# - Saves survival probabilities under treatment strategies "always treated" and "never treated", and corresponding risk differences.
#
# - The results are saved in the folder "results_detailed_msm".
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#Set seed
set.seed(431879)

#sample size 
n=1000000

#number of visits (K+1)
n.visit=5

#expit function
expit=function(x){exp(x)/(1+exp(x))}

#parameter values for hazard model
alpha.0=0.2
alpha.A=-0.04
alpha.L=0.015
alpha.U=0.015

#generate L[,1] (i.e. L0) which is the same in both treatment strategy groups
L=matrix(nrow=n,ncol=n.visit)
U=rnorm(n,0,0.1)
L.common.baseline=rnorm(n,U,1)

#--------------------------------------------
#A=1 group ("always treated"): generate L[,2:5] and event times
#--------------------------------------------

A=1
L[,1]=L.common.baseline
for(k in 2:n.visit){
  L[,k]=rnorm(n,0.8*L[,k-1]-A+0.1*(k-1)+U,1)
}

T.obs=rep(NA,n)
for(k in 1:n.visit){
  u.t=runif(n,0,1)
  haz=alpha.0+alpha.A*A+alpha.L*L[,k]+alpha.U*U
  new.t=-log(u.t)/haz
  T.obs=ifelse(is.na(T.obs) & new.t<1 & haz>0,k-1+new.t,T.obs)#the haz>0 is just used to deal with tiny possibility (under this data generating mechanism) the hazard could go negative. 
}
D.obs=ifelse(is.na(T.obs),0,1)
T.obs=ifelse(is.na(T.obs),5,T.obs)

dat.A1=data.frame(id=1:n,T.obs,D.obs,A=1)

#--------------------------------------------
#A=0 group ("always treated"): generate L[,2:5] and event times
#--------------------------------------------

A=0
L[,1]=L.common.baseline
for(k in 2:n.visit){
  L[,k]=rnorm(n,0.8*L[,k-1]-A+0.1*(k-1)+U,1)
}

T.obs=rep(NA,n)
for(k in 1:n.visit){
  u.t=runif(n,0,1)
  haz=alpha.0+alpha.A*A+alpha.L*L[,k]+alpha.U*U
  new.t=-log(u.t)/haz
  T.obs=ifelse(is.na(T.obs) & new.t<1 & haz>0,k-1+new.t,T.obs)#the haz>0 is just used to deal with tiny possibility (under this data generating mechanism) the hazard could go negative. 
}
D.obs=ifelse(is.na(T.obs),0,1)
T.obs=ifelse(is.na(T.obs),5,T.obs)

dat.A0=data.frame(id=1:n,T.obs,D.obs,A=0)

#--------------------------------------------
#survival curves in A=1 ("always treated") and A=0 ("never treated") groups
#--------------------------------------------

#combine data rrom the two treatment strategies
dat=rbind(dat.A1,dat.A0)

#Kaplan-Meier estimates of survival probabilities
surv.A=survfit(Surv(T.obs,D.obs)~A,data=dat)
surv.A.summ=summary(surv.A,times=t.hor)

#Obtain survival probabilities under the two treatment strategies: "always treated" (surv1), "never treated" (surv0)
surv0_true=surv.A.summ$surv[surv.A.summ$strata=="A=0"]
surv1_true=surv.A.summ$surv[surv.A.summ$strata=="A=1"]

#corresponding risk differences
survdiff_true=surv1_true-surv0_true

#--------------------------------------------
#save
#--------------------------------------------

save(surv1_true,file="results_detailed_msm/surv1_true.RData")
save(surv0_true,file="results_detailed_msm/surv0_true.RData")
save(survdiff_true,file="results_detailed_msm/survdiff_true.RData")


