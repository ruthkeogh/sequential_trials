###########################################################################################
###########################################################################################
# - Simulates longitudinal and time-to-event data as described in Section 6.1 of the paper.
# - Data are simulated under three scenarios (see Table 1). 
# - All three scenarios use this same simulation file, but different parameters are used in scenarios 1,2,3. 
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------
#simulation parameters - common across all scenarios
#------------------

#----
#sample size 

n=1000

#----
#number of visits (K+1)

n.visit=5

#----
#expit function

expit=function(x){exp(x)/(1+exp(x))}

#------------------
#parameter values: scenario  1 (standard scenario)
#------------------

if(scenario==1){

  #model for A|L
  gamma.0=-1
  gamma.L=0.5
  
  #model for hazard
  alpha.0=0.2
  alpha.A=-0.04
  alpha.L=0.015
  alpha.U=0.015
}

#------------------
#parameter values: scenario 2
#Fewer individuals have A=1 at visit 1 (and beyond)
#------------------

if(scenario==2){
  #model for A|L
  gamma.0=-3
  gamma.L=0.5
  
  #model for hazard
  alpha.0=0.2
  alpha.A=-0.04
  alpha.L=0.015
  alpha.U=0.015
}

#------------------
#parameter values: scenario 3
#A_k depends more strongly on L_k
#------------------

if(scenario==3){
  #model for A|L
  gamma.0=-1
  gamma.L=3
  
  #model for hazard
  alpha.0=0.2
  alpha.A=-0.04
  alpha.L=0.015
  alpha.U=0.015
}

#------------------
#simulate data
#------------------

#----
#generate U, A, L

A=matrix(nrow=n,ncol=n.visit)
L=matrix(nrow=n,ncol=n.visit)

U=rnorm(n,0,0.1)
L[,1]=rnorm(n,U,1)
A[,1]=rbinom(n,1,expit(gamma.0+gamma.L*L[,1]))
for(k in 2:n.visit){
  L[,k]=rnorm(n,0.8*L[,k-1]-A[,k-1]+0.1*(k-1)+U,1)
  A[,k]=ifelse(A[,k-1]==1,1,rbinom(n,1,expit(gamma.0+gamma.L*L[,k])))
}

#----
#generate event times T.obs, and event indicators D.obs

T.obs=rep(NA,n)

for(k in 1:n.visit){
  u.t=runif(n,0,1)
  haz=alpha.0+alpha.A*A[,k]+alpha.L*L[,k]+alpha.U*U
  new.t=-log(u.t)/haz
  T.obs=ifelse(is.na(T.obs) & new.t<1 & haz>0,k-1+new.t,T.obs)#the haz>0 is just used to deal with tiny possibility (under this data generating mechanism) the hazard could go negative. 
}
D.obs=ifelse(is.na(T.obs),0,1)
T.obs=ifelse(is.na(T.obs),5,T.obs)

# #------------------
# #some summaries: may be useful if you wish to change the parameter values used above, to consider other scenarios.
# #------------------
# 
# #proportion always treated
# always.treat=A[,1]+A[,2]+A[,3]+A[,4]+A[,5]
# 
# #proportion never treated
# never.treat=(1-A[,1])+(1-A[,2])+(1-A[,3])+(1-A[,4])+(1-A[,5])
# 
# tabyl(always.treat)
# tabyl(never.treat)

#------------------
#Create data frame and reshape into 'long' format (multiple rows per individual: 1 row for each visit)
#------------------

colnames(A)=paste0("A.",0:4)
colnames(L)=paste0("L.",0:4)
dat=data.frame(id=1:n,T.obs,D.obs,A,L)

dat.long=reshape(data = dat,varying=c(paste0("A.",0:4),paste0("L.",0:4)),direction="long",idvar="id")
dat.long=dat.long[order(dat.long$id,dat.long$time),]

#generate start and stop times for each row
dat.long$time.stop=dat.long$time+1

dat.long=dat.long[dat.long$time<dat.long$T.obs,]

dat.long$time.stop=ifelse(dat.long$time.stop>dat.long$T.obs,dat.long$T.obs,dat.long$time.stop)

dat.long$event=ifelse(dat.long$time.stop==dat.long$T.obs & dat.long$D.obs==1,1,0)

#visit number
dat.long$visit=ave(rep(1,dim(dat.long)[1]),dat.long$id,FUN=cumsum)

#generate lagged A values
dat.long=dat.long %>%
  group_by(id) %>%
  mutate(Alag1 = lag(A,n=1),Alag2 = lag(A,n=2),Alag3 = lag(A,n=3),Alag4 = lag(A,n=4)) %>%
  mutate(Alag1=replace_na(Alag1,0),Alag2=replace_na(Alag2,0),Alag3=replace_na(Alag3,0),Alag4=replace_na(Alag4,0))

#generate lagged L values
dat.long=dat.long %>%
  group_by(id) %>%
  mutate(Llag1 = lag(L,n=1),Llag2 = lag(L,n=2),Llag3 = lag(L,n=3),Llag4 = lag(L,n=4)) %>%
  mutate(Llag1=replace_na(Llag1,0),Llag2=replace_na(Llag2,0),Llag3=replace_na(Llag3,0),Llag4=replace_na(Llag4,0))

#baseline L
dat.long=dat.long %>%
  group_by(id) %>%
  mutate(L.baseline = first(L))

