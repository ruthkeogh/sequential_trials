###########################################################################################
###########################################################################################
# - Puts the results in the format required to create the plots in Figures 4 and 5. 
# - This involves obtaining the average survival curves across the 1000 simulations.
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#data sets for use in ggplot: mean survival curves across simulations
#------------------------

dat.surv1.msm=data.frame(time=t.hor,surv1_true,surv0_true,
                    surv1_msm_sw_scen1=colMeans(surv1_msm_sw_scen1),surv1_msm_swL_scen1=colMeans(surv1_msm_swL_scen1),
                    surv1_msm_swtrunc_scen1=colMeans(surv1_msm_swtrunc_scen1),surv1_msm_swLtrunc_scen1=colMeans(surv1_msm_swLtrunc_scen1),
                    surv1_msm_sw_scen2=colMeans(surv1_msm_sw_scen2),surv1_msm_swL_scen2=colMeans(surv1_msm_swL_scen2),
                    surv1_msm_swtrunc_scen2=colMeans(surv1_msm_swtrunc_scen2),surv1_msm_swLtrunc_scen2=colMeans(surv1_msm_swLtrunc_scen2),
                    surv1_msm_sw_scen3=colMeans(surv1_msm_sw_scen3),surv1_msm_swL_scen3=colMeans(surv1_msm_swL_scen3),
                    surv1_msm_swtrunc_scen3=colMeans(surv1_msm_swtrunc_scen3),surv1_msm_swLtrunc_scen3=colMeans(surv1_msm_swLtrunc_scen3))

dat.surv0.msm=data.frame(time=t.hor,surv1_true,surv0_true,
                         surv0_msm_sw_scen1=colMeans(surv0_msm_sw_scen1),surv0_msm_swL_scen1=colMeans(surv0_msm_swL_scen1),
                         surv0_msm_swtrunc_scen1=colMeans(surv0_msm_swtrunc_scen1),surv0_msm_swLtrunc_scen1=colMeans(surv0_msm_swLtrunc_scen1),
                         surv0_msm_sw_scen2=colMeans(surv0_msm_sw_scen2),surv0_msm_swL_scen2=colMeans(surv0_msm_swL_scen2),
                         surv0_msm_swtrunc_scen2=colMeans(surv0_msm_swtrunc_scen2),surv0_msm_swLtrunc_scen2=colMeans(surv0_msm_swLtrunc_scen2),
                         surv0_msm_sw_scen3=colMeans(surv0_msm_sw_scen3),surv0_msm_swL_scen3=colMeans(surv0_msm_swL_scen3),
                         surv0_msm_swtrunc_scen3=colMeans(surv0_msm_swtrunc_scen3),surv0_msm_swLtrunc_scen3=colMeans(surv0_msm_swLtrunc_scen3))

dat.surv1.seq=data.frame(time=t.hor,surv1_true,surv0_true,
                        surv1_seq_sw_scen1=colMeans(surv1_seq_sw_scen1),
                        surv1_seq_sw_scen2=colMeans(surv1_seq_sw_scen2),
                        surv1_seq_sw_scen3=colMeans(surv1_seq_sw_scen3),
                        surv1_seq_swtrunc_scen1=colMeans(surv1_seq_sw_scen1),
                        surv1_seq_swtrunc_scen2=colMeans(surv1_seq_swtrunc_scen2),
                        surv1_seq_swtrunc_scen3=colMeans(surv1_seq_swtrunc_scen3))

dat.surv0.seq=data.frame(time=t.hor,surv1_true,surv0_true,
                         surv0_seq_sw_scen1=colMeans(surv0_seq_sw_scen1),
                         surv0_seq_sw_scen2=colMeans(surv0_seq_sw_scen2),
                         surv0_seq_sw_scen3=colMeans(surv0_seq_sw_scen3),
                         surv0_seq_swtrunc_scen1=colMeans(surv0_seq_sw_scen1),
                         surv0_seq_swtrunc_scen2=colMeans(surv0_seq_swtrunc_scen2),
                         surv0_seq_swtrunc_scen3=colMeans(surv0_seq_swtrunc_scen3))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#MSM-IPTW SCENARIO 1
#------------------------

#---
#msm_sw_scen1

lines_surv1_msm_sw_scen1=as.matrix(surv1_msm_sw_scen1)
lines_surv1_msm_sw_scen1=as.vector(t(lines_surv1_msm_sw_scen1))
lines_surv1_msm_sw_scen1=data.frame(lines_surv1_msm_sw_scen1)
names(lines_surv1_msm_sw_scen1)="surv"
lines_surv1_msm_sw_scen1$time=rep(t.hor,n.sim)
lines_surv1_msm_sw_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_sw_scen1$treat=1

lines_surv0_msm_sw_scen1=as.matrix(surv0_msm_sw_scen1)
lines_surv0_msm_sw_scen1=as.vector(t(lines_surv0_msm_sw_scen1))
lines_surv0_msm_sw_scen1=data.frame(lines_surv0_msm_sw_scen1)
names(lines_surv0_msm_sw_scen1)="surv"
lines_surv0_msm_sw_scen1$time=rep(t.hor,n.sim)
lines_surv0_msm_sw_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_sw_scen1$treat=1

lines_msm_sw_scen1=data.frame(rbind(lines_surv1_msm_sw_scen1,lines_surv0_msm_sw_scen1))

#---
#msm_swL_scen1

lines_surv1_msm_swL_scen1=as.matrix(surv1_msm_swL_scen1)
lines_surv1_msm_swL_scen1=as.vector(t(lines_surv1_msm_swL_scen1))
lines_surv1_msm_swL_scen1=data.frame(lines_surv1_msm_swL_scen1)
names(lines_surv1_msm_swL_scen1)="surv"
lines_surv1_msm_swL_scen1$time=rep(t.hor,n.sim)
lines_surv1_msm_swL_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swL_scen1$treat=1

lines_surv0_msm_swL_scen1=as.matrix(surv0_msm_swL_scen1)
lines_surv0_msm_swL_scen1=as.vector(t(lines_surv0_msm_swL_scen1))
lines_surv0_msm_swL_scen1=data.frame(lines_surv0_msm_swL_scen1)
names(lines_surv0_msm_swL_scen1)="surv"
lines_surv0_msm_swL_scen1$time=rep(t.hor,n.sim)
lines_surv0_msm_swL_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swL_scen1$treat=1

lines_msm_swL_scen1=data.frame(rbind(lines_surv1_msm_swL_scen1,lines_surv0_msm_swL_scen1))

#---
#msm_wtrunc_scen1

lines_surv1_msm_swtrunc_scen1=as.matrix(surv1_msm_swtrunc_scen1)
lines_surv1_msm_swtrunc_scen1=as.vector(t(lines_surv1_msm_swtrunc_scen1))
lines_surv1_msm_swtrunc_scen1=data.frame(lines_surv1_msm_swtrunc_scen1)
names(lines_surv1_msm_swtrunc_scen1)="surv"
lines_surv1_msm_swtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv1_msm_swtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swtrunc_scen1$treat=1

lines_surv0_msm_swtrunc_scen1=as.matrix(surv0_msm_swtrunc_scen1)
lines_surv0_msm_swtrunc_scen1=as.vector(t(lines_surv0_msm_swtrunc_scen1))
lines_surv0_msm_swtrunc_scen1=data.frame(lines_surv0_msm_swtrunc_scen1)
names(lines_surv0_msm_swtrunc_scen1)="surv"
lines_surv0_msm_swtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv0_msm_swtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swtrunc_scen1$treat=1

lines_msm_swtrunc_scen1=data.frame(rbind(lines_surv1_msm_swtrunc_scen1,lines_surv0_msm_swtrunc_scen1))

#---
#msm_swLtrunc_scen1

lines_surv1_msm_swLtrunc_scen1=as.matrix(surv1_msm_swLtrunc_scen1)
lines_surv1_msm_swLtrunc_scen1=as.vector(t(lines_surv1_msm_swLtrunc_scen1))
lines_surv1_msm_swLtrunc_scen1=data.frame(lines_surv1_msm_swLtrunc_scen1)
names(lines_surv1_msm_swLtrunc_scen1)="surv"
lines_surv1_msm_swLtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv1_msm_swLtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swLtrunc_scen1$treat=1

lines_surv0_msm_swLtrunc_scen1=as.matrix(surv0_msm_swLtrunc_scen1)
lines_surv0_msm_swLtrunc_scen1=as.vector(t(lines_surv0_msm_swLtrunc_scen1))
lines_surv0_msm_swLtrunc_scen1=data.frame(lines_surv0_msm_swLtrunc_scen1)
names(lines_surv0_msm_swLtrunc_scen1)="surv"
lines_surv0_msm_swLtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv0_msm_swLtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swLtrunc_scen1$treat=1

lines_msm_swLtrunc_scen1=data.frame(rbind(lines_surv1_msm_swLtrunc_scen1,lines_surv0_msm_swLtrunc_scen1))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#MSM-IPTW SCENARIO 2
#------------------------

#---
#msm_sw_scen2

lines_surv1_msm_sw_scen2=as.matrix(surv1_msm_sw_scen2)
lines_surv1_msm_sw_scen2=as.vector(t(lines_surv1_msm_sw_scen2))
lines_surv1_msm_sw_scen2=data.frame(lines_surv1_msm_sw_scen2)
names(lines_surv1_msm_sw_scen2)="surv"
lines_surv1_msm_sw_scen2$time=rep(t.hor,n.sim)
lines_surv1_msm_sw_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_sw_scen2$treat=1

lines_surv0_msm_sw_scen2=as.matrix(surv0_msm_sw_scen2)
lines_surv0_msm_sw_scen2=as.vector(t(lines_surv0_msm_sw_scen2))
lines_surv0_msm_sw_scen2=data.frame(lines_surv0_msm_sw_scen2)
names(lines_surv0_msm_sw_scen2)="surv"
lines_surv0_msm_sw_scen2$time=rep(t.hor,n.sim)
lines_surv0_msm_sw_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_sw_scen2$treat=1

lines_msm_sw_scen2=data.frame(rbind(lines_surv1_msm_sw_scen2,lines_surv0_msm_sw_scen2))

#---
#msm_swL_scen2

lines_surv1_msm_swL_scen2=as.matrix(surv1_msm_swL_scen2)
lines_surv1_msm_swL_scen2=as.vector(t(lines_surv1_msm_swL_scen2))
lines_surv1_msm_swL_scen2=data.frame(lines_surv1_msm_swL_scen2)
names(lines_surv1_msm_swL_scen2)="surv"
lines_surv1_msm_swL_scen2$time=rep(t.hor,n.sim)
lines_surv1_msm_swL_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swL_scen2$treat=1

lines_surv0_msm_swL_scen2=as.matrix(surv0_msm_swL_scen2)
lines_surv0_msm_swL_scen2=as.vector(t(lines_surv0_msm_swL_scen2))
lines_surv0_msm_swL_scen2=data.frame(lines_surv0_msm_swL_scen2)
names(lines_surv0_msm_swL_scen2)="surv"
lines_surv0_msm_swL_scen2$time=rep(t.hor,n.sim)
lines_surv0_msm_swL_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swL_scen2$treat=1

lines_msm_swL_scen2=data.frame(rbind(lines_surv1_msm_swL_scen2,lines_surv0_msm_swL_scen2))

#---
#msm_wtrunc_scen2

lines_surv1_msm_swtrunc_scen2=as.matrix(surv1_msm_swtrunc_scen2)
lines_surv1_msm_swtrunc_scen2=as.vector(t(lines_surv1_msm_swtrunc_scen2))
lines_surv1_msm_swtrunc_scen2=data.frame(lines_surv1_msm_swtrunc_scen2)
names(lines_surv1_msm_swtrunc_scen2)="surv"
lines_surv1_msm_swtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv1_msm_swtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swtrunc_scen2$treat=1

lines_surv0_msm_swtrunc_scen2=as.matrix(surv0_msm_swtrunc_scen2)
lines_surv0_msm_swtrunc_scen2=as.vector(t(lines_surv0_msm_swtrunc_scen2))
lines_surv0_msm_swtrunc_scen2=data.frame(lines_surv0_msm_swtrunc_scen2)
names(lines_surv0_msm_swtrunc_scen2)="surv"
lines_surv0_msm_swtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv0_msm_swtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swtrunc_scen2$treat=1

lines_msm_swtrunc_scen2=data.frame(rbind(lines_surv1_msm_swtrunc_scen2,lines_surv0_msm_swtrunc_scen2))

#---
#msm_swLtrunc_scen2

lines_surv1_msm_swLtrunc_scen2=as.matrix(surv1_msm_swLtrunc_scen2)
lines_surv1_msm_swLtrunc_scen2=as.vector(t(lines_surv1_msm_swLtrunc_scen2))
lines_surv1_msm_swLtrunc_scen2=data.frame(lines_surv1_msm_swLtrunc_scen2)
names(lines_surv1_msm_swLtrunc_scen2)="surv"
lines_surv1_msm_swLtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv1_msm_swLtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swLtrunc_scen2$treat=1

lines_surv0_msm_swLtrunc_scen2=as.matrix(surv0_msm_swLtrunc_scen2)
lines_surv0_msm_swLtrunc_scen2=as.vector(t(lines_surv0_msm_swLtrunc_scen2))
lines_surv0_msm_swLtrunc_scen2=data.frame(lines_surv0_msm_swLtrunc_scen2)
names(lines_surv0_msm_swLtrunc_scen2)="surv"
lines_surv0_msm_swLtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv0_msm_swLtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swLtrunc_scen2$treat=1

lines_msm_swLtrunc_scen2=data.frame(rbind(lines_surv1_msm_swLtrunc_scen2,lines_surv0_msm_swLtrunc_scen2))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#MSM-IPTW SCENARIO 3
#------------------------

#---
#msm_sw_scen3

lines_surv1_msm_sw_scen3=as.matrix(surv1_msm_sw_scen3)
lines_surv1_msm_sw_scen3=as.vector(t(lines_surv1_msm_sw_scen3))
lines_surv1_msm_sw_scen3=data.frame(lines_surv1_msm_sw_scen3)
names(lines_surv1_msm_sw_scen3)="surv"
lines_surv1_msm_sw_scen3$time=rep(t.hor,n.sim)
lines_surv1_msm_sw_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_sw_scen3$treat=1

lines_surv0_msm_sw_scen3=as.matrix(surv0_msm_sw_scen3)
lines_surv0_msm_sw_scen3=as.vector(t(lines_surv0_msm_sw_scen3))
lines_surv0_msm_sw_scen3=data.frame(lines_surv0_msm_sw_scen3)
names(lines_surv0_msm_sw_scen3)="surv"
lines_surv0_msm_sw_scen3$time=rep(t.hor,n.sim)
lines_surv0_msm_sw_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_sw_scen3$treat=1

lines_msm_sw_scen3=data.frame(rbind(lines_surv1_msm_sw_scen3,lines_surv0_msm_sw_scen3))

#---
#msm_swL_scen3

lines_surv1_msm_swL_scen3=as.matrix(surv1_msm_swL_scen3)
lines_surv1_msm_swL_scen3=as.vector(t(lines_surv1_msm_swL_scen3))
lines_surv1_msm_swL_scen3=data.frame(lines_surv1_msm_swL_scen3)
names(lines_surv1_msm_swL_scen3)="surv"
lines_surv1_msm_swL_scen3$time=rep(t.hor,n.sim)
lines_surv1_msm_swL_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swL_scen3$treat=1

lines_surv0_msm_swL_scen3=as.matrix(surv0_msm_swL_scen3)
lines_surv0_msm_swL_scen3=as.vector(t(lines_surv0_msm_swL_scen3))
lines_surv0_msm_swL_scen3=data.frame(lines_surv0_msm_swL_scen3)
names(lines_surv0_msm_swL_scen3)="surv"
lines_surv0_msm_swL_scen3$time=rep(t.hor,n.sim)
lines_surv0_msm_swL_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swL_scen3$treat=1

lines_msm_swL_scen3=data.frame(rbind(lines_surv1_msm_swL_scen3,lines_surv0_msm_swL_scen3))

#---
#msm_wtrunc_scen3

lines_surv1_msm_swtrunc_scen3=as.matrix(surv1_msm_swtrunc_scen3)
lines_surv1_msm_swtrunc_scen3=as.vector(t(lines_surv1_msm_swtrunc_scen3))
lines_surv1_msm_swtrunc_scen3=data.frame(lines_surv1_msm_swtrunc_scen3)
names(lines_surv1_msm_swtrunc_scen3)="surv"
lines_surv1_msm_swtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv1_msm_swtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swtrunc_scen3$treat=1

lines_surv0_msm_swtrunc_scen3=as.matrix(surv0_msm_swtrunc_scen3)
lines_surv0_msm_swtrunc_scen3=as.vector(t(lines_surv0_msm_swtrunc_scen3))
lines_surv0_msm_swtrunc_scen3=data.frame(lines_surv0_msm_swtrunc_scen3)
names(lines_surv0_msm_swtrunc_scen3)="surv"
lines_surv0_msm_swtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv0_msm_swtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swtrunc_scen3$treat=1

lines_msm_swtrunc_scen3=data.frame(rbind(lines_surv1_msm_swtrunc_scen3,lines_surv0_msm_swtrunc_scen3))

#---
#msm_swLtrunc_scen3

lines_surv1_msm_swLtrunc_scen3=as.matrix(surv1_msm_swLtrunc_scen3)
lines_surv1_msm_swLtrunc_scen3=as.vector(t(lines_surv1_msm_swLtrunc_scen3))
lines_surv1_msm_swLtrunc_scen3=data.frame(lines_surv1_msm_swLtrunc_scen3)
names(lines_surv1_msm_swLtrunc_scen3)="surv"
lines_surv1_msm_swLtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv1_msm_swLtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_msm_swLtrunc_scen3$treat=1

lines_surv0_msm_swLtrunc_scen3=as.matrix(surv0_msm_swLtrunc_scen3)
lines_surv0_msm_swLtrunc_scen3=as.vector(t(lines_surv0_msm_swLtrunc_scen3))
lines_surv0_msm_swLtrunc_scen3=data.frame(lines_surv0_msm_swLtrunc_scen3)
names(lines_surv0_msm_swLtrunc_scen3)="surv"
lines_surv0_msm_swLtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv0_msm_swLtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_msm_swLtrunc_scen3$treat=1

lines_msm_swLtrunc_scen3=data.frame(rbind(lines_surv1_msm_swLtrunc_scen3,lines_surv0_msm_swLtrunc_scen3))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#Sequential trials SCENARIO 1
#------------------------

#---
#seq_sw_scen1

lines_surv1_seq_sw_scen1=as.matrix(surv1_seq_sw_scen1)
lines_surv1_seq_sw_scen1=as.vector(t(lines_surv1_seq_sw_scen1))
lines_surv1_seq_sw_scen1=data.frame(lines_surv1_seq_sw_scen1)
names(lines_surv1_seq_sw_scen1)="surv"
lines_surv1_seq_sw_scen1$time=rep(t.hor,n.sim)
lines_surv1_seq_sw_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_sw_scen1$treat=1

lines_surv0_seq_sw_scen1=as.matrix(surv0_seq_sw_scen1)
lines_surv0_seq_sw_scen1=as.vector(t(lines_surv0_seq_sw_scen1))
lines_surv0_seq_sw_scen1=data.frame(lines_surv0_seq_sw_scen1)
names(lines_surv0_seq_sw_scen1)="surv"
lines_surv0_seq_sw_scen1$time=rep(t.hor,n.sim)
lines_surv0_seq_sw_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_sw_scen1$treat=1


lines_seq_sw_scen1=data.frame(rbind(lines_surv1_seq_sw_scen1,lines_surv0_seq_sw_scen1))

#---
#seq_swtrunc_scen1

lines_surv1_seq_swtrunc_scen1=as.matrix(surv1_seq_swtrunc_scen1)
lines_surv1_seq_swtrunc_scen1=as.vector(t(lines_surv1_seq_swtrunc_scen1))
lines_surv1_seq_swtrunc_scen1=data.frame(lines_surv1_seq_swtrunc_scen1)
names(lines_surv1_seq_swtrunc_scen1)="surv"
lines_surv1_seq_swtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv1_seq_swtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_swtrunc_scen1$treat=1

lines_surv0_seq_swtrunc_scen1=as.matrix(surv0_seq_swtrunc_scen1)
lines_surv0_seq_swtrunc_scen1=as.vector(t(lines_surv0_seq_swtrunc_scen1))
lines_surv0_seq_swtrunc_scen1=data.frame(lines_surv0_seq_swtrunc_scen1)
names(lines_surv0_seq_swtrunc_scen1)="surv"
lines_surv0_seq_swtrunc_scen1$time=rep(t.hor,n.sim)
lines_surv0_seq_swtrunc_scen1$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_swtrunc_scen1$treat=1


lines_seq_swtrunc_scen1=data.frame(rbind(lines_surv1_seq_swtrunc_scen1,lines_surv0_seq_swtrunc_scen1))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#Sequential trials SCENARIO 2
#------------------------

#---
#seq_sw_scen2

lines_surv1_seq_sw_scen2=as.matrix(surv1_seq_sw_scen2)
lines_surv1_seq_sw_scen2=as.vector(t(lines_surv1_seq_sw_scen2))
lines_surv1_seq_sw_scen2=data.frame(lines_surv1_seq_sw_scen2)
names(lines_surv1_seq_sw_scen2)="surv"
lines_surv1_seq_sw_scen2$time=rep(t.hor,n.sim)
lines_surv1_seq_sw_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_sw_scen2$treat=1

lines_surv0_seq_sw_scen2=as.matrix(surv0_seq_sw_scen2)
lines_surv0_seq_sw_scen2=as.vector(t(lines_surv0_seq_sw_scen2))
lines_surv0_seq_sw_scen2=data.frame(lines_surv0_seq_sw_scen2)
names(lines_surv0_seq_sw_scen2)="surv"
lines_surv0_seq_sw_scen2$time=rep(t.hor,n.sim)
lines_surv0_seq_sw_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_sw_scen2$treat=1


lines_seq_sw_scen2=data.frame(rbind(lines_surv1_seq_sw_scen2,lines_surv0_seq_sw_scen2))

#---
#seq_swtrunc_scen2

lines_surv1_seq_swtrunc_scen2=as.matrix(surv1_seq_swtrunc_scen2)
lines_surv1_seq_swtrunc_scen2=as.vector(t(lines_surv1_seq_swtrunc_scen2))
lines_surv1_seq_swtrunc_scen2=data.frame(lines_surv1_seq_swtrunc_scen2)
names(lines_surv1_seq_swtrunc_scen2)="surv"
lines_surv1_seq_swtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv1_seq_swtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_swtrunc_scen2$treat=1

lines_surv0_seq_swtrunc_scen2=as.matrix(surv0_seq_swtrunc_scen2)
lines_surv0_seq_swtrunc_scen2=as.vector(t(lines_surv0_seq_swtrunc_scen2))
lines_surv0_seq_swtrunc_scen2=data.frame(lines_surv0_seq_swtrunc_scen2)
names(lines_surv0_seq_swtrunc_scen2)="surv"
lines_surv0_seq_swtrunc_scen2$time=rep(t.hor,n.sim)
lines_surv0_seq_swtrunc_scen2$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_swtrunc_scen2$treat=1


lines_seq_swtrunc_scen2=data.frame(rbind(lines_surv1_seq_swtrunc_scen2,lines_surv0_seq_swtrunc_scen2))

#------------------------
#data sets for use in ggplot: curves from each simulated data set
#Sequential trials SCENARIO 3
#------------------------

#---
#seq_sw_scen3

lines_surv1_seq_sw_scen3=as.matrix(surv1_seq_sw_scen3)
lines_surv1_seq_sw_scen3=as.vector(t(lines_surv1_seq_sw_scen3))
lines_surv1_seq_sw_scen3=data.frame(lines_surv1_seq_sw_scen3)
names(lines_surv1_seq_sw_scen3)="surv"
lines_surv1_seq_sw_scen3$time=rep(t.hor,n.sim)
lines_surv1_seq_sw_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_sw_scen3$treat=1

lines_surv0_seq_sw_scen3=as.matrix(surv0_seq_sw_scen3)
lines_surv0_seq_sw_scen3=as.vector(t(lines_surv0_seq_sw_scen3))
lines_surv0_seq_sw_scen3=data.frame(lines_surv0_seq_sw_scen3)
names(lines_surv0_seq_sw_scen3)="surv"
lines_surv0_seq_sw_scen3$time=rep(t.hor,n.sim)
lines_surv0_seq_sw_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_sw_scen3$treat=1

lines_seq_sw_scen3=data.frame(rbind(lines_surv1_seq_sw_scen3,lines_surv0_seq_sw_scen3))

#---
#seq_swtrunc_scen3

lines_surv1_seq_swtrunc_scen3=as.matrix(surv1_seq_swtrunc_scen3)
lines_surv1_seq_swtrunc_scen3=as.vector(t(lines_surv1_seq_swtrunc_scen3))
lines_surv1_seq_swtrunc_scen3=data.frame(lines_surv1_seq_swtrunc_scen3)
names(lines_surv1_seq_swtrunc_scen3)="surv"
lines_surv1_seq_swtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv1_seq_swtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv1_seq_swtrunc_scen3$treat=1

lines_surv0_seq_swtrunc_scen3=as.matrix(surv0_seq_swtrunc_scen3)
lines_surv0_seq_swtrunc_scen3=as.vector(t(lines_surv0_seq_swtrunc_scen3))
lines_surv0_seq_swtrunc_scen3=data.frame(lines_surv0_seq_swtrunc_scen3)
names(lines_surv0_seq_swtrunc_scen3)="surv"
lines_surv0_seq_swtrunc_scen3$time=rep(t.hor,n.sim)
lines_surv0_seq_swtrunc_scen3$sim=rep(1:n.sim,each=length(t.hor))
lines_surv0_seq_swtrunc_scen3$treat=1

lines_seq_swtrunc_scen3=data.frame(rbind(lines_surv1_seq_swtrunc_scen3,lines_surv0_seq_swtrunc_scen3))
