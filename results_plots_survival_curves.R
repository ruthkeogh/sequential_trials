###########################################################################################
###########################################################################################
# - Creates Figure 4: estimates of survival curves under the treatment strategies "always treated" and "never treated" using the two methods. 
# - The plot is saved in the folder "results" as figure4.pdf
# - This file also shows how to create similar plots, e.g. for the analyses using truncated weights, which are not shown in the paper. 
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------------------------------------------
#some set-up for the plots
#------------------------------------------------------------

#function used to add a line to a plot
addlinetoplot.lty1 <- function(dataset, varx, vary,vcol,vline) { 
  list(geom_line(data=dataset, aes_string(x=varx, y=vary,group=1,colour=vcol,linetype=vline),size=1))
}

#colours
cols=c("true0"="#000000","true1"="#000000","est0"="#D55E00","est1"="#009E73")

#line types
lines=c("true0"="dashed","true1"="solid","est0"="dashed","est1"="solid")

#------------------------------------------------------------
#MSM-IPTW SCENARIO 1
#------------------------------------------------------------

#---
#msm_sw_scen1

plot_msm_sw_scen1 <- ggplot(data = lines_surv1_msm_sw_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_sw_scen1=plot_msm_sw_scen1+
  geom_line(aes(y=lines_surv1_msm_sw_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_sw_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_sw_scen1=plot_msm_sw_scen1+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_sw_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_sw_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swL_scen1

plot_msm_swL_scen1 <- ggplot(data = lines_surv1_msm_swL_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swL_scen1=plot_msm_swL_scen1+
  geom_line(aes(y=lines_surv1_msm_swL_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swL_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swL_scen1=plot_msm_swL_scen1+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swL_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swL_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swtrunc_scen1

plot_msm_swtrunc_scen1 <- ggplot(data = lines_surv1_msm_swtrunc_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swtrunc_scen1=plot_msm_swtrunc_scen1+
  geom_line(aes(y=lines_surv1_msm_swtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swtrunc_scen1=plot_msm_swtrunc_scen1+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swtrunc_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swtrunc_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swLtrunc_scen1

plot_msm_swLtrunc_scen1 <- ggplot(data = lines_surv1_msm_swLtrunc_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swLtrunc_scen1=plot_msm_swLtrunc_scen1+
  geom_line(aes(y=lines_surv1_msm_swLtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swLtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swLtrunc_scen1=plot_msm_swLtrunc_scen1+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swLtrunc_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swLtrunc_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 1")


#------------------------------------------------------------
#MSM-IPTW SCENARIO 2
#------------------------------------------------------------

#---
#msm_sw_scen2

plot_msm_sw_scen2 <- ggplot(data = lines_surv1_msm_sw_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_sw_scen2=plot_msm_sw_scen2+
  geom_line(aes(y=lines_surv1_msm_sw_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_sw_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_sw_scen2=plot_msm_sw_scen2+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_sw_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_sw_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swL_scen2

plot_msm_swL_scen2 <- ggplot(data = lines_surv1_msm_swL_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swL_scen2=plot_msm_swL_scen2+
  geom_line(aes(y=lines_surv1_msm_swL_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swL_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swL_scen2=plot_msm_swL_scen2+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swL_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swL_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swtrunc_scen2

plot_msm_swtrunc_scen2 <- ggplot(data = lines_surv1_msm_swtrunc_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swtrunc_scen2=plot_msm_swtrunc_scen2+
  geom_line(aes(y=lines_surv1_msm_swtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swtrunc_scen2=plot_msm_swtrunc_scen2+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swtrunc_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swtrunc_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swLtrunc_scen2

plot_msm_swLtrunc_scen2 <- ggplot(data = lines_surv1_msm_swLtrunc_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swLtrunc_scen2=plot_msm_swLtrunc_scen2+
  geom_line(aes(y=lines_surv1_msm_swLtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swLtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swLtrunc_scen2=plot_msm_swLtrunc_scen2+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swLtrunc_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swLtrunc_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 2")

#------------------------------------------------------------
#MSM-IPTW SCENARIO 3
#------------------------------------------------------------

#---
#msm_w_scen3

plot_msm_sw_scen3 <- ggplot(data = lines_surv1_msm_sw_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_sw_scen3=plot_msm_sw_scen3+
  geom_line(aes(y=lines_surv1_msm_sw_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_sw_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_sw_scen3=plot_msm_sw_scen3+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_sw_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_sw_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swL_scen3

plot_msm_swL_scen3 <- ggplot(data = lines_surv1_msm_swL_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swL_scen3=plot_msm_swL_scen3+
  geom_line(aes(y=lines_surv1_msm_swL_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swL_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swL_scen3=plot_msm_swL_scen3+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swL_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swL_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swtrunc_scen3

plot_msm_swtrunc_scen3 <- ggplot(data = lines_surv1_msm_swtrunc_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swtrunc_scen3=plot_msm_swtrunc_scen3+
  geom_line(aes(y=lines_surv1_msm_swtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swtrunc_scen3=plot_msm_swtrunc_scen3+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swtrunc_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swtrunc_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swLtrunc_scen3

plot_msm_swLtrunc_scen3 <- ggplot(data = lines_surv1_msm_swLtrunc_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_msm_swLtrunc_scen3=plot_msm_swLtrunc_scen3+
  geom_line(aes(y=lines_surv1_msm_swLtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_msm_swLtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_msm_swLtrunc_scen3=plot_msm_swLtrunc_scen3+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.msm,"time","surv0_msm_swLtrunc_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.msm,"time","surv1_msm_swLtrunc_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("MSM-IPTW: scenario 3")


#------------------------------------------------------------
#Sequential trials SCENARIO 1
#------------------------------------------------------------

#---
#seq_sw_scen1

plot_seq_sw_scen1 <- ggplot(data = lines_surv1_seq_sw_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_sw_scen1=plot_seq_sw_scen1+
  geom_line(aes(y=lines_surv1_seq_sw_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_sw_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_sw_scen1=plot_seq_sw_scen1+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_sw_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_sw_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 1")

#---
#seq_swtrunc_scen1

plot_seq_swtrunc_scen1 <- ggplot(data = lines_surv1_seq_swtrunc_scen1, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_swtrunc_scen1=plot_seq_swtrunc_scen1+
  geom_line(aes(y=lines_surv1_seq_swtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_swtrunc_scen1$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_swtrunc_scen1=plot_seq_swtrunc_scen1+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_swtrunc_scen1",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_swtrunc_scen1",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 1")

#------------------------------------------------------------
#Sequential trials SCENARIO 2
#------------------------------------------------------------

#---
#seq_sw_scen2

plot_seq_sw_scen2 <- ggplot(data = lines_surv1_seq_sw_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_sw_scen2=plot_seq_sw_scen2+
  geom_line(aes(y=lines_surv1_seq_sw_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_sw_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_sw_scen2=plot_seq_sw_scen2+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_sw_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_sw_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 2")

#---
#seq_swtrunc_scen2

plot_seq_swtrunc_scen2 <- ggplot(data = lines_surv1_seq_swtrunc_scen2, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_swtrunc_scen2=plot_seq_swtrunc_scen2+
  geom_line(aes(y=lines_surv1_seq_swtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_swtrunc_scen2$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_swtrunc_scen2=plot_seq_swtrunc_scen2+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_swtrunc_scen2",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_swtrunc_scen2",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 2")

#------------------------------------------------------------
#Sequential trials SCENARIO 3
#------------------------------------------------------------

#---
#seq_sw_scen3

plot_seq_sw_scen3 <- ggplot(data = lines_surv1_seq_sw_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_sw_scen3=plot_seq_sw_scen3+
  geom_line(aes(y=lines_surv1_seq_sw_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_sw_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_sw_scen3=plot_seq_sw_scen3+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_sw_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_sw_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 3")


#---
#seq_swtrunc_scen3

plot_seq_swtrunc_scen3 <- ggplot(data = lines_surv1_seq_swtrunc_scen3, aes(x = time, y = surv, group = sim),alpha=0.00001)
plot_seq_swtrunc_scen3=plot_seq_swtrunc_scen3+
  geom_line(aes(y=lines_surv1_seq_swtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#009E73")+
  geom_line(aes(y=lines_surv0_seq_swtrunc_scen3$surv),alpha=0.01,size=0.0000001,colour="#D55E00")
plot_seq_swtrunc_scen3=plot_seq_swtrunc_scen3+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_true",vcol='"true0"',vline='"true0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_true",vcol='"true1"',vline='"true1"')+
  addlinetoplot.lty1(dat.surv0.seq,"time","surv0_seq_swtrunc_scen3",vcol='"est0"',vline='"est0"')+
  addlinetoplot.lty1(dat.surv1.seq,"time","surv1_seq_swtrunc_scen3",vcol='"est1"',vline='"est1"')+
  ylab("Survival probability")+xlab("Time")+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  scale_colour_manual(NULL,values=cols,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  scale_linetype_manual(NULL,values=lines,labels=c(true0="True: Never treated",true1="True: Always treated",est0="Estimated: Never treated",est1="Estimated: Always treated"),breaks=c("true0","true1","est0","est1"))+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),legend.text=element_text(size = 8))+
  theme(legend.position=c(0.4,0.2))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.background = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.key.width = unit(1, "cm"),legend.key.height = unit(0.5,"line"))+
  theme(legend.key = element_rect(fill = NA))+
  ggtitle("Sequential trials: scenario 3")

#------------------------------------------------------------
#Combine plots for scenarios 1,2,3 to create Figure 4
#Figure 4 shows results from the MSM-IPTW approach where the MSM is conditional on L0, and the sequential trials approach
#------------------------------------------------------------

figure4=arrangeGrob(plot_msm_swL_scen1,plot_seq_sw_scen1,
                    plot_msm_swL_scen2,plot_seq_sw_scen2,
                    plot_msm_swL_scen3,plot_seq_sw_scen3,ncol=2)

ggsave("results/figure4.pdf", figure4)

#------------------------------------------------------------
#Similar plots can be made for the other variations of ghe methods considered in the simulation study, but for which the plots are not shown in the paper.
#------------------------------------------------------------

#MSM-IPTW approach where the MSM is not conditional on L0, and the sequential trials approach
# arrangeGrob(plot_msm_sw_scen1,plot_seq_sw_scen1,
#              plot_msm_sw_scen2,plot_seq_sw_scen2,
#              plot_msm_sw_scen3,plot_seq_sw_scen3,ncol=2)

#MSM-IPTW approach where the MSM is conditional on L0, and the sequential trials approach: both using truncated weights
# arrangeGrob(plot_msm_swLtrunc_scen1,plot_seq_swtrunc_scen1,
#             plot_msm_swLtrunc_scen2,plot_seq_swtrunc_scen2,
#             plot_msm_swLtrunc_scen3,plot_seq_swtrunc_scen3,ncol=2)

#MSM-IPTW approach where the MSM is not conditional on L0, and the sequential trials approach: both using truncated weights
# arrangeGrob(plot_msm_swtrunc_scen1,plot_seq_swtrunc_scen1,
#              plot_msm_swtrunc_scen2,plot_seq_swtrunc_scen2,
#              plot_msm_swtrunc_scen3,plot_seq_swtrunc_scen3,ncol=2)
