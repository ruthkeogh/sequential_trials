###########################################################################################
###########################################################################################
# - Creates Figure 5: estimates of bias in risk differences under the treatment strategies "always treated" and "never treated" using the two methods.  
# - Creates Supplementary Figure 1 and Supplementary Figure 3, which show the estimates of bias obtained using different weights. 
# - The plots are saved in the folder "results" as figure5.pdf, suppfigure1.pdf, suppfigure3.pdf.
#
# Created by Ruth Keogh
###########################################################################################
###########################################################################################

#------------------------
#Calculate bias in risk differences and corresponding lower and upper limits of 95% Monte-Carlo confidence intervals.
#------------------------

#MSM-IPTW: scenario 1

bias_survdiff_msm_sw_scen1=colMeans(survdiff_msm_sw_scen1)-survdiff_true
bias_survdiff_msm_swL_scen1=colMeans(survdiff_msm_swL_scen1)-survdiff_true
bias_survdiff_msm_swtrunc_scen1=colMeans(survdiff_msm_swtrunc_scen1)-survdiff_true
bias_survdiff_msm_swLtrunc_scen1=colMeans(survdiff_msm_swLtrunc_scen1)-survdiff_true

sd_survdiff_msm_sw_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_sw_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swL_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swtrunc_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swLtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swLtrunc_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_msm_sw_scen1=bias_survdiff_msm_sw_scen1-1.96*sd_survdiff_msm_sw_scen1
lower_survdiff_msm_swL_scen1=bias_survdiff_msm_swL_scen1-1.96*sd_survdiff_msm_swL_scen1
lower_survdiff_msm_swtrunc_scen1=bias_survdiff_msm_swtrunc_scen1-1.96*sd_survdiff_msm_swtrunc_scen1
lower_survdiff_msm_swLtrunc_scen1=bias_survdiff_msm_swLtrunc_scen1-1.96*sd_survdiff_msm_swLtrunc_scen1

upper_survdiff_msm_sw_scen1=bias_survdiff_msm_sw_scen1+1.96*sd_survdiff_msm_sw_scen1
upper_survdiff_msm_swL_scen1=bias_survdiff_msm_swL_scen1+1.96*sd_survdiff_msm_swL_scen1
upper_survdiff_msm_swtrunc_scen1=bias_survdiff_msm_swtrunc_scen1+1.96*sd_survdiff_msm_swtrunc_scen1
upper_survdiff_msm_swLtrunc_scen1=bias_survdiff_msm_swLtrunc_scen1+1.96*sd_survdiff_msm_swLtrunc_scen1

#MSM-IPTW: scenario 2

bias_survdiff_msm_sw_scen2=colMeans(survdiff_msm_sw_scen2)-survdiff_true
bias_survdiff_msm_swL_scen2=colMeans(survdiff_msm_swL_scen2)-survdiff_true
bias_survdiff_msm_swtrunc_scen2=colMeans(survdiff_msm_swtrunc_scen2)-survdiff_true
bias_survdiff_msm_swLtrunc_scen2=colMeans(survdiff_msm_swLtrunc_scen2)-survdiff_true

sd_survdiff_msm_sw_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_sw_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swL_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swtrunc_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swLtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swLtrunc_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_msm_sw_scen2=bias_survdiff_msm_sw_scen2-1.96*sd_survdiff_msm_sw_scen2
lower_survdiff_msm_swL_scen2=bias_survdiff_msm_swL_scen2-1.96*sd_survdiff_msm_swL_scen2
lower_survdiff_msm_swtrunc_scen2=bias_survdiff_msm_swtrunc_scen2-1.96*sd_survdiff_msm_swtrunc_scen2
lower_survdiff_msm_swLtrunc_scen2=bias_survdiff_msm_swLtrunc_scen2-1.96*sd_survdiff_msm_swLtrunc_scen2

upper_survdiff_msm_sw_scen2=bias_survdiff_msm_sw_scen2+1.96*sd_survdiff_msm_sw_scen2
upper_survdiff_msm_swL_scen2=bias_survdiff_msm_swL_scen2+1.96*sd_survdiff_msm_swL_scen2
upper_survdiff_msm_swtrunc_scen2=bias_survdiff_msm_swtrunc_scen2+1.96*sd_survdiff_msm_swtrunc_scen2
upper_survdiff_msm_swLtrunc_scen2=bias_survdiff_msm_swLtrunc_scen2+1.96*sd_survdiff_msm_swLtrunc_scen2

#MSM-IPTW: scenario 3

bias_survdiff_msm_sw_scen3=colMeans(survdiff_msm_sw_scen3)-survdiff_true
bias_survdiff_msm_swL_scen3=colMeans(survdiff_msm_swL_scen3)-survdiff_true
bias_survdiff_msm_swtrunc_scen3=colMeans(survdiff_msm_swtrunc_scen3)-survdiff_true
bias_survdiff_msm_swLtrunc_scen3=colMeans(survdiff_msm_swLtrunc_scen3)-survdiff_true

sd_survdiff_msm_sw_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_sw_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swL_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swL_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swtrunc_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_msm_swLtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_msm_swLtrunc_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_msm_sw_scen3=bias_survdiff_msm_sw_scen3-1.96*sd_survdiff_msm_sw_scen3
lower_survdiff_msm_swL_scen3=bias_survdiff_msm_swL_scen3-1.96*sd_survdiff_msm_swL_scen3
lower_survdiff_msm_swtrunc_scen3=bias_survdiff_msm_swtrunc_scen3-1.96*sd_survdiff_msm_swtrunc_scen3
lower_survdiff_msm_swLtrunc_scen3=bias_survdiff_msm_swLtrunc_scen3-1.96*sd_survdiff_msm_swLtrunc_scen3

upper_survdiff_msm_sw_scen3=bias_survdiff_msm_sw_scen3+1.96*sd_survdiff_msm_sw_scen3
upper_survdiff_msm_swL_scen3=bias_survdiff_msm_swL_scen3+1.96*sd_survdiff_msm_swL_scen3
upper_survdiff_msm_swtrunc_scen3=bias_survdiff_msm_swtrunc_scen3+1.96*sd_survdiff_msm_swtrunc_scen3
upper_survdiff_msm_swLtrunc_scen3=bias_survdiff_msm_swLtrunc_scen3+1.96*sd_survdiff_msm_swLtrunc_scen3

#Sequential trials: scenario 1

bias_survdiff_seq_sw_scen1=colMeans(survdiff_seq_sw_scen1)-survdiff_true
bias_survdiff_seq_swtrunc_scen1=colMeans(survdiff_seq_swtrunc_scen1)-survdiff_true

sd_survdiff_seq_sw_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_seq_swtrunc_scen1=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_swtrunc_scen1[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_seq_sw_scen1=bias_survdiff_seq_sw_scen1-1.96*sd_survdiff_seq_sw_scen1
lower_survdiff_seq_swtrunc_scen1=bias_survdiff_seq_swtrunc_scen1-1.96*sd_survdiff_seq_swtrunc_scen1

upper_survdiff_seq_sw_scen1=bias_survdiff_seq_sw_scen1+1.96*sd_survdiff_seq_sw_scen1
upper_survdiff_seq_swtrunc_scen1=bias_survdiff_seq_swtrunc_scen1+1.96*sd_survdiff_seq_swtrunc_scen1

#Sequential trials: scenario 2

bias_survdiff_seq_sw_scen2=colMeans(survdiff_seq_sw_scen2)-survdiff_true
bias_survdiff_seq_swtrunc_scen2=colMeans(survdiff_seq_swtrunc_scen2)-survdiff_true

sd_survdiff_seq_sw_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_seq_swtrunc_scen2=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_swtrunc_scen2[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_seq_sw_scen2=bias_survdiff_seq_sw_scen2-1.96*sd_survdiff_seq_sw_scen2
lower_survdiff_seq_swtrunc_scen2=bias_survdiff_seq_swtrunc_scen2-1.96*sd_survdiff_seq_swtrunc_scen2

upper_survdiff_seq_sw_scen2=bias_survdiff_seq_sw_scen2+1.96*sd_survdiff_seq_sw_scen2
upper_survdiff_seq_swtrunc_scen2=bias_survdiff_seq_swtrunc_scen2+1.96*sd_survdiff_seq_swtrunc_scen2

#Sequential trials: scenario 3

bias_survdiff_seq_sw_scen3=colMeans(survdiff_seq_sw_scen3)-survdiff_true
bias_survdiff_seq_swtrunc_scen3=colMeans(survdiff_seq_swtrunc_scen3)-survdiff_true

sd_survdiff_seq_sw_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_sw_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})
sd_survdiff_seq_swtrunc_scen3=sapply(1:length(t.hor),FUN=function(x){sd(survdiff_seq_swtrunc_scen3[,x]-survdiff_true[x])/sqrt(n.sim)})

lower_survdiff_seq_sw_scen3=bias_survdiff_seq_sw_scen3-1.96*sd_survdiff_seq_sw_scen3
lower_survdiff_seq_swtrunc_scen3=bias_survdiff_seq_swtrunc_scen3-1.96*sd_survdiff_seq_swtrunc_scen3

upper_survdiff_seq_sw_scen3=bias_survdiff_seq_sw_scen3+1.96*sd_survdiff_seq_sw_scen3
upper_survdiff_seq_swtrunc_scen3=bias_survdiff_seq_swtrunc_scen3+1.96*sd_survdiff_seq_swtrunc_scen3

#------------------------
#data set containing the above results, for use in ggplot
#------------------------

dat.bias=data.frame(t.hor,
                    bias_survdiff_msm_sw_scen1,bias_survdiff_msm_swL_scen1,bias_survdiff_msm_swtrunc_scen1,bias_survdiff_msm_swLtrunc_scen1,
                    bias_survdiff_msm_sw_scen2,bias_survdiff_msm_swL_scen2,bias_survdiff_msm_swtrunc_scen2,bias_survdiff_msm_swLtrunc_scen2,
                    bias_survdiff_msm_sw_scen3,bias_survdiff_msm_swL_scen3,bias_survdiff_msm_swtrunc_scen3,bias_survdiff_msm_swLtrunc_scen3,
                    lower_survdiff_msm_sw_scen1,lower_survdiff_msm_swL_scen1,lower_survdiff_msm_swtrunc_scen1,lower_survdiff_msm_swLtrunc_scen1,
                    lower_survdiff_msm_sw_scen2,lower_survdiff_msm_swL_scen2,lower_survdiff_msm_swtrunc_scen2,lower_survdiff_msm_swLtrunc_scen2,
                    lower_survdiff_msm_sw_scen3,lower_survdiff_msm_swL_scen3,lower_survdiff_msm_swtrunc_scen3,lower_survdiff_msm_swLtrunc_scen3,
                    upper_survdiff_msm_sw_scen1,upper_survdiff_msm_swL_scen1,upper_survdiff_msm_swtrunc_scen1,upper_survdiff_msm_swLtrunc_scen1,
                    upper_survdiff_msm_sw_scen2,upper_survdiff_msm_swL_scen2,upper_survdiff_msm_swtrunc_scen2,upper_survdiff_msm_swLtrunc_scen2,
                    upper_survdiff_msm_sw_scen3,upper_survdiff_msm_swL_scen3,upper_survdiff_msm_swtrunc_scen3,upper_survdiff_msm_swLtrunc_scen3,
                    bias_survdiff_seq_sw_scen1,bias_survdiff_seq_swtrunc_scen1,
                    bias_survdiff_seq_sw_scen2,bias_survdiff_seq_swtrunc_scen2,
                    bias_survdiff_seq_sw_scen3,bias_survdiff_seq_swtrunc_scen3,
                    lower_survdiff_seq_sw_scen1,lower_survdiff_seq_swtrunc_scen1,
                    lower_survdiff_seq_sw_scen2,lower_survdiff_seq_swtrunc_scen2,
                    lower_survdiff_seq_sw_scen3,lower_survdiff_seq_swtrunc_scen3,
                    upper_survdiff_seq_sw_scen1,upper_survdiff_seq_swtrunc_scen1,
                    upper_survdiff_seq_sw_scen2,upper_survdiff_seq_swtrunc_scen2,
                    upper_survdiff_seq_sw_scen3,upper_survdiff_seq_swtrunc_scen3)

#------------------------
#some functions used to create the plots
#------------------------

#function used to add a line to a plot
addlinetoplot.lty <- function(dataset, varx, vary,vcol) { 
  list(
    geom_line(data=dataset, aes_string(x=varx, y=vary,colour=vcol),size=1.5) 
  )
}
cols=c("black"="#000000","grey"="#999999", "orange"="#E69F00", "lblue"="#56B4E9", 
       "green"="#009E73", "dblue"="#0072B2", "red"="#D55E00", "pink"="#CC79A7")

#function used to add a ribbon to a plot
addribbontoplot <- function(dataset, varymin, varymax,vcolour,valpha) { 
  list(
    geom_ribbon(data=dataset, aes_string(ymin=varymin, ymax=varymax),fill=vcolour,alpha=valpha)
  )
}

#------------------------
#plots: MSM-IPTW scenario 1
#------------------------

#---
#msm_sw_scen1

biasplot.msm.sw.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_sw_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_sw_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_sw_scen1",varymax="upper_survdiff_msm_sw_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swL_scen1

biasplot.msm.swL.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swL_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swL_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swL_scen1",varymax="upper_survdiff_msm_swL_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swtrunc_scen1

biasplot.msm.swtrunc.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swtrunc_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swtrunc_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swtrunc_scen1",varymax="upper_survdiff_msm_swtrunc_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 1")

#---
#msm_swLtrunc_scen1

biasplot.msm.swLtrunc.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swLtrunc_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swLtrunc_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swLtrunc_scen1",varymax="upper_survdiff_msm_swLtrunc_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 1")

#------------------------
#plots: MSM-IPTW scenario 2
#------------------------

#---
#msm_sw_scen2

biasplot.msm.sw.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_sw_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_sw_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_sw_scen2",varymax="upper_survdiff_msm_sw_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swL_scen2

biasplot.msm.swL.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swL_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swL_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swL_scen2",varymax="upper_survdiff_msm_swL_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swtrunc_scen2

biasplot.msm.swtrunc.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swtrunc_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swtrunc_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swtrunc_scen2",varymax="upper_survdiff_msm_swtrunc_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 2")

#---
#msm_swLtrunc_scen2

biasplot.msm.swLtrunc.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swLtrunc_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swLtrunc_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swLtrunc_scen2",varymax="upper_survdiff_msm_swLtrunc_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 2")

#------------------------
#plots: MSM-IPTW scenario 3
#------------------------

#---
#msm_sw_scen3

biasplot.msm.sw.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_sw_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_sw_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_sw_scen3",varymax="upper_survdiff_msm_sw_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swL_scen3

biasplot.msm.swL.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swL_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swL_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swL_scen3",varymax="upper_survdiff_msm_swL_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swtrunc_scen3

biasplot.msm.swtrunc.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swtrunc_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swtrunc_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swtrunc_scen3",varymax="upper_survdiff_msm_swtrunc_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 3")

#---
#msm_swLtrunc_scen3

biasplot.msm.swLtrunc.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_msm_swLtrunc_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_msm_swLtrunc_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_msm_swLtrunc_scen3",varymax="upper_survdiff_msm_swLtrunc_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("MSM-IPTW: scenario 3")

#------------------------
#plots: Sequential trials scenario 1
#------------------------

#---
#seq_sw_scen1

biasplot.seq.sw.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_sw_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_sw_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_sw_scen1",varymax="upper_survdiff_seq_sw_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 1")

#---
#seq_swtrunc_scen1

biasplot.seq.swtrunc.scen1=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_swtrunc_scen1))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_swtrunc_scen1",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_swtrunc_scen1",varymax="upper_survdiff_seq_swtrunc_scen1",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 1")

#------------------------
#plots: Sequential trials scenario 2
#------------------------

#---
#seq_sw_scen2

biasplot.seq.sw.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_sw_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_sw_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_sw_scen2",varymax="upper_survdiff_seq_sw_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 2")

#---
#seq_swtrunc_scen2

biasplot.seq.swtrunc.scen2=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_swtrunc_scen2))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_swtrunc_scen2",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_swtrunc_scen2",varymax="upper_survdiff_seq_swtrunc_scen2",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 2")

#------------------------
#plots: Sequential trials scenario 3
#------------------------

#---
#seq_sw_scen3

biasplot.seq.sw.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_sw_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_sw_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_sw_scen3",varymax="upper_survdiff_seq_sw_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 3")

#---
#seq_swtrunc_scen3

biasplot.seq.swtrunc.scen3=ggplot(dat.bias,aes(x=t.hor,y=bias_survdiff_seq_swtrunc_scen3))+theme_bw()+
  ylab("Bias in risk difference")+xlab("Time")+
  addlinetoplot.lty(dat.bias,"t.hor","bias_survdiff_seq_swtrunc_scen3",vcol='"black"')+
  addribbontoplot(dat.bias,varymin="lower_survdiff_seq_swtrunc_scen3",varymax="upper_survdiff_seq_swtrunc_scen3",vcolour="black", valpha=0.2)+
  scale_x_continuous(breaks=seq(0,5,1),limits=c(0,5))+
  scale_y_continuous(breaks=seq(-0.03,0.03,0.01),limits=c(-0.035,0.035))+
  scale_colour_manual(values=cols)+
  geom_hline(yintercept=0,linetype="dashed", color="red",size=1.5)+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
        legend.position="none",
        axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle("Sequential trials: scenario 3")

#------------------------------------------------------------
#Combine plots for scenarios 1,2,3 to create Figure 5, Supplementary Figure 1, Supplementary Figure 3.
#------------------------------------------------------------

figure5=arrangeGrob(biasplot.msm.swL.scen1,biasplot.seq.sw.scen1,
                    biasplot.msm.swL.scen2,biasplot.seq.sw.scen2,
                    biasplot.msm.swL.scen3,biasplot.seq.sw.scen3,ncol=2)
ggsave("results/figure5.pdf", figure5)

suppfigure1=arrangeGrob(biasplot.msm.sw.scen1,biasplot.seq.sw.scen1,
             biasplot.msm.sw.scen2,biasplot.seq.sw.scen2,
             biasplot.msm.sw.scen3,biasplot.seq.sw.scen3,ncol=2)
ggsave("results/suppfigure1.pdf", suppfigure1)

suppfigure3=arrangeGrob(biasplot.msm.swLtrunc.scen1,biasplot.seq.swtrunc.scen1,
             biasplot.msm.swLtrunc.scen2,biasplot.seq.swtrunc.scen2,
             biasplot.msm.swLtrunc.scen3,biasplot.seq.swtrunc.scen3,ncol=2)
ggsave("results/suppfigure3.pdf", suppfigure3)

